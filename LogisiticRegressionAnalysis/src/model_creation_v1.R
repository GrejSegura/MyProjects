
rm(list = ls())
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(magrittr)
library(DT)
set.seed(123)


data_1 <- fread("./dta/gate_scans.csv")

names(data_1)[1] <- "Card.Number"
data_1 <- setDT(data_1)
data_1 <- data_1[, Len := nchar(`Card.Number`)]
data_1 <- data_1[Len == 10,] ## remove Length of card.number != 10
data_1 <- data_1[ ,-c("Len")] ## remove Len

ind_char <- which((grepl("[^0-9]", data_1$Card.Number))) ## identify the non-digits characters

data_1 <- data_1[-ind_char, ] ## remove card.numbers with non-digits character

data_1$Card.Number <- as.character(data_1$Card.Number)
data_1$Show <- as.character(data_1$Show)

data_1 <- data_1[order(data_1$Card.Number, data_1$Date, data_1$Time),] ## sort by card.number, date, time

data_1 <- data_1[, seq_id := c(1:.N), by = .(Card.Number, Show)]
data_1 <- setDT(data_1)


data_1 <- data_1[seq_id == 1, ] ## REMOVE THE DUPLICATE HALL VISITS
data_1 <- data_1[, -c("seq_id")]

data_1$Date <- dmy(data_1$Date) ## date formatting
data_1$day <- wday(data_1$Date) ## generates 2,3,4 which means 2 = monday, 3 = tue, 3 = wed


data_1$Show <- as.character(data_1$Show)

data_1 <- data_1 %>% group_by(Card.Number) %>% mutate(seq_id = c(1:n())) ## create a sequence for visits

data_1 <- setDT(data_1)
k <- length(names(data_1)) + 1  ## used as index to create the variable 'flow'

## CREATE VARIABLES BY LOOPING 

varnames <- paste("V", 1:max(data_1$seq_id), sep = "")
n <- 1:max(data_1$seq_id)

for (i in n){
	
	data_1[seq_id >= i, varnames[i] := lag(Show, max(seq_id) - i), by = .(Card.Number)]
	data_1[seq_id <= i, varnames[i] := "", by = .(Card.Number)]
	data_1[seq_id == i, varnames[i] := "", by = .(Card.Number)]
}


## IDENTIFY THE MAXIMUM NUMBER OF VISITS PER ID
data_1 <- data_1 %>% group_by(Card.Number) %>% mutate(flag_max = ifelse(seq_id == max(seq_id), 1, 0))
data_1 <- setDT(data_1)
data_1 <- data_1[flag_max == 1, -c("flag_max")]
data_1 <- data_1[, -c(2,3,4,6,7,8)]
data_1 <- melt(data_1, measure.vars = names(data_1)[3:length(data_1)], variable.name = "label", value.name = "visited_show")

data_1 <- data_1[label == "V1" | visited_show != "", ]
data_1 <- data_1[, -3]
data_1$visited_show[data_1$visited_show == ""] <- "None"
names(data_1)[1] <- "id"
str(data_1)

fwrite(data_1, "./dta/cleanData1.csv", row.names = FALSE)


'## LOAD REGION DATA
regionData <- fread("./dta/Regional Grouping.csv", sep = ",", na.strings = c(" ", ""))  ### data for regional groups
names(regionData)[1] <- "country"
#regionData[] <- lapply(regionData, function(x) tolower(x))
'
## LOAD SOURCE DATA FROM GITEX DU 2017 FAS
fasData <- fread("./dta/Source Data.csv", sep = ",", na.strings = c(" ", ""))  ### data for attended

## TRANSFORM DATA FROM WIDE TO LONG
fasData <- melt(fasData, measure.vars = names(fasData)[3:length(fasData)], variable.name = "attribute", value.name = "value")

## separate "value" into columns by "]"
names <- paste0("v", 1:39)
fasData[, c(names) := tstrsplit(value, "]", fixed = TRUE)]
fasData <- fasData[, -4]

fasData <- melt(fasData, measure.vars = names, variable.name = "attribute", value.name = "value")
fasData <- fasData[, -4]
fasData <- fasData[value != "NA", ]
names(fasData) <- c("id", "country", "attribute", "value")



fwrite(fasData, "./dta/fasData.csv", row.names = FALSE)


## LOAD THE SOURCE CODES DATA
codeData <- fread("./dta/source code.csv", sep = ",", na.strings = c(" ", ""))  ### data for regional groups
names(codeData)[2] <- "value"

fasData <- fread("./dta/fasData.csv")
fasData <- merge(fasData, codeData, by = "value", all.x = TRUE)
'fasData <- merge(fasData, regionData, by = "country", all.x = TRUE)'
fasData$country <- ifelse(fasData$country == "United Arab Emirates", "Local", "International")
fasData <- fasData[, -c(1,5)]
names(fasData)[c(2,4)] <- c("region", "value")

fasData <- dcast(fasData, id + region ~ value)
fasData$region <- ifelse(fasData$region == "Local", 1, 0)

fasData$id <- as.character(fasData$id)
fasData <- merge(fasData, data_1, by = "id", all = TRUE)
fasData <- fasData[!(is.na(Show)), ]
fasData <- fasData[!(Show == "CONFERENCE"), ]
fasData <- fasData[!(Show == "MAJLIS Lounge"), ]
fasData <- fasData[!(Show == "Connexions Lounge"), ]
fwrite(fasData, "./dta/cleanData2.csv", row.names = FALSE)


## LOAD SHOW DATA

showData <- fread("./dta/showData.csv")
fasData <- fread("./dta/cleanData2.csv")
fasData <- merge(fasData, showData, by = "Show", all.x = TRUE)

names(showData)[c(1,2)] <- c("visited_show", "visited")

fasData <- merge(fasData, showData, by = "visited_show", all.x = TRUE)
fasData <- fasData[!(is.na(region)), ]
fasData <- fasData[region != "", ]
fasData <- fasData[, -c(1,2,3)]
fwrite(fasData, "./dta/cleanData2.csv", row.names = FALSE)


#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

#### TRAINING THE DATA ####
rm(list = ls())
gc()
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(magrittr)
library(DT)
library(xgboost)
library(caret)
set.seed(4321)

dta <- fread("./dta/cleanData2.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- (dta$label)-1

'd <- 1:nrow(dta)
index <- sample(d, round(nrow(dta)*.75))

train_dta <- dta[index, ]
test_dta <- dta[-index, ]


train_1 <- train_dta[,-c("label")]
test_1 <- test_dta[,-c("label")]

train_2 <- train_dta[, c("label")]
test_2 <- test_dta[, c("label")]
'
dta[] <- lapply(dta, function(x) as.numeric(x))

train_dta <- dta[,-c("label")]
train_label <- dta[, c("label")]

str(dta)

xgModel <- xgboost(as.matrix(train_dta), as.matrix(train_label), 
		   booster = 'gbtree',
		   objective = 'multi:softprob',
		   num_class = 13,
		   max.depth = 8,
		   eta = 0.01,
		   nthread = 8,
		   nrounds = 600,
		   min_child_weight = 1,
		   subsample = .75,
		   colsample_bytree = .8, 
		   num_parallel_tree = 1)

pred_xg <- predict(xgModel, as.matrix(train_1), reshape = TRUE)
pred_xg <- as.data.frame(pred_xg_train)

save(xgModel, file = "./src/xgModel.rda")

fasData <- fread("./dta/cleanData2.csv")
testData <- fasData[1:2, ]
testData <- fwrite(testData, "./dta/testData.csv")

##### PREDICTIONS #####

load(file = "./src/xgModel.rda")
testData <- fread("./dta/testData.csv")
testData[] <- lapply(testData, function(x) as.numeric(x))

pred_xg <- predict(xgModel, as.matrix(testData), reshape = TRUE)
pred_xg <- as.data.frame(pred_xg)

fwrite(pred_xg, "./dta/prediction.csv", row.names = FALSE)


names <- dimnames(train_dta)[[2]]

varImportance <- xgb.importance(names, model = xgModel)

fwrite(varImportance, file = "./dta/varImportance.csv")

xgb.plot.importance(varImportance[1:50,])




#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####
#### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

'#### TRAINING THE DATA ####
rm(list = ls())
gc()
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(magrittr)
library(DT)
library(xgboost)
library(caret)
library(e1071)
set.seed(43211)

dta <- fread("./dta/fasData.csv")
dta <- dta[!(is.na(dta$label)),]


d <- 1:nrow(dta)
index <- sample(d, round(nrow(dta)*.75))

train_dta <- dta[index, ]
test_dta <- dta[-index, ]

naiveModel <- naiveBayes(label ~., train_dta)
'