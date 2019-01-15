
## DATE : 10/16/2017
## AUTHOR  : GREJELL B. SEGURA

## THIS SCRIPT WILL PREPARE THE DATA FOR DATA ANALYSIS OF BIG 5 2016

## Part 1 ##
rm(list = ls())
library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)
library(caret)
memory.limit(50000)


regionData <- read.csv2("./dta/Regional Grouping.csv", sep = ",", na.strings = c(" ", ""))  ### data for regional groups
names(regionData)[1] <- "Country"
regionData[] <- lapply(regionData, function(x) tolower(x))


#codeData <- read.csv2("./Source Codes.csv", sep = ",", na.strings = c(" ", ""))  ### data for codes


dta_1 <- read.csv2("./dta/BIG5 ATD 2016.csv", sep = ",", na.strings = c(" ", ""))  ### data for attended
dta_1$label <- 1
dta_1 <- dta_1[dta_1$Show.Registered.For == "BIG]",]

dta_2 <- read.csv2("./dta/BIG5 NS 2016.csv", sep = ",", na.strings = c(" ", ""))  ### data for no shows
dta_2$label <- 0
dta_2 <- dta_2[dta_2$Show.Registered.For == "BIG]",]

dta_3 <- rbind(dta_1, dta_2)
dta_3 <- setDT(dta_3) ## SET AS DATA.TABLE
dta_3[] <- lapply(dta_3, function(x) as.character(x))
dta_3[] <- lapply(dta_3, function(x) tolower(x))
dta_3 <- dta_3[, c(138, 1:137)]


names <- grep("Attend", names(dta_3), value = TRUE)
dta_3 <- dta_3[, -as.vector(names), with = FALSE]


## TRANSFORM DATA FROM WIDE TO LONG
dta_3 <- melt(dta_3, measure.vars = names(dta_3)[36:length(dta_3)], variable.name = "attribute", value.name = "value")

## remove unnecessary features
dta_3 <- dta_3[!(is.na(value)),]
dta_3[] <- lapply(dta_3, function(x) as.character(x))
na <- colSums(is.na(dta_3))/nrow(dta_3)
na <- ifelse(na > .9, FALSE, TRUE)
dta_3 <- dta_3[, na, with = FALSE]

## remove special characters in names, position, company, id
names(dta_3)[2] <- "id"
dta_3[, c(3,4,6,7)] <- lapply(dta_3[, c(3,4,6,7), with = FALSE], function(x) gsub("[^A-z]", "", x))
dta_3[, 2] <- lapply(dta_3[, 2,with = FALSE], function(x) gsub("[^0-9]", "", x))
dta_3[, 6] <- lapply(dta_3[, 6,with = FALSE], function(x) gsub("[^A-z0-9]", "", x))
dta_3[] <- lapply(dta_3, function(x) gsub("^$", NA, x)) ## REPLACE BLANKS WITH NA
dta_3 <- dta_3[, -c(3,8,9,10,11,13,14,15,16,17,18,19,20,21,24)]

## separate "value" into columns by "]"
names <- paste0("v", 1:33)
dta_4 <- dta_3[, c(2, 11)]
dta_4[, c(names) := tstrsplit(value, "]", fixed = TRUE)]
dta_4 <- dta_4[, -2]
dta_4 <- melt(dta_4, measure.vars = names, variable.name = "attribute", value.name = "value")
dta_4 <- dta_4[, -c("attribute")]
dta_4 <- dta_4[!(is.na(value)), ]

dta_3 <- dta_3[, -c("attribute","value")]
dta_3 <- unique(dta_3)
dta_3 <- merge(dta_3, dta_4, by = "id", all = TRUE)

dta_3 <- merge(dta_3, regionData, by = "Country", all.x = TRUE)



## format Dates

dta_3[, c("Date", "Time", "am.pm") := tstrsplit(Date.Created, " ", fixed = TRUE)]
dta_3$Date <- dmy(dta_3$Date)

dta_clean <- dta_3[, c(1,2,3,4,5,6,9,10,11,12,13)]
write.csv(dta_clean, "./dta/big5_cleanData.csv", row.names = FALSE)
##############################################################################################################

## Part 2 ##
rm(list = ls()) ## clear the memory

codeData <- read.csv2("./dta/BIG5 CODES 2016.csv", sep = ",", na.strings = c(" ", ""))  ### data for codes
codeData[] <- lapply(codeData, function(x) tolower(x))
codeData[] <- lapply(codeData, function(x) gsub(" ", ".", x))
codeData[] <- lapply(codeData, function(x) gsub("[^A-z0-9]", "", x))

varnames1 <- grepl("dtcm", codeData[, 1])
codeData <- codeData[varnames1 == "FALSE", ]
codeData <- codeData[, -1]
codeData <- codeData[codeData$usage == "demographic", ]
codeData <- codeData[, -3]


names(codeData) <- c("value", "value.1")

dtaMiningRaw <- read.csv2("./dta/big5_cleanData.csv", sep = ",", na.strings = c(" ", ""))  ### data for codes
dtaMining <- setDT(dtaMiningRaw)

# remove unwanted features
dtaMining <- dtaMining[, -c(4,5,6,7,9)]
dtaMining[] <- lapply(dtaMining, function(x) as.character(x))

# convert to characters
dtaMining$Country[is.na(dtaMining$Country)] <- "none"
dtaMining$Region.2[is.na(dtaMining$Region.2)] <- "none"

# format Date and create no.days to show
dtaMining$Date <- ymd(dtaMining$Date)
dtaMining$daysToShow <- ymd("2016-11-21") - dtaMining$Date
dtaMining$daysToShow <- as.numeric(dtaMining$daysToShow)

# remove those who registered on the event period
dtaMining <- dtaMining[daysToShow > 0, ]

# remove Date
dtaMining <- dtaMining[, -6]
dtaMining <- dtaMining[, -1]


# replace codes with identifiable terms
codeData <- unique(codeData)
dtaMining <- merge(dtaMining, codeData, by = "value", all.x = TRUE)

dtaMining2 <- dtaMining

dtaMining <- dtaMining2[, -c("value")]
dtaMining$value <- dtaMining$value.1
dtaMining <- dtaMining[, -c("value.1")]

# convert to wide
dtaMining <- dcast(dtaMining, id + label + Region.2 + daysToShow ~ value)
dtaMining <- dtaMining[, -1]

dummy <- dummyVars(~., dtaMining)
dummy_dta <- predict(dummy, dtaMining)
dtaMining2 <- as.data.frame(dummy_dta)

dtaMining <- dtaMining2


varnames1 <- grep("attd", names(dtaMining), value = TRUE)
#dtaMining <- dtaMining[, -as.vector(varnames1), with = FALSE]
dtaMining <- dtaMining[, -c("NA")]


nms <- names(dtaMining[, -1])
nms <- lapply(nms, function(x) paste0("a",x,"a", collapse = ""))
names(dtaMining)[2:length(dtaMining)] <- as.character(nms)
dtaMining <- dtaMining[, -(length(dtaMining))]

names(dtaMining)[c(11:21)] <- c(paste("a", 11:21, sep = ""))

write.csv(dtaMining, "./dta/dtaminingClean.csv", row.names = FALSE)
