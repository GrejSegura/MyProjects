
## THIS IS THE SCRIPT WHERE THE NAIVE BAYES MODEL IS TRAINED FOR THE SELECTED DATA SUBSET IN THE PRECEDING predict_by_probability
## THIS WILL GENERATE PROBABILITY SCORES USING THE NAIVE BAYES ALGORITHM
## THE TOP10 MOST PROBABLE ITEM IS THEN SAVED

rm(list = ls())
library(e1071)
library(data.table)
library(randomForest)
set.seed(90210)

## Part 1 - Data Preparation ##
newSourceTrain <- readRDS("./RData/newSourceTrain.RData") ## this is an in-memory data which lists the clusters where prior users belong 
userItemData <- readRDS("./RData/userItemData.RData")

company <- userItemData$companyName
company <- as.data.frame(cbind(userItemData$barcode, as.character(company)))
names(company) <- c("barcode","company")
userItemData <- dcast(userItemData, barcode  ~ companyName)

userItemData <- merge(userItemData, company, by = "barcode", all = TRUE)

userItemData <- merge(userItemData, newSourceTrain[,c("barcode", "predTrain")], by = "barcode", all = TRUE)
userItemData <- userItemData[!is.na(userItemData$predTrain),]


userItemData <- userItemData[, -c("barcode")]
userItemData$rowsum <- apply(userItemData[,-c("company", "predTrain")], 1, sum)
userItemData <- userItemData[userItemData$rowsum > 2, -c("rowsum")]  ## retain those who have visited atleast 1 exhibitor

## drop the columns that dont have entries
cols <- colSums(userItemData[,-c("company", "predTrain")])
names <- names(userItemData[,-c("company", "predTrain")])
ind <- which(cols > 0)
names <- names[ind]
userItemData <- userItemData[, c(names,"company","predTrain"), with = FALSE]

userItemData <- droplevels(userItemData) ## drop the unused levels


names <- names(userItemData[, -c("company", "predTrain")])
for(i in names){
	
	userItemData[company == i, (i) := 0]  ## if the column name is equal to the label(company) then set to 0
	
}

userItemData[,c(1:550)] <- lapply(userItemData[, c(1:550)], function(x) as.logical(x))
userItemData$predTrain <- as.factor(userItemData$predTrain)


## Part 2 - Model Training ##
varNames <- names(userItemData[, -c("company", "predTrain")])
saveRDS(varNames, "./RData/varNames.RData")

naiveModel <- naiveBayes(company ~., data = userItemData)

saveRDS(naiveModel,"./output/naiveModel.rds")



test <- userItemData[, -c("company")]


## testing some ##

sample <- sample(1:nrow(test), 1)

t <- as.data.frame(t(test[sample,]))
t$company <- row.names(t)
t <- t[t$V1 == "TRUE" ,]


pred <- predict(naiveModel, test[sample,], type = "raw")
pred <- as.data.frame(pred)
max(pred)
recommended <- as.data.frame(sort(colSums(pred), decreasing = TRUE))
recommended$company <- row.names(recommended)
row.names(recommended) <- c(1:nrow(recommended))
recommended <- anti_join(recommended, t)
top10 <- recommended[1:10, c(2,1)]
names(top10)[c(1,2)] <- c("exhibitor", "probability")
visited <- t[-1, ]
visited <- (visited[, -2, drop = FALSE])
top10
visited
