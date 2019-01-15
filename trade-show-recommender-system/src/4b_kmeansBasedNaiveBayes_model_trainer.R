
## THIS IS THE SCRIPT WHERE THE NAIVE BAYES MODEL IS TRAINED FOR THE SELECTED DATA SUBSET IN THE PRECEDING predict_by_probability
## THIS WILL GENERATE PROBABILITY SCORES USING THE NAIVE BAYES ALGORITHM
## THE TOP10 MOST PROBABLE ITEM IS THEN SAVED

rm(list = ls())
library(e1071)
library(data.table)
set.seed(12345)

## Part 1 - Data Preparation ##
newSourceTrain <- readRDS("./RData/newSourceTrainNBModel.RData") ## this is an in-memory data which lists the clusters where prior users belong 
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
userItemData <- userItemData[userItemData$rowsum > 5, -c("rowsum")]  ## retain those who have visited atleast 5 exhibitor

## drop the columns that dont have entries
cols <- colSums(userItemData[,-c("company", "predTrain")])
names <- names(userItemData[,-c("company", "predTrain")])
ind <- which(cols > 0)
names <- names[ind]
userItemData <- userItemData[, c(names,"company","predTrain"), with = FALSE]
userItemData <- droplevels(userItemData) ## drop the unused levels
names <- names(userItemData[, -c("company", "predTrain")])

userItemData[,c(1:549)] <- lapply(userItemData[, c(1:549)], function(x) as.logical(x))
userItemData$predTrain <- as.factor(userItemData$predTrain)

## Part 2 - Model Training ##
varNames <- names(userItemData[, -c("company", "predTrain")])
saveRDS(varNames, "./RData/varNames.RData")

naiveModel <- naiveBayes(company ~., data = userItemData)
saveRDS(naiveModel,"./output/naiveModel.rds")


