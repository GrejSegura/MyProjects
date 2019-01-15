
## THIS IS THE SCRIPT WHERE THE NAIVE BAYES MODEL IS TRAINED
## THIS WILL GENERATE PROBABILITY SCORES
## THE TOP10 MOST PROBABLE ITEM PER USER IS THEN RECOMMENDEDED

rm(list = ls())
library(e1071)
library(data.table)
library(tidyverse)
set.seed(12345)

## Part 1 - Data Preparation ##
newSourceTrain <- readRDS("./RData/train.RData") ## this is an in-memory data which lists the clusters where prior users belong 
userItemData <- readRDS("./RData/userItemData.RData")

company <- userItemData$companyName
company <- as.data.frame(cbind(userItemData$barcode, as.character(company)))
names(company) <- c("barcode","company")
userItemData <- dcast(userItemData, barcode  ~ companyName)
userItemData <- merge(userItemData, company, by = "barcode", all = TRUE)
userItemData <- merge(userItemData, newSourceTrain[,c("barcode")], by = "barcode", all.y = TRUE)
userItemData$rowsum <- apply(userItemData[,-c("company", "barcode")], 1, sum)
userItemData <- userItemData[userItemData$rowsum > 5, -c("rowsum")]  ## retain those who have visited atleast 5 exhibitor

## drop the columns that dont have entries
cols <- colSums(userItemData[,-c("company", "barcode")])
names <- names(userItemData[,-c("company", "barcode")])
ind <- which(cols > 0)
names <- names[ind]
userItemData <- userItemData[, c(names,"company", "barcode"), with = FALSE]
userItemData <- droplevels(userItemData) ## drop the unused levels
names <- names(userItemData[, -c("company", "barcode")])
userItemData <- merge(userItemData, newSourceTrain, by = "barcode", all.x = TRUE)
userItemData <- userItemData[, c(1, 551, 2:550, 552:625)]
userItemData[,c(3:625)] <- lapply(userItemData[, c(3:625)], function(x) as.logical(x))  ####

## Part 2 - Model Training ##
varNames <- names(userItemData[, -c("company")])
saveRDS(varNames, "./RData/varNamesNoCluster.RData")

userItemData <- userItemData[,-c("barcode")]
userItemData$type <- "train"
saveRDS(userItemData[, -c("company")], "./RData/useritemTrain.RData") ## to be used as appended data in prediction to prevent error ##

userItemData <- userItemData[, -c("type")]
naiveModel <- naiveBayes(company ~., data = userItemData)
saveRDS(naiveModel,"./output/naiveModelNoCluster.rds")
