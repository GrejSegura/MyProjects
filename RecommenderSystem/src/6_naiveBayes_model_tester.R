


rm(list = ls())
library(flexclust)
library(e1071)
library(data.table)
library(randomForest)
library(tidyverse)


## Part 1 - Predict Cluster and add as new feature ##
sourceTest <- readRDS("./RData/test.RData")
kmodel <- readRDS("./output/kmodel.rds")

predTest <- predict(kmodel, sourceTest[, -1])
newSourceTest <- cbind(sourceTest, predTest)   ## new source data - will be the reference for the similarity on predictions ##


## Part 2 - Data Process for Naive Bayes Testing ##
userItemData <- readRDS("./RData/userItemData.RData")
varNames <- readRDS("./RData/varNames.RData")
naiveModel <- readRDS("./output/naiveModel.rds")
newSourceTest <- newSourceTest  ## just to remind me that we need this data set in this part

existingID <- as.data.frame(unique(sourceTest$barcode))
names(existingID)[1] <- "barcode"
userItemData <- merge(userItemData, existingID)


userItemData <- userItemData[, seq := c(1:.N), by = "barcode"] 
userItemDB <- userItemData[seq == 1, -c("seq")]  
userItemTest <- userItemData[seq > 1, -c("seq")]  ## remove the first occurence - those which only have 1 visit will also be removed.

userItemDB <- merge(userItemDB, userItemTest, by = "barcode") ## -------> will be used later to check the recommendations

userItemTest <- dcast(userItemTest, barcode  ~ companyName)
userItemTest <- merge(userItemTest, newSourceTest[,c("barcode", "predTest")], by = "barcode", all = TRUE)
userItemTest <- userItemTest[which(!is.na(userItemTest[,2])),]
userItemTest[,c(2:554)] <- lapply(userItemTest[,c(2:554)], function(x) as.logical(x))
userItemTest$predTest <- as.factor(as.character(userItemData$predTest))


## check the variable names - should be matched with the trained naive bayes model ##
varnames1 <- as.data.frame(varNames)
names(varnames1)[1] <- "company"
varnames2 <- as.data.frame(names(userItemTest[, -c("barcode", "predTest")]))
names(varnames2)[1] <- "company"



varNames3 <- merge(varnames1, varnames2, by = "company")
varNames3 <- as.vector(varNames3[,1])

notVarnames <- !(varNames %in% varNames3)
var <- varNames[notVarnames]

userItemTest <- userItemTest[, c(varNames3,"predTest"), with = FALSE]
userItemTest[, (var) := 0,]


## Part 3 - Testing ##

## sample ##
sample <- sample(1:nrow(userItemTest), 1)

pred <- predict(naiveModel, userItemTest[4,], type = "raw")
pred <- as.data.frame(pred)
max(pred)
recommended <- as.data.frame(sort(colSums(pred), decreasing = TRUE))
recommended$company <- row.names(recommended)
row.names(recommended) <- c(1:nrow(recommended))
top10 <- recommended[1:10, c(2,1)]
names(top10)[c(1,2)] <- c("exhibitor", "probability")
top10
