


rm(list = ls())
library(e1071)
library(data.table)
library(tidyverse)
set.seed(12345)

## Part 1 - Predict Cluster and add as new feature ##
newSourceTest <- readRDS("./RData/test.RData")


## Part 2 - Data Process for Naive Bayes Testing ##
userItemData <- readRDS("./RData/userItemData.RData")
varNames <- readRDS("./RData/varNamesNoCluster.RData")
naiveModel <- readRDS("./output/naiveModelNoCluster.rds")

newSourceTest <- newSourceTest  ## just to remind me that we need this data set in this part

existingID <- as.data.frame(unique(newSourceTest$barcode))
names(existingID)[1] <- "barcode"
userItemTest <- merge(userItemData, existingID)

userItemDB <- unique(userItemTest[, c(1,2)])  ## these are the companies to be predicted
userItemTest <- dcast(userItemTest, barcode  ~ companyName)
userItemTest <- merge(userItemTest, newSourceTest[,c("barcode")], by = "barcode", all = TRUE)
userItemTest$rowsum <- apply(userItemTest[,-c("barcode")], 1, sum)
userItemTest <- userItemTest[userItemTest$rowsum > 5, -c("rowsum")]  ## retain those who have visited atleast 5 exhibitor
userItemTest <- userItemTest[which(!is.na(userItemTest[,2])),]

barcodes <- userItemTest[, c("barcode")]
userItemTest <- merge(userItemTest, newSourceTest, by = "barcode", all.y = TRUE)  ## added the demographic data
userItemTest <- userItemTest[!is.na(userItemTest$SaudiExportDevelopmentAuthority),]

## check the variable names - should be matched with the trained naive bayes model ##
varnames1 <- as.data.frame(varNames[-1])
names(varnames1)[1] <- "company"
varnames2 <- as.data.frame(names(userItemTest[, -c("barcode")]))
names(varnames2)[1] <- "company"
varNames3 <- merge(varnames1, varnames2, by = "company")
varNames3 <- as.vector(varNames3[,1])

notVarnames <- !(varNames %in% varNames3)
var <- varNames[notVarnames]
var <- var[-1]
userItemTest <- userItemTest[, c(varNames3, "barcode"), with = FALSE]
userItemTest[,(var) := 0,]
userItemTest <- userItemTest[, c(619, 1:618, 620:624)]
userItemTest[,c(2:624)] <- lapply(userItemTest[, c(2:624)], function(x) as.logical(x)) #####

varNames2 <- names(userItemTest)
not <- which(!(varNames2 %in% varNames))
not

## Part 3 - Predict ##
userItemTest1 <- userItemTest[-754, ]    ## remove #754 barcode == 04F91A6A865280 -- it generates error in predict (reason still unknown)
userItemTest1 <- userItemTest1[, -c("barcode")]

pred <- predict(naiveModel, userItemTest1, type = "raw")
pred <- as.data.frame(pred)
pred <- setDT(pred)
pred$barcode <- barcodes[-754]

## calculate the precision - recall ##
## check the top k = 10
accuracyData <- data.frame(barcode = vector(), precision = numeric(), recall = numeric())
for (i in 1:nrow(pred)) {
	
	userNumber <- pred$barcode[i]  ##
	userDB <- userItemDB[userItemDB$barcode == userNumber, ]
	userDB <- userDB[, 2]
	names(userDB)[1] <- "company"
	
	cols <- names(pred[, -c("barcode")])
	topN <- as.data.frame(t(pred[i, -c("barcode")]))   ##
	names(topN)[1] <- "probability"
	topN$company <- cols
	row.names(topN) <- c(1:nrow(topN))
	topN <- topN[order(topN$probability, decreasing = TRUE),]
	topN <- setDT(as.data.frame(topN[1:10, 2]))  ## choose only the top 10 recommendations
	names(topN) <- "company"
	
	recall1 <- userDB$company %in% topN$company
	recall <- length(recall1[recall1 == "TRUE"])/length(userDB$company)
	
	precision1 <- topN$company %in% userDB$company
	precision <- length(precision1[precision1 == "TRUE"])/length(topN$company)
	
	collectTable <- cbind(userNumber, precision, recall)
	accuracyData <- rbind(accuracyData, collectTable)
}

accuracyData[, c(2:3)] <- lapply(accuracyData[, c(2:3)], function(x) as.numeric(as.character(unlist(x))))

precision <- mean(accuracyData$precision)
precision
## 0.5862565

recall <- mean(accuracyData$recall)
recall
## 0.6493635
