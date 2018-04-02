

rm(list = ls())
library(flexclust)
library(e1071)
library(data.table)
library(randomForest)
library(tidyverse)
library(caret)
set.seed(12345)

## Part 1 - Predict Cluster and add as new feature ##
sourceTest <- readRDS("./RData/test.RData")
kmodel <- readRDS("./output/kmodelforNB.rds")

predTest <- predict(kmodel, sourceTest[, -1])
newSourceTest <- as.data.frame(cbind(sourceTest, as.factor(predTest)) )  ## new source data - will be the reference for the similarity on predictions ##

## Part 2 - Data Process for Naive Bayes Testing ##
userItemData <- readRDS("./RData/userItemData.RData")
varNames <- readRDS("./RData/varNames.RData")
naiveModel <- readRDS("./output/naiveModel.rds")

newSourceTest <- newSourceTest  ## just to remind me that we need this data set in this part

existingID <- as.data.frame(unique(newSourceTest$barcode))
names(existingID)[1] <- "barcode"
userItemTest <- merge(userItemData, existingID)

userItemDB <- unique(userItemTest[, c(1,2)])  ## these are the companies to be predicted
userItemTest <- dcast(userItemTest, barcode  ~ companyName)
n <- as.data.frame(newSourceTest[,c("barcode")])
names(n)[1] <- "barcode"
userItemTest <- merge(userItemTest, n, by = "barcode", all = TRUE)
userItemTest <- userItemTest[which(!is.na(userItemTest[,2])),]
userItemTest[,c(2:562)] <- lapply(userItemTest[,c(2:562)], function(x) as.logical(x))
barcodeTest <- userItemTest[, c("barcode")]
userItemTest <- merge(userItemTest, newSourceTest, by = "barcode", all.y = TRUE)  ## added the demographic data
userItemTest <- userItemTest[!is.na(userItemTest$SaudiExportDevelopmentAuthority),]

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
userItemTest[,(var) := "FALSE",]

userItemTest <- userItemTest[, c(544, 1:543, 545:549)]
userItemTest[,c(2:549)] <- lapply(userItemTest[, c(2:549)], function(x) as.logical(x))
userItemTest <- as.data.frame(cbind(userItemTest, predTest))
names(userItemTest)[550] <- "predTrain"

userItem <- userItemTest[, -550]
userItem[] <- lapply(userItem[], function(x) ifelse(x == "TRUE", 1, 0))
userItem$rowsum <- apply(userItem, 1, sum)
index <- which(userItem$rowsum > 5)  ## retain those who have visited atleast 5 exhibitor

barcodes <- userItemTest[index, c("barcode")]
userItemTest <- userItemTest[index, ]

varNames2 <- names(userItemTest)
not <- which(!(varNames2 %in% varNames))

barcodes <- userItemTest[, c("barcode")]
userItemTest <- userItemTest[, -1]

pred <- predict(naiveModel, userItemTest, type = "raw")
pred <- as.data.frame(pred)
pred <- setDT(pred)
pred$barcode <- barcodes

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
	topN <- setDT(as.data.frame(topN[1:10, 2]))   ## choose only the top 10 recommendations
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
# @ k = 70 --- 0.6550201
# @ k = 60 --- 0.6551539
# @ k = 50 --- 0.6559572
# @ k = 40 --- 0.6539491
# @ k = 30 --- 0.6542169
# @ k = 20 --- 0.6440428

recall <- mean(accuracyData$recall)
recall
# @ k = 70 --- 0.721245
# @ k = 60 --- 0.7214123
# @ k = 50 --- 0.7227
# @ k = 40 --- 0.7203867
# @ k = 30 --- 0.7208568
# @ k = 20 --- 0.7082171

