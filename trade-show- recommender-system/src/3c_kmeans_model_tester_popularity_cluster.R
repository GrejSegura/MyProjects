
rm(list = ls())
library(flexclust)
library(data.table)
library(tidyverse)
set.seed(12345)

## Part 1 - Predict Cluster and add as new feature ##
userItemData <- readRDS("./RData/userItemData.RData")
newSourceTrain <- readRDS("./RData/newSourceTrain.RData")
sourceTest <- readRDS("./RData/test.RData")
kmodel <- readRDS("./output/kmodel.rds")

## identify the clusters ##
predTest <- predict(kmodel, sourceTest[, -1])
newSourceTest <- cbind(sourceTest, predTest)   ## new source data - will be the reference for the similarity on predictions ##

existingID <- as.data.frame(unique(newSourceTest$barcode))
names(existingID)[1] <- "barcode"
userItemTest <- merge(userItemData, existingID)

userItemDB <- unique(userItemTest[, c(1,2)])  ## these are the companies to be predicted

userItemTest <- dcast(userItemTest, barcode  ~ companyName)
userItemTest <- merge(userItemTest, newSourceTest[,c("barcode")], by = "barcode", all = TRUE)

userItemTest$rowsum <- apply(userItemTest[,-c("barcode")], 1, sum)
userItemTest <- userItemTest[userItemTest$rowsum > 5, -c("rowsum")] 
barcodes <- userItemTest[, c("barcode")]
##

accuracyData <- data.frame(barcode = vector(), precision = numeric(), recall = numeric())
for (i in 1:nrow(barcodes)){
	
	userNumber <- barcodes[i,1]  ## get the barcode
	userCluster <- as.numeric(newSourceTest[barcode == userNumber, c("predTest")]) ## get the cluster number for the barcode
	source <- newSourceTrain[newSourceTrain$predTrain == userCluster, c("barcode")]  ## include only the data which has the cluster group
	
	userDB <- userItemDB[userItemDB$barcode == as.character(userNumber),]  ## get the data for the checked barcodes in useritemDB -- those we try to predict
	names(userDB)[2] <- "company"
	
	topN <- merge(userItemData, source, by = "barcode")    ## use the popularity results in useritemData
	topN <- topN %>% group_by(companyName) %>% summarise(count = n())
	topN <- topN[order(topN$count, decreasing = TRUE),]
	topN <- topN[1:10, ] ## choose only the top 10 recommendations
	names(topN)[1] <- "company"
	
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
#0.1224837

recall <- mean(accuracyData$recall)
recall
#0.1279633