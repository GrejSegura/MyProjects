
rm(list = ls())
library(data.table)
library(tidyverse)
set.seed(12345)

userItemData <- readRDS("./RData/userItemData.RData")
newSourceTest <- readRDS("./RData/test.RData")

existingID <- as.data.frame(unique(newSourceTest$barcode))  
names(existingID)[1] <- "barcode"
userItemTest <- merge(userItemData, existingID)  ## use only the barcodes that exists in the useritemData

userItemDB <- unique(userItemTest[, c(1,2)])  ## these are the companies to be predicted

userItemTest <- dcast(userItemTest, barcode  ~ companyName)
userItemTest <- merge(userItemTest, newSourceTest[,c("barcode")], by = "barcode", all = TRUE)

userItemTest$rowsum <- apply(userItemTest[,-c("barcode")], 1, sum)
userItemTest <- userItemTest[userItemTest$rowsum > 5, -c("rowsum")] 
barcodes <- userItemTest[, c("barcode")]

accuracyData <- data.frame(barcode = vector(), precision = numeric(), recall = numeric())
for (i in 1:nrow(barcodes)){
	
	userNumber <- barcodes[i,1]  ## get the barcode
	userDB <- userItemDB[userItemDB$barcode == as.character(userNumber),]  ## get the data for the checked barcodes in useritemDB -- those we try to predict
	names(userDB)[2] <- "company"
	
	topN <- userItemData %>% group_by(companyName) %>% summarise(count = n())
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
#0.1229659

recall <- mean(accuracyData$recall)
recall
#0.1289438