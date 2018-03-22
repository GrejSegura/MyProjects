
## THIS WILL GENERATE PROBABILITY SCORES USING THE RANDOM FORESR ALGORITHM
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


## Part 2 - Model Training ##

userItemData <- userItemData[, -c("barcode")]
userItemData$rowsum <- apply(userItemData[,-c("company", "predTrain")], 1, sum)
userItemData <- userItemData[userItemData$rowsum > 5, -c("rowsum")]

cols <- colSums(userItemData[,-c("company", "predTrain")])
names <- names(userItemData[,-c("company", "predTrain")])
ind <- which(cols > 0)
names <- names[ind]

userItemData <- userItemData[, c(names,"company","predTrain"), with = FALSE]

var <- names(userItemData[, -c("company")])
varname <- paste(var, collapse = '+')
f1 <- as.formula(paste('company', varname, sep = '~')) 
userItemData <- droplevels(userItemData)

saveRDS(userItemData, "./output/userItemData_rfModel.RData")

rfModel <- randomForest(f1, data = userItemData, ntree = 100, mtry = 24)

saveRDS(rfModel, "./output/rfModel.rds")

test <- userItemData[, -c("company")]
## Part 3 - Probability Ranking and Recommendation ##
'
sample <- sample(1:nrow(test), 1)

pred <- predict(rfModel, test[sample,], type = "prob")
pred <- as.data.frame(pred)
max(pred)
recommended <- as.data.frame(sort(colSums(pred), decreasing = TRUE))
recommended$company <- row.names(recommended)
row.names(recommended) <- c(1:nrow(recommended))
recommended[1:10, c(2,1)]
'