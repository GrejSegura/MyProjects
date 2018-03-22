
## THIS IS THE SCRIPT WHERE THE NAIVE BAYES MODEL IS TRAINED FOR THE SELECTED DATA SUBSET IN THE PRECEDING predict_by_probability
## THIS WILL GENERATE PROBABILITY SCORES USING THE NAIVE BAYES ALGORITHM
## THE TOP10 MOST PROBABLE ITEM IS THEN SAVED

rm(list = ls())
library(e1071)
library(data.table)
set.seed(90210)

## Part 1 - Data Preparation ##
userItemData <- readRDS("./RData/userItemData.RData")
userItemData$company <- userItemData$companyName
userItemData <- dcast(userItemData, barcode + company ~ companyName)


## Part 2 - Model Training ##

userItemData <- userItemData[, -c("barcode")]
naiveModel <- naiveBayes(company ~ ., data = userItemData)


## Part 3 - Probability Ranking and Recommendation ##
pred <- predict(naiveModel, userItemData, type = "raw")
pred <- as.data.frame(pred)
recommended <- colSums(pred)
recommended[1:10]
