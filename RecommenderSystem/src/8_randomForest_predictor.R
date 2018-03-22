

## THIS IS THE SCRIPT WHICH WILL GIVE A RECOMMENDATION TO THE ACTIVE USER ##
## THIS SCRIPT IS USED WHEN THERE IS A PRIOR ITEM TRANSACTION OBSERVED FOR THE ACTIVE USER ##
## PROBABILITY SCORES WERE CALCULATED USING RANDOM FOREST ALGORITHM AND RANKED TO GENERATE THE RECOMMENDATIONS ##

rm(list = ls())
library(randomForest)
library(tidyverse)
library(data.table)
set.seed(1)

rfModel <- readRDS("./output/rfModel.rds")
userItemData <- readRDS("./output/userItemData_rfModel.RData")


test <- userItemData[, -c("company")]
## Part 3 - Probability Ranking and Recommendation ##

sample <- sample(1:nrow(test), 1)
t <- as.data.frame(t(test[sample,]))
t$company <- row.names(t)
t <- t[order(-t$V1),]
t <- t[t$V1 > 0 ,]


pred <- predict(rfModel, test[sample,], type = "prob")
pred <- as.data.frame(pred)
max(pred)
recommended <- as.data.frame(sort(colSums(pred), decreasing = TRUE))
recommended$company <- row.names(recommended)
row.names(recommended) <- c(1:nrow(recommended))
recommended <- anti_join(recommended, t)
recommended[1:10,]
t[-1, ]
