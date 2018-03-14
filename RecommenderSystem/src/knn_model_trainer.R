
## THIS IS THE MODEL TRAINER FOR THE RECOMMENDER SYSTEM
## IT USES KNN AS THE SIMILARITY ALGORITHM
## THE DATA UTILIZES THE DEMOGRAPHIC AS AN ADDITIONAL INFORMATION FOR THE MACHINE LEARNING ALGORITHM

rm(list = ls())
library(tidyverse)
library(data.table)
library(class)
set.seed(32134)

userItemData <- readRDS("./RData/userItemData.RData")
sourceTest <- readRDS("./RData/test.RData")
sourceTrain <- readRDS("./RData/train.RData")

sample <- sample(1:nrow(sourceTest), 1)
test <- sourceTest[sample, ]


knnmodel <- knn(train = sourceTrain, test = test, cl = target_train #this is the classifying variable
		, k = round(sqrt(nrow(data3))))
