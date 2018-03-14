

## THIS IS THE SCRIPT WHICH WILL GIVE A RECOMMENDATION TO THE ACTIVE USER ##
## THIS SCRIPT IS USED WHEN THERE IS A PRIOR ITEM TRANSACTION OBSERVED FOR THE ACTIVE USER ##
## PROBABILITY SCORES WERE CALCULATED USING NAIVE BAYES ALGORITHM AND RANKED TO GENERATE THE RECOMMENDATIONS ##

rm(list = ls())
library(flexclust)
library(tidyverse)
library(data.table)


## Part 1  -- subset the data by finding out what cluster the active user belongs ##
userItemData <- readRDS("./RData/userItemData.RData") ## the database for the user-item matrix ##
activeUserDemog <- readRDS("./RData/test.RData")  ## this is the data for the active user -- all demographics ##
newSourceTrain <- readRDS("./RData/newSourceTrain.RData") ## this is an in-memory data which lists the clusters where prior users belong ##
kmodel <- readRDS("./output/kmodel.rds") ## the pre-trained model ##

## this is just to take 1 random sample if there are multiple active users listed -- in reality, there should only be 1 sample listed every prediction process ##
sample <- sample(1:nrow(activeUserDemog), 1)  
test <- activeUserDemog[sample, ]
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## this will identify the cluster for the given user ##
predTest <- predict(kmodel, test[, -1])
test <- cbind(test, predTrain = predTest)  ## append the predicted cluster to the 

## subset the data based on the predicted cluster ##
source <- newSourceTrain[newSourceTrain$predTrain == predTest, 1]
userItemData <- merge(userItemData, source, by = "barcode")

saveRDS(userItemData, "./userItemData.RData")


## Part 2 - summon the naiveBayes model ##
source("./src/6_naiveBayes_model_trainer.R")

