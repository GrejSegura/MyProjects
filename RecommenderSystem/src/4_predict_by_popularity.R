

## THIS IS THE SCRIPT WHICH WILL GIVE A RECOMMENDATION TO THE ACTIVE USER ##
## THIS SCRIPT IS USED IF THERE IS NO PRIOR ITEM SELECTION WAS OBSERVED FOR THE ACTIVE USER ##
## THE RECOMMENDATION IS BASED ON THE POPULARITY OF ITEMS BASED ON THE CLUSTER (GROUP) WHERE THE ACTIVE USER BELONGS ##
## THERE ARE 31 IDENTIFIED CLUSTERS BASED ON THE MODEL TRAINING ##

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

## identify the cluster ##
predTest <- predict(kmodel, test[, -1])
test <- cbind(test, predTrain = predTest)  ## append the predicted cluster to the active user's data

source <- newSourceTrain[newSourceTrain$predTrain == predTest, 1]

userItemData <- merge(userItemData, source, by = "barcode")
listItemFrequency <- userItemData %>% group_by(companyName) %>% summarise(count = n())
listItemFrequency <- listItemFrequency[order(listItemFrequency$count, decreasing = TRUE),]

## List the top 10 Recommendations ##
top10 <- listItemFrequency[1:10, ]
saveRDS(top10, "./output/top10.RData")