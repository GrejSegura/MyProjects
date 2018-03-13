

## THIS IS A TRAINER FOR THE RECOMMENDER SYSTEM
## IT USES K-MEANS TO FIND THE SIMILARITY BETWEEN THE USERS
## THE DATA UTILIZES THE DEMOGRAPHIC AS AN ADDITIONAL INFORMATION FOR THE MACHINE LEARNING ALGORITHM


rm(list = ls())
library(tidyverse)
library(data.table)
library(flexclust)
set.seed(32134)

sourceTrain <- readRDS("./RData/train.RData")

## train the model ##
kmodel <- kcca(sourceTrain[, -1], 100, kccaFamily("kmeans"))

saveRDS(kmodel,"./output/kmodel.rds")

## predict the clusters per user ##
predTrain <- predict(kmodel, sourceTrain[, -1])
newSourceTrain <- cbind(sourceTrain, predTrain)   ## new source data - will be the reference for the similarity on predictions ##

saveRDS(newSourceTrain, file = "./RData/newSourceTrain.RData")

