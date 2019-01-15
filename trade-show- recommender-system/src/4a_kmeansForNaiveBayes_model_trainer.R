

## THIS IS THE KMEANS CLUSTERING PART OF THE NAIVE BAYES MODEL
## THIS IS DIFFERENT FROM THE KMEANS WITH K = 7 AS THIS WILL BE USED AS AN ADDITIONAL FEATURE FOR THE NAIVE BAYES MODEL LATER


rm(list = ls())
library(flexclust)
library(data.table)
set.seed(12345)

sourceTrain <- readRDS("./RData/train.RData")


## train the model ##
kmodel <- kcca(sourceTrain[, -1], 50, kccaFamily("jaccard"))

saveRDS(kmodel,"./output/kmodelforNB.rds")

## predict the clusters per user ##
predTrain <- predict(kmodel, sourceTrain[, -1])
newSourceTrain <- cbind(sourceTrain, predTrain)   ## new source data - will be the reference for the similarity on predictions ##

saveRDS(newSourceTrain, file = "./RData/newSourceTrainNBModel.RData")

