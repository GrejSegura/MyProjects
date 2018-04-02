

## THIS IS A TRAINER FOR THE RECOMMENDER SYSTEM
## IT USES K-MEANS TO FIND THE SIMILARITY BETWEEN THE USERS
## THE DATA UTILIZES THE DEMOGRAPHIC AS AN ADDITIONAL INFORMATION FOR THE MACHINE LEARNING ALGORITHM


rm(list = ls())
library(flexclust)
library(data.table)
set.seed(12345)

sourceTrain <- readRDS("./RData/train.RData")


## train the model ##
kmodel <- kcca(sourceTrain[, -1], 8, kccaFamily("jaccard"))

saveRDS(kmodel,"./output/kmodel.rds")

## predict the clusters per user ##
predTrain <- predict(kmodel, sourceTrain[, -1])
newSourceTrain <- cbind(sourceTrain, predTrain)   ## new source data - will be the reference for the similarity on predictions ##

saveRDS(newSourceTrain, file = "./RData/newSourceTrain.RData")

