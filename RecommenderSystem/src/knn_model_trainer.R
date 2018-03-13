
## THIS IS THE MODEL TRAINER FOR THE RECOMMENDER SYSTEM
## IT USES KNN AS THE SIMILARITY ALGORITHM
## THE DATA UTILIZES THE DEMOGRAPHIC AS AN ADDITIONAL INFORMATION FOR THE MACHINE LEARNING ALGORITHM


rm(list = ls())
library(tidyverse)
library(data.table)


sourceTrain <- readRDS("./RData/train.RData")

sourceTrain <- sourceTrain[, -1]
sourceTrain <- as.matrix(sourceTrain)

