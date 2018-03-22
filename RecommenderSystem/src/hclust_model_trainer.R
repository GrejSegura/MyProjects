

## THIS IS A TRAINER FOR THE RECOMMENDER SYSTEM
## IT USES HIERARCHICHAL CLUSTERING TO FIND THE SIMILARITY BETWEEN THE USERS
## THE DATA UTILIZES THE DEMOGRAPHIC AS AN ADDITIONAL INFORMATION FOR THE MACHINE LEARNING ALGORITHM


rm(list = ls())
library(tidyverse)
library(data.table)
set.seed(32134)

sourceTrain <- readRDS("./RData/train.RData")

## train the model ##
distance <- dist(sourceTrain[, -1], method = "binary")
hclustModel <- hclust(distance)
plot(hclustModel)

## cut the cluster into specific number of group = k ##
cluster <- cutree(hclustModel, k = 20)
sourceTrain$group <- cluster ## cluster group added as new feature in the data ##
table(cluster)
plot(cluster)
plot(hclustModel)
