
## THIS IS A PREDICTOR USING THE KMEANS MODEL

rm(list = ls())
library(flexclust)
library(tidyverse)


newSourceData <- readRDS("./RData/newSource.RData")
kmodel <- readRDS("./output/kmodel.rds")


## create a data.frame for prediction model ##


