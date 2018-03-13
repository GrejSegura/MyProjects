

##  THIS IS USED TO TEST THE KMEANS MODEL ##

rm(list = ls())
library(flexclust)
library(tidyverse)

sourceTest <- readRDS("./RData/test.RData")
newSourceData <- readRDS("./RData/newSource.RData")
kmodel <- readRDS("./output/kmodel.rds")


predTest <- predict(kmodel, sourceTest[, -c(1,2)])
newSourceTest <- cbind(sourceTest, predTest)
