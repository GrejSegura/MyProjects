
## THIS IS THE SCRIPT WHICH WILL GIVE A RECOMMENDATION TO THE ACTIVE USER ##

rm(list = ls())
library(flexclust)
library(tidyverse)

userItemData <- readRDS("./RData/userItemData.RData")
sourceTest <- readRDS("./RData/test.RData")
newSourceTrain <- readRDS("./RData/newSourceTrain.RData")
kmodel <- readRDS("./output/kmodel.rds")

sample <- sample(1:nrow(sourceTest), 1)
test <- sourceTest[sample, ]

predTest <- predict(kmodel, test[, -1])
test <- cbind(test, predTrain = predTest)

source <- newSourceTrain[newSourceTrain$predTrain == predTest, 1]

userItemData <- merge(userItemData, source, by = "barcode")
listItemFrequency <- userItemData %>% group_by(companyName) %>% summarise(count = n())
listItemFrequency <- listItemFrequency[order(listItemFrequency$count, decreasing = TRUE),]

## List the top 10 Recommendations ##
top10 <- listItemFrequency[1:10, ]
print(top10)
