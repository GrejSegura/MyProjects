
rm(list = ls())
library(xgboost)
library(randomForest)
library(caret)
library(data.table)
library(lubridate)
memory.limit(50000)

train <- fread("./dta/train.csv")
test <- fread("./dta/test.csv")

train$label <- as.factor(train$label)
ctrl <- trainControl(method="none",repeats = 1)
knnModel <- train(label ~ ., data = train, method = "knn", trControl = ctrl,  tuneGrid = data.frame(k = 5))

knnPredict <- predict(knnModel, knnModel = test,  type="prob")


kmeansModel <- kmeans(train[, -1], 2)
table(kmeansModel$cluster, as.matrix(train[, 1]))
