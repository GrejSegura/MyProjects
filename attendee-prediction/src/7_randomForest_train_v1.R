
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
test$label <- as.factor(test$label)

train$daysToShow <- as.numeric(train$daysToShow)

rf_model <- randomForest(label ~ ., data = as.data.frame(train), ntree = 120, importance = TRUE)

# prediction and accurancy
pred_rf_train <- predict(rf_model, train)
pred_rf_train <- as.numeric(pred_rf_train)
pred_rf_train <- ifelse(pred_rf_train == 2, 1, 0)

pred_rf_test <- predict(rf_model, test)
pred_rf_test <- as.numeric(pred_rf_test)
pred_rf_test <- ifelse(pred_rf_test == 2, 1, 0)

save(rf_model, "./rfmodel.rda")
write.csv(pred_rf_train, "./dta/pred_rf_train.csv", row.names = FALSE)
write.csv(pred_rf_test, "./dta/pred_rf_test.csv", row.names = FALSE)


rf_mse <- table(pred_rf, as.matrix(test$label))
