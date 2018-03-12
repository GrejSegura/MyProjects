rm(list = ls())
library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)
library(caret)
library(lmtest)
memory.limit(50000)

train <- fread("./dta/train.csv")
test <- fread("./dta/test.csv")

train$label <- as.numeric(train$label)
test$label <- as.numeric(test$label)

logitModel <- glm(label ~., data = train, family = binomial(link = "logit"))

pred_lg_train <- predict(logitModel, train)
pred_lg_train <- ifelse(pred_logit > 0.5, 1, 0)

pred_lg_test <- predict(logitModel_1, test)
pred_lg_test <- ifelse(pred_logit > 0.5, 1, 0)

save(rf_model, "./logitModel.rda")
write.csv(pred_lg_train, "./dta/pred_lg_train.csv", row.names = FALSE)
write.csv(pred_lg_test, "./dta/pred_lg_test.csv", row.names = FALSE)

