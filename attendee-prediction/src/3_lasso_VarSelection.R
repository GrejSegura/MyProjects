rm(list = ls())
library(xgboost)
library(randomForest)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(glmnet)
library(doParallel)

set.seed(1234)
clusters <- makeCluster(detectCores())
registerDoParallel(clusters)

## THIS IS AN XGBOOST TRAINER USING THE LOGISTIC REGRESSION FILE

dtaMining <- fread("./dta/dtaminingClean_for_logisticRegression.csv", sep = ",")
names(dtaMining)[1] <- "label"
train_dta <- dtaMining[, 2:max(length(dtaMining))]
label <- dtaMining[, 1]

lassoModel <- cv.glmnet(as.matrix(train_dta), as.matrix(label), family = 'binomial', alpha = 1)
varImp <- coef(lassoModel, s = "lambda.1se")
varImp <- as.matrix(varImp)
varImp <- as.data.frame(varImp)
row <- row.names(varImp)
varImp$vars <- row
names(varImp) <- c("se", "var")
varImp <- varImp[varImp$se != 0,]
varImp <- varImp[-1, ]

write.csv(varImp, "./dta/lassoVarImp.csv", row.names = FALSE)

