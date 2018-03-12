
rm(list = ls())
library(xgboost)
library(randomForest)
library(caret)
library(data.table)
library(lubridate)
memory.limit(50000)


## Ensembling ##

pred_xg <- fread("./dta/pred_xg.csv")
pred_logit <- fread("./dta/pred_logit.csv")
pred_rf <- fread("./dta/pred_rf.csv")

train <- fread("./dta/train.csv")
test <- fread("./dta/test.csv")

corData <- cbind(pred_logit, pred_xg, pred_rf, train[, 1])
cor(corData)
names(corData) <- c("lg", "xg", "rf", "label")
corData[] <- lapply(corData, function(x) as.numeric(x))

label <- corData[, 4]
corData1 <- corData[, c(1:3)]
corData1 <- as.data.frame(corData1)
label <- as.numeric(unlist(label))

xg_2ndLayer <- xgboost(as.matrix(corData1), label, 
		       booster = 'gbtree',
		       objective = 'multi:softmax',
		       num_class = 2,
		       max.depth = 8,
		       eta = 10/500,
		       nthread = 8,
		       nrounds = 1000,
		       min_child_weight = 3/(nrow(corData[corData$label == "1",])/nrow(corData)),
		       subsample = 0.5,
		       colsample_bytree = 1, 
		       num_parallel_tree = 1)


test[] <- lapply(test, function(x) as.numeric(x))

pred_xgtree_ <- predict(xg_2ndLayer, as.matrix(test))

msexgtree_ <- table(pred_xgtree_, test[,1])

msexgtree_
