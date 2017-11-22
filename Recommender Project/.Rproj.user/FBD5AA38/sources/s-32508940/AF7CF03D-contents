rm(list = ls())
library(xgboost)
library(randomForest)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(DiagrammeR)

## THIS IS AN XGBOOST TRAINER USING THE LOGISTIC REGRESSION FILE

dtaMining <- fread("./dta/dtaminingClean_for_logisticRegression.csv", sep = ",")
sigVars <- read.csv("./dta/SignificantVariables.csv")
sigVars <- sigVars[, 1]
sigVars <- as.vector(sigVars)
names(dtaMining)[1] <- "label"

dtaMining <- dtaMining[, c("label", sigVars), with = FALSE]


d <- 1:nrow(dtaMining)
index <- sample(d, round(nrow(dtaMining)*.8))



#dtaMining[] <- lapply(dtaMining, function(x) as.numeric(x))
train_dta <- dtaMining[index, ]
test_dta <- dtaMining[-index, ]

write.csv(train_dta, "./dta/train.csv", row.names = FALSE)
write.csv(test_dta, "./dta/test.csv", row.names = FALSE)

## XGBOOST ---

train_1 <- train_dta[,2:length(dtaMining)]
test_1 <- test_dta[,2:length(dtaMining)]

train_2 <- train_dta[, 1]
test_2 <- test_dta[, 1]

length(train_2)

train_1[] <- lapply(train_1, function(x) as.numeric(x))
train_2 <- as.numeric(train_2)
test_1[] <- lapply(test_1, function(x) as.numeric(x))
test_2 <- as.numeric(test_2)


xgModel <- xgboost(as.matrix(train_1), as.matrix(train_2), 
		   booster = 'gbtree',
		   objective = 'multi:softmax',
		   num_class = 2,
		   max.depth = 8,
		   eta = 0.01,
		   nthread = 8,
		   nrounds = 1200,
		   min_child_weight = 1,
		   subsample = .75,
		   colsample_bytree = .8, 
		   num_parallel_tree = 1)


#predict gbtree ----------------

pred_xg_train <- predict(xgModel, as.matrix(train_1))
pred_xg_test <- predict(xgModel, as.matrix(test_1))
table(pred_xg_test, as.matrix(test_2))

save(xgModel, "./xgModel.rda")
write.csv(pred_xg_train, "./dta/pred_xg_train.csv", row.names = FALSE)
write.csv(pred_xg_test, "./dta/pred_xg_test.csv", row.names = FALSE)



## VARIABLE IMPORTANCE ##

names <- dimnames(train_1)[[2]]

varImportance <- xgb.importance(names, model = xgModel)
kmeansVarImp <- kmeans(varImportance[,-c(1,3,4)], centers = 5)
kmeansVarImp$centers
varImportance <- cbind(varImportance, kmeansVarImp$cluster)
xgb.plot.importance(varImportance[1:50,])

plot <- ggplot(varImportance[varImportance$V2 != 4, ], aes(x = Cover, y = Frequency, size = Gain, color = as.factor(V2))) + geom_point(alpha = .5)
plot
varImportance <- varImportance[varImportance$V2 != 5, ]
varImportance <- varImportance$Feature




dtaMining <- dtaMining[, c("label", varImportance), with = FALSE]

d <- 1:nrow(dtaMining)
index <- sample(d, round(nrow(dtaMining)*.8))



#dtaMining[] <- lapply(dtaMining, function(x) as.numeric(x))
train_dta <- dtaMining[index, ]
test_dta <- dtaMining[-index, ]

write.csv(train_dta, "./dta/train.csv", row.names = FALSE)
write.csv(test_dta, "./dta/test.csv", row.names = FALSE)

## XGBOOST ---

train_1 <- train_dta[,2:length(dtaMining)]
test_1 <- test_dta[,2:length(dtaMining)]

train_2 <- train_dta[, 1]
test_2 <- test_dta[, 1]

length(train_2)

train_1[] <- lapply(train_1, function(x) as.numeric(x))
train_2 <- as.numeric(train_2)
test_1[] <- lapply(test_1, function(x) as.numeric(x))
test_2 <- as.numeric(test_2)


xgModel <- xgboost(as.matrix(train_1), as.matrix(train_2), 
		   booster = 'gbtree',
		   objective = 'multi:softmax',
		   num_class = 2,
		   max.depth = 8,
		   eta = 0.001,
		   nthread = 8,
		   nrounds = 1500,
		   min_child_weight = 1,
		   subsample = .75,
		   colsample_bytree = .8, 
		   num_parallel_tree = 1)


#predict gbtree ----------------

pred_xg_train <- predict(xgModel, as.matrix(train_1))
pred_xg_test <- predict(xgModel, as.matrix(test_1))
table(pred_xg_test, as.matrix(test_2))



names <- dimnames(train_1)[[2]]

varImportance <- xgb.importance(names, model = xgModel)
xgb.plot.importance(varImportance)






#xgb.plot.tree(feature_names = colnames(train_1), model = xgModel)


##compute accuracy
## subsample = .5 , colsample = 1 ---- accuracy 63.83
## subsample = .75 , colsample = .75 ---- accuracy 64.16
## subsample = .75 , colsample = .6 ---- accuracy 64.100
## subsample = .5 , colsample = .8 ---- accuracy 63.97
## subsample = .75 , colsample = .4 ---- accuracy 64.08
## subsample = .75 , colsample = 1 ---- accuracy 63.88
## subsample = 1 , colsample = 1 ---- accuracy 64.09
## subsample = .75 , colsample = .75, nrounds = 1500 ---- accuracy 64.20
## subsample = .75 , colsample = .75, nrounds = 2000 ---- accuracy 64.288
## subsample = .75 , colsample = .8, nrounds = 1000 ---- accuracy 64.21
## subsample = .75 , colsample = .8, nrounds = 1200 ---- accuracy 63.85


## using lasso significant vars
##compute accuracy
## subsample = .75 , colsample = .8, nrounds = 1200 ---- accuracy 63.065