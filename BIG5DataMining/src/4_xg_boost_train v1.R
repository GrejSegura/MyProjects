
rm(list = ls())
library(xgboost)
library(randomForest)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)

#######################################################################################

dtaMining <- read.csv("./dta/dtaminingClean.csv", sep = ",", na.strings = c(" ", ""))
dtaMining <- dtaMining[, -1]
names(dtaMining)[1] <- "label"



## Retain important variables only
imp_vars <- read.csv("./dta/importance.csv", sep = ",")
imp_vars <- imp_vars[imp_vars$MeanDecreaseAccuracy > 0, ]
imp_vars <- imp_vars$variable.1

dtaMining_vars <- names(dtaMining[, -1])

imp_vars <- dtaMining_vars %in% imp_vars

dtaMining <- dtaMining[, imp_vars]



#######################################################################################
d <- 1:nrow(dtaMining)
index <- sample(d, round(nrow(dtaMining)*.8))



#dtaMining[] <- lapply(dtaMining, function(x) as.numeric(x))
train_dta <- dtaMining[index, ]
test_dta <- dtaMining[-index, ]



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


xgtree_ <- xgboost(as.matrix(train_1), as.matrix(train_2), 
		   booster = 'gbtree',
		   objective = 'multi:softmax',
		   num_class = 2,
		   max.depth = 7,
		   eta = 0.1,
		   nthread = 8,
		   nrounds = 100,
		   min_child_weight = 1,
		   subsample = 0.5, 
		   colsample_bytree = 1, 
		   num_parallel_tree = 1)
save(xgtree_, file = "xgb_model.rda")
load("xgb_model.rda")

#predict gbtree ----------------

pred_xgtree_ <- predict(xgtree_, as.matrix(test_1))

msexgtree_ <- table(as.factor(pred_xgtree_), as.factor(test_2))

msexgtree_

##compute accuracy

wrong <- ifelse(abs(test_2 - pred_xgtree_) > 0, 1, 0)

error_xg <- sum(wrong) / length(as.factor(test_2))

error_xg



### cross validate

xg_cv <- xgb.cv(data = as.matrix(train_1), nfold = 6, label = train_2, nround = 30, objective = "binary:logistic", eval_metric = "error")


###########################################
## Grid Search ###
train_2 <- ifelse(train_2 == 1, "att", "nos")

xgb_grid_1 <- expand.grid(
	nrounds = 1000,
	eta = c(0.01, 0.001, 0.0001),
	max_depth = c(2, 4, 6, 8, 10),
	gamma = 1,
	colsample_bytree = 1,
	min_child_weight = 1,
	subsample = 0.5
)

# pack the training control parameters
xgb_trcontrol_1 <- trainControl(
	method = "cv",
	number = 5,
	verboseIter = TRUE,
	returnData = FALSE,
	returnResamp = "all",                                                        # save losses across all models
	classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
	summaryFunction = twoClassSummary,
	allowParallel = TRUE
)

# train the model for each parameter combination in the grid,
#   using CV to evaluate
xgb_train_1 <- train(
	x = as.matrix(train_1),
	y = as.factor(train_2),
	trControl = xgb_trcontrol_1,
	tuneGrid = xgb_grid_1,
	method = "xgbTree"
)

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) +
	geom_point() +
	theme_bw() +
	scale_size_continuous(guide = "none")