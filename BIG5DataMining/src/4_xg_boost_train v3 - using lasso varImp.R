rm(list = ls())
library(xgboost)
library(randomForest)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)

## THIS IS AN XGBOOST TRAINER USING THE LOGISTIC REGRESSION FILE

dtaMining <- fread("./dta/dtaminingClean_for_logisticRegression.csv", sep = ",")
sigVars <- read.csv("./dta/lassoVarImp.csv")
sigVars <- sigVars[, 2]
sigVars <- as.vector(sigVars)
label <- dtaMining[, 1]
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
		   max.depth = 7,
		   eta = 10/1000,
		   nthread = 8,
		   nrounds = 1000,
		   min_child_weight = 3/(nrow(train_dta[train_dta$label == "1",])/nrow(train_dta)),
		   subsample = 0.5,
		   colsample_bytree = 1, 
		   num_parallel_tree = 1)

#predict gbtree ----------------

pred_xg_train <- predict(xgModel, as.matrix(train_1))
pred_xg_test <- predict(xgModel, as.matrix(test_1))
table(pred_xg_test, as.matrix(test_2))


wrong <- ifelse(abs(test_2 - pred_xg_test) > 0, 1, 0)

error_xg <- sum(wrong) / nrow(test_2)

error_xg


save(rf_model, "./xgModel.rda")
write.csv(pred_xg_train, "./dta/pred_xg_train.csv", row.names = FALSE)
write.csv(pred_xg_test, "./dta/pred_xg_test.csv", row.names = FALSE)

##compute accuracy

xg_cv <- xgb.cv(data = as.matrix(train_1), nfold = 6, label = train_2, nround = 30, objective = "binary:logistic", eval_metric = "error")


###########################################
## Grid Search ###

set.seed(12340)
clusters <- makeCluster(detectCores())
registerDoParallel(clusters)


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
	method = "xgbTree",
	allowParallel = TRUE
)

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) +
	geom_point() +
	theme_bw() +
	scale_size_continuous(guide = "none")