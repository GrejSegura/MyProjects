

# This is the machine learning part
# the algorithm used is XGBoost

rm(list = ls())

library(xgboost)
library(caret)
library(data.table)



# load the pre-processed data
gessData <- fread("./dta/gessData.csv", sep = ",", header = TRUE)
names(gessData)[1] <- "label"
gessData$label <- ifelse(gessData$label == "atd", 1, 0) # change labels to 1 if attended 0 if no show
dummy <- dummyVars(~., gessData)
gessData <- predict(dummy, gessData)
gessData <- as.data.frame(gessData)
gessData <- gessData[, -2]

# gessData[] <- lapply(gessData, function(x) as.numeric(x))

#################################
d <- 1:nrow(gessData)
index <- sample(d, round(nrow(gessData)*.7))



#gessData[] <- lapply(gessData, function(x) as.numeric(x))
train_dta <- gessData[index, ]
test_dta <- gessData[-index, ]



## XGBOOST ---

train_1 <- train_dta[,2:length(gessData)]
test_1 <- test_dta[,2:length(gessData)]

train_2 <- train_dta[, 1]
test_2 <- test_dta[, 1]

length(train_2)

train_1[] <- lapply(train_1, function(x) as.numeric(x))

test_1[] <- lapply(test_1, function(x) as.numeric(x))



xgtree_ <- xgboost(as.matrix(train_1), as.matrix(train_2), 
		   booster = 'gbtree',
		   objective = 'multi:softmax',
		   num_class = 2,
		   max.depth = 6,
		   eta = 0.0001,
		   nthread = 8,
		   nrounds = 300,
		   min_child_weight = 1,
		   subsample = 0.5, 
		   colsample_bytree = 1, 
		   num_parallel_tree = 1)

save(xgtree_, file = "./model/xgb_model.rda")
load("./model/xgb_model.rda")

#predict gbtree ----------------

pred_xgtree_ <- predict(xgtree_, as.matrix(test_1))

msexgtree_ <- table(pred_xgtree_, unlist(test_2))

msexgtree_

##compute accuracy

wrong <- ifelse(abs(test_2 - pred_xgtree_) == 0, 1, 0)

accuracy <- sum(wrong) / length(test_2)

accuracy



####### ####### ####### ####### ####### ####### ####### ####### ####### ####### 

## grid search


xgb_grid_1 <- expand.grid(
	nrounds = 300,
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

train_dta$label <- ifelse(train_dta$label == 1, "atd", "ns")
train_dta[, c(2:133)] <- lapply(train_dta[, c(2,133)], function(x) as.numeric(x))
xgb_train_1 <- train(
	x = train_dta[,2:133],
	y = train_dta$label,
	trControl = xgb_trcontrol_1,
	tuneGrid = xgb_grid_1,
	method = "xgbTree",
	allowParallel = TRUE
)

# Fitting nrounds = 300, max_depth = 2, eta = 1e-04, gamma = 1, colsample_bytree = 1, min_child_weight = 1, subsample = 0.5 on full training set
