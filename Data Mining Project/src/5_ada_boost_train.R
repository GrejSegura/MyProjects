
rm(list = ls())
library(fastAdaboost)
library(randomForest)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(doParallel)
library(ada)
set.seed(1)

clusters <- makeCluster(detectCores())
registerDoParallel(clusters)

#############################


train <- fread("./dta/train.csv")
test <- fread("./dta/test.csv")

train$label <- as.factor(train$label)
test$label <- as.factor(test$label)


'## Retain important variables only
imp_vars <- fread("./dta/importance.csv", sep = ",")
imp_vars <- imp_vars[imp_vars$MeanDecreaseAccuracy > 0, ]
imp_vars <- imp_vars$variable.1

dtaMining_vars <- names(dtaMining[, -1])

imp_vars <- dtaMining_vars %in% imp_vars

dtaMining <- dtaMining[, imp_vars]'



#################################




'
train_1 <- train_dta[,2:length(dtaMining)]
test_1 <- test_dta[,2:length(dtaMining)]

train_2 <- train_dta[, 1]
test_2 <- test_dta[, 1]

length(train_2)

train_1[] <- lapply(train_1, function(x) as.numeric(x))
train_2 <- as.numeric(train_2)
test_1[] <- lapply(test_1, function(x) as.numeric(x))
test_2 <- as.numeric(test_2)'



'# pack the training control parameters
adaControl <- trainControl(
	method = "cv",
	number = 5,
	verboseIter = TRUE,
	returnData = FALSE,
	returnResamp = "all",                                                        # save losses across all models
	classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
	summaryFunction = twoClassSummary,
	allowParallel = TRUE
)
'


# training adaboost model


train_1 <- train_dta[,2:length(train_dta)]
test_1 <- test_dta[,2:length(test_dta)]

train_2 <- train_dta[, 1]
test_2 <- test_dta[, 1]


adaboostModel <- ada(
	label ~.,
	data = train_dta,
	control = rpart.control(maxdepth = 8, cp = 0.010000, minsplit = 20, xval = 10), iter = 100
)



### PREDICTION EVALUATION

pred_ada <- predict(adaboostModel, test_1)
write.csv(pred_ada, "./dta/pred_ada.csv")

mseada <- table(pred_ada, as.matrix(test_2))

mseada


##compute accuracy

wrong <- ifelse(abs(test_2 - pred_ada) > 0, 1, 0)

error_ada <- sum(wrong) / length(as.factor(test_2))

error_ada
