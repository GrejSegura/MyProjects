

library(xgboost)
library(randomForest)
library(caret)
library(data.table)
library(lubridate)

#############################

dtaMining <- read.csv("./dtaminingClean.csv", sep = ",", na.strings = c(" ", ""))

d <- 1:nrow(dtaMining)
index <- sample(d, round(nrow(dtaMining)*.8))

#dtaMining[] <- lapply(dtaMining, function(x) as.numeric(x))
train_dta <- dtaMining[index, ]
test_dta <- dtaMining[-index, ]

##################################################################

## XGBOOST ---

train_1 <- train_dta[,2:length(dtaMining)]
test_1 <- test_dta[,2:length(dtaMining)]

train_2 <- train_dta[, 1]
test_2 <- test_dta[, 1]

length(train_2)


xgtree_ <- xgboost(as.matrix(train_1), as.matrix(train_2), 
		   booster = 'gbtree',
		   objective = 'multi:softmax',
		   num_class = 2,
		   max.depth = 5,
		   eta = 0.1,
		   nthread = 4,
		   nrounds = 30,
		   min_child_weight = 1,
		   subsample = 0.5, 
		   colsample_bytree = 1, 
		   num_parallel_tree = 1)

#predict gbtree ----------------

pred_xgtree_ <- predict(xgtree_, as.matrix(test_1))

msexgtree_ <- table(as.factor(pred_xgtree_), as.factor(test_2$label))

msexgtree_

##compute accuracy

wrong <- ifelse(abs(test_2 - pred_xgtree_) > 0, 1, 0)

error_xg <- sum(wrong) / length(as.factor(test_2))

error_xg



##########################################################################################

## RANDOM FOREST




#rf_formula <- paste0(rf_formula, collapse = "+")

#rf_formula <- as.formula(paste("'label'", rf_formula, sep = "~"))


#train_dta <- train_dta[, -3]
#test_dta <- test_dta[, -3]

rf_model <- randomForest(as.factor(label) ~ ., data = as.matrix(train_dta[,-3]), ntree = 20, importance = TRUE)


# prediction and accurancy
pred_rf <- predict(rf_model, as.matrix(test_dta[, -c(1,3)]))

rf_mse <- table(as.factor(pred_rf), as.factor(test_dta$label))

rf_mse



############### importance #####
imp <- as.data.frame(importance(rf_model))
i <- imp[order(-(imp$MeanDecreaseAccuracy)),]

rnames <- rownames(i)
rnames[] <- lapply(rnames, function(x) gsub("^a", "", x))
rnames[] <- lapply(rnames, function(x) gsub("a$", "", x))
row.names(i) <- rnames

write.csv(i, "./importance.csv")
