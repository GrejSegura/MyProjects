
rm(list = ls())
library(xgboost)
library(randomForest)
library(caret)
library(data.table)
library(lubridate)

#############################

dtaMining <- read.csv("./dta/dtaminingClean.csv", sep = ",", na.strings = c(" ", ""))
dtaMining <- dtaMining[, -1]
names(dtaMining)[1] <- "label"

d <- 1:nrow(dtaMining)
index <- sample(d, round(nrow(dtaMining)*.8))



#dtaMining[] <- lapply(dtaMining, function(x) as.numeric(x))
train_dta <- dtaMining[index, ]
test_dta <- dtaMining[-index, ]

##########################################################################################


##########################################################################################

## RANDOM FOREST




#rf_formula <- paste0(rf_formula, collapse = "+")

#rf_formula <- as.formula(paste("'label'", rf_formula, sep = "~"))


#train_dta <- train_dta[, -3]
#test_dta <- test_dta[, -3]

rf_model <- randomForest(as.factor(label) ~ ., data = as.matrix(train_dta), ntree = 120, importance = TRUE)
save(rf_model, "./rf_model.rds")

# prediction and accurancy
pred_rf <- predict(rf_model, as.matrix(test_dta))

rf_mse <- table(as.factor(pred_rf), as.factor(test_dta$label))

rf_mse

(4896+7121)/nrow(test_dta) ## out of sample accuracy

importance(rf_model)


############### importance #####
imp <- as.data.frame(importance(rf_model))
i <- imp[order(-(imp$MeanDecreaseAccuracy)),]

rnames <- rownames(i)
rnames[] <- lapply(rnames, function(x) gsub("^a", "", x))
rnames[] <- lapply(rnames, function(x) gsub("a$", "", x))
row.names(i) <- rnames

write.csv(i, "./dta/importance.csv")











