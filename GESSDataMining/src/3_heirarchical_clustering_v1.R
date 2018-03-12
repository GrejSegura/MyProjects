
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


### heirarchical clustering ###

train_1.dist <- dist(train_1, method = "euclidean") ##expirement on the method to see better result
hcluster <- hclust(train_1.dist, method = "ward.D")
plot(hcluster)
rect.hclust(hcluster, k = 4, border = "red")

group <- cutree(hcluster, 4)
table(train_2, group)


train_1$group <- group

