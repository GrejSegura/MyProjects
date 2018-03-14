
## THIS IS JUST A SAMPLE RECOMMENDER -> A USER BASED COLLABORATIVE FILTERING
## THE DATA IS BASICALLY A PAIRING OF VISITORS AND EXHIBITORS, IT LISTS THE EXHIBITORS THAT THE VISITORS HAVE MADE AN INTERACTION WITH

rm(list = ls())
library(tidyverse)
library(data.table)
library(recommenderlab)

sampleData <- readRDS("./RData/visitData.RData")
sampleData <- setDT(sampleData)
sampleData <- sampleData[rating != 0,]

# count the number of items per user
sumsItems <- sampleData %>% group_by(userID) %>% summarise(sum = sum(rating)) ## check the number of rated items per user
summary(sumsItems$sum) ## check the summary
hist(sumsItems$sum, breaks = 200) ## check the distribution

# count the number of users per item
sumsUser <- sampleData %>% group_by(itemid) %>% summarise(sum = sum(rating)) ## check the number of rated items per user
summary(sumsUser$sum, breaks = 200) ## check the summary
hist(sumsUser$sum) ## check the distribution


sampleData <- sampleData[, sum1 := sum(rating), by = userID] # create a column for sum of ratings
sampleData1 <- sampleData[sum1 > 10, ] # retain only those who rated atleast 4 items
sampleData1 <- sampleData1[, sum1 := NULL] # remove the "sum" variable

sampleData <- sampleData[, sum2 := sum(rating), by = itemid] # create a column for sum of ratings
sampleData2 <- sampleData[sum2 > 49, ] # retain only those who rated atleast 4 items
sampleData2 <- sampleData2[, sum2 := NULL] # remove the "sum" variable



sampleData1 <- as(sampleData1, "binaryRatingMatrix")
sampleData2 <- as(sampleData2, "binaryRatingMatrix")

# corTable <- cor(sampleData)

# r1 <- Recommender(sampleData[c(1:34299), ], method = "POPULAR")
# r2 <- Recommender(sampleData[c(1:34299), ],  method = "UBCF")
# r3 <- Recommender(sampleData[c(1:34299), ],  method = "IBCF")


rec1 <- Recommender(sampleData, method = "IBCF")
recommend1 <- predict(rec1, sampleData[1000], type = "topNList")

rec2 <- Recommender(sampleData, method = "UBCF")
recommend2 <- predict(rec2, sampleData[1000], type = "topNList")

rec3 <- Recommender(sampleData, method = "POPULAR")
recommend3 <- predict(rec3, sampleData[1000], type = "topNList")

as(recommend1, "list")
as(recommend2, "list")
as(recommend3, "list")

# HYBRID RECOMMENDER
recom <- HybridRecommender(
	Recommender(sampleData2, method = "IBCF"),
	Recommender(sampleData1, method = "UBCF"),
	weights = c(.4, .6)
)

getModel(recom)

as(predict(recom, sampleData[1001]), "list")


# CREATE AN EVALUATION SCHEME

scheme1 <- evaluationScheme(sampleData2, method = "split", train = 0.8, given = 1)
scheme2 <- evaluationScheme(sampleData1, method = "cross", train = 0.8, given = 5)

rec1a <- Recommender(getData(scheme1, "train"), method = "IBCF")
rec2a <- Recommender(getData(scheme2, "train"), method = "UBCF")
rec3a <- Recommender(getData(scheme2, "train"), method = "POPULAR")

pred1a <- predict(rec1a, getData(scheme1, "known"), type = "topNList", n = 10)
pred2a <- predict(rec2a, getData(scheme2, "known"), type = "topNList", n = 10)
pred3a <- predict(rec3a, getData(scheme2, "known"), type = "topNList", n = 10)
a <- as(pred1a, "list")
b <- as(pred2a, "list")
c <- as(pred3a, "list")


error1a <- calcPredictionAccuracy(pred1a, getData(scheme1, "unknown"), given = 1, goodRating = 1)
error2a <- calcPredictionAccuracy(pred2a, getData(scheme2, "unknown"), given = 5, goodRating = 1)
error3a <- calcPredictionAccuracy(pred3a, getData(scheme2, "unknown"), given = 5, goodRating = 1)


errorList <- rbind(error1a, error2a, error3a)

rownames(errorList) <- c("IBCF","UBCF", "POPULARITY")
errorList



## ANOTHER APPROACH ##
scheme2 <- evaluationScheme(sampleData1, method = "cross", train = 0.8, given = 3)
results <- evaluate(scheme2, method = "UBCF", param = list(method = "Jaccard", nn = 50), n = c(1,3,5,10,15,20))
getConfusionMatrix(results)[[1]]
avg(results)
plot(results, annotate = TRUE)
plot(results, "prec/rec", annotate = TRUE)



## COMPARING MULTIPLE MODELS ##
algorithms <- list(`random items` = list(name = "RANDOM",
	param = NULL), `popular items` = list(name = "POPULAR",
	param = NULL), `user-based CF` = list(name = "UBCF",
	param = list(method = "Jaccard", nn = 50)), `item-based CF` = list(name = "IBCF",
	param = list(method = "Jaccard", k = 50)), `association rules` = list(name = "AR",
	param = list(supp = 0.001, conf = 0.2, maxlen = 2)))
results <- evaluate(scheme2, algorithms, n = seq(from = 1, to = 1000, by = 100))

plot(results, annotate = c(1, 3), legend = "right")
plot(results, "prec/rec", annotate = 3)

