
## THIS IS JUST A SAMPLE RECOMMENDER -> A USER BASED COLLABORATIVE FILTERING
## THE DATA WAS THE DEMOGRAPHICS
## NO EXHIBITOR VISIT DATA WAS INCLUDED

rm(list = ls())
library(data.table)
library(recommenderlab)
data("MovieLense")

sampleData <- readRDS(".RData/visitData.RData")
sampleData <- setDT(sampleData)
sampleData <- sampleData[rating != 0,]

sums <- sampleData %>% group_by(userID) %>% summarise(sum = sum(rating)) ## check the number of rated items per user
summary(sums$sum) ## check the summary
hist(sums$sum) ## check the distribution

sampleData <- sampleData[, sum := sum(rating), by = userID] # create a column for sum of ratings
sampleData <- sampleData[sum > 9, ] # retain only those who rated atleast 4 items
sampleData <- sampleData[, sum := NULL] # remove the "sum" variable

'sampleData <- setDT(sampleData)
sampleData <- sampleData[, -c(1,2)]
sampleData <- t(sampleData)
sampleData <- as.data.frame(sampleData)'
sampleData <- as(sampleData, "binaryRatingMatrix")

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
	Recommender(sampleData, method = "IBCF"),
	Recommender(sampleData, method = "UBCF"),
	weights = c(.6, .4)
)

getModel(recom)

as(predict(recom, sampleData[1001]), "list")


# CREATE AN EVALUATION SCHEME

scheme <- evaluationScheme(sampleData, method = "cross-validation", train = 0.8, given = 10)

rec1a <- Recommender(getData(scheme, "train"), method = "IBCF")
rec2a <- Recommender(getData(scheme, "train"), method = "UBCF")
rec3a <- Recommender(getData(scheme, "train"), method = "POPULAR")


pred1a <- predict(rec1a, getData(scheme, "known"), type = "topNList", n = 10)
pred2a <- predict(rec2a, getData(scheme, "known"), type = "topNList", n = 10)
pred3a <- predict(rec3a, getData(scheme, "known"), type = "topNList", n = 10)

error1a <- calcPredictionAccuracy(pred1a, getData(scheme, "unknown"), given = 10, goodRating = 1)
error2a <- calcPredictionAccuracy(pred2a, getData(scheme, "unknown"), given = 10, goodRating = 1)
error3a <- calcPredictionAccuracy(pred3a, getData(scheme, "unknown"), given = 10, goodRating = 1)


errorList <- rbind(error1a, error2a, error3a)

rownames(errorList) <- c("IBCF","UBCF", "POPULARITY")
errorList
