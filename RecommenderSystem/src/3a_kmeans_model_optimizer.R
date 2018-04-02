
# THIS IS USED TO SUGGEST THE OPTIMUM NUMBER OF CLUSTERS FOR THE KMEANS CLUSTERING ##
# THE OUTPUT WILL PLOT THE TOTAL SUM OF SQUARES VS NUMBER OF POSSIBLE CLUSTERS
# THE "ELBOW" WILL SHOW THE LEAST CHANGE IN THE SUM OF SQUARES FOR THE CLUSTERS
# WHICH MEANS THERE IS LITTER CHANGE OF VARIANCE IN THE PROCEEDING NUMBER OF CLUSTER


rm(list = ls())
library(doParallel)
set.seed(12345)
clusters <- makeCluster(detectCores())
registerDoParallel(clusters)

sourceTrain <- readRDS("./RData/train.RData")


## use elbow method to find the optimum number of clusters ##
k.max <- 80
data <- sourceTrain[, -1]
wss <- sapply(2:k.max, 
	      function(k){kmeans(data, k, nstart = 50,iter.max = 15 )$tot.withinss})
wss

plot(2:80, wss[2:80],
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")

## chose 10 clusters