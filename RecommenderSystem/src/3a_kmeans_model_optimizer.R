
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
k.max <- 20
data <- sourceTrain[, -1]
wss <- sapply(2:k.max, 
	      function(k){kmeans(data, k, nstart = 50,iter.max = 15 )$tot.withinss})
wss
saveRDS(wss,"./output/kmeans_wss.rds")

wss <- readRDS("./output/kmeans_wss.rds")
plot(2:10, wss[2:10],
     type = "b", pch = 19, frame = TRUE, 
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")
abline(v = 7, untf = FALSE, col = "red")

y <- wss[2:10]
x <- c(2:10)
data <- as.data.frame(cbind(x, y))
elbow <- ggplot(data, aes(x = x, y = y)) + geom_line() + geom_point(size = 5) + 
	labs(title = "K-means Clustering Elbow Method", x = "No. of Clusters", y = "Total within-clusters sum of squares") + geom_abline(mapping = aes(x = 7), slope = 0, intercept = 7)
elbow
## chose 9 clusters