## THIS IS THE SCRIPT WHERE THE NAIVE BAYES MODEL IS TRAINED FOR THE SELECTED DATA SUBSET IN THE PRECEDING predict_by_probability
## THIS WILL GENERATE PROBABILITY SCORES USING THE NAIVE BAYES ALGORITHM
## THE TOP10 MOST PROBABLE ITEM IS THEN SAVED

userItemData <- readRDS("./userItemData.RData")


userItemData <- userItemData %>% group_by(barcode) %>% mutate(seq_id = c(1:n())) ## create a sequence for visits
userItemData <- setDT(userItemData)

varnames <- paste("V", 1:max(userItemData$seq_id), sep = "")
n <- 1:max(userItemData$seq_id)

for (i in n){
	
	userItemData[seq_id >= i, varnames[i] := as.character(lag(companyName, max(seq_id)) - i), by = .(barcode)]
	userItemData[seq_id <= i, varnames[i] := "", by = .(barcode)]
	userItemData[seq_id == i, varnames[i] := "", by = .(barcode)]
}


## List the top 10 Recommendations ##
top10 <- listItemFrequency[1:10, ]
saveRDS(top10, "./output/top10.RData")