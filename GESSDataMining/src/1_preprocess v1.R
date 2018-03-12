
## This is a data mining project for GESS
## Data is based on 2017
## Author: Grejell Segura

rm(list = ls())

library(data.table)
library(tidyverse)
library(caret)


atdData <- fread("./dta/attd.csv", sep = ",") # attended data
nsData <- fread("./dta/ns.csv", sep = ",") # no show data
codeData <- fread("./dta/codes_v2.csv", sep = ",") # codes
regionData <- fread("./dta/region.csv", sep = ",") # regions data

# create an attendance columns
atdData$attended <- "atd"
nsData$attended <- "ns"

atdData[] <- lapply(atdData, function(x) as.character(x))  # change all features to characters to be able to join to nsData
nsData[] <- lapply(nsData, function(x) as.character(x))  # change all features to characters to be able to join atdData

# combine data
gessData <- rbind(atdData, nsData)

# summarize
summary(gessData)
any(is.na(gessData))
str(gessData)

# arrange columns
gessData <- gessData[, c(1,2,19,3:18)]

# gather the columns to make one feature for all the responses
# this will ease the cleaning process
gessData <- melt(gessData, measure.vars = names(gessData)[4:length(gessData)], variable.name = "attribute", value.name = "value")

# next is to split the responses delimited by ]
# first, create names for the features where the splitted strings will be dumped
names <- paste0("v", 1:22)
gessData[, c(names) := tstrsplit(value, "]", fixed = TRUE)] # split delimited by ]
gessData <- gessData[, -"value"]

# gather the variables again in one column
gessData <- melt(gessData, measure.vars = names(gessData)[5:length(gessData)], variable.name = "attribute2", value.name = "value")
gessData <- gessData[, -"attribute2"]
gessData <- gessData[!(is.na(gessData$value)),] # remove NAs
gessData$value <- paste(gessData$value, "-17", sep = "")

# lookup the values in the codes data - to identify the descriptions
names(codeData)[c(3,4)] <- c("value", "decode") # change the column names to match data
codeData$value <- as.character(codeData$value)
gessData$value <- as.character(gessData$value)
codeData <- unique(codeData)
gessData <- merge(gessData, codeData, by = "value", all.x = TRUE)
gessData <- gessData[,-c(1,6,7)] # remove the unnecessary coulmns

# dummify the categories
names(gessData) <- c("id", "country", "attended", "attribute", "value")
names(regionData)[1] <- "country"
gessData <- merge(gessData, regionData, by = "country", all.x = TRUE)
gessData$Region[gessData$country == "United Arab Emirates"] <- "UAE"
gessData <- gessData[, c(2:6)]
gessData <- dcast(gessData, id + Region + attended ~ value)
gessData <- gessData[, c(3,2,4:86)]
#gessData[,c(3:85)] <- lapply(gessData[,c(3:85)], function(x) ifelse(is.na(x), 0, 1))

fwrite(gessData, "./dta/gessData.csv", row.names = FALSE)
