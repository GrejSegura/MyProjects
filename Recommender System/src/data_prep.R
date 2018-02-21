
## THIS IS THE DATA PREPARATION PART OF THE RECOMMENDER SYSTEM PROJECT
## VARIOUS DATA IS CLEANED, MERGED AND JOINED
## THE DATA USED IS THE BIG5 2017 FAS - AND KONDUKO DATA
## WRITTEN BY: GREJELL SEGURA

## LAST UPADTE: 2/19/2018
## COMMENT: THE DATA FROM SOURCE IS EXTRACTED FROM INFOWEB, TOUCH AND CONVO COMES FROM KONDUKO

rm(list = ls())
memory.limit(100000)
set.seed(12345)

library(tidyverse)
library(data.table)
library(xlsx)

# load data
sourceData <- read.csv("./dta/source.csv", sep = ",", stringsAsFactors = TRUE)
touchData <- read.csv("./dta/touch.csv", sep = ",", stringsAsFactors = TRUE)
convoData <- read.csv("./dta/convo.csv", sep = ",", stringsAsFactors = TRUE)
codeData <- read.xlsx2("./dta/code.xlsx", sheetIndex = 1)

# convert to data.table
sourceData <- setDT(sourceData)
touchData <- setDT(touchData)
convoData <- setDT(convoData)


## 1. tidying the source data ##


codeData <- codeData[, c(3,4)] # cleaning code data #
names(codeData) <- c("code", "decode")

sourceNames <- paste0("v", 1:200) # create variable names
sourceData[, c(sourceNames) := tstrsplit(Barcode.List, ",", fixed = TRUE)] # split the barcode.list variable
sourceData <- melt(sourceData, measure.vars = sourceNames, variable.name = "barcodeList", value.name = "barcode")
sourceData[, barcode := gsub(" ", "", barcode)]
sourceData <- sourceData[!is.na(barcode), ]
summary(sourceData$barcode)
sourceData$barLength <- nchar(sourceData$barcode)
summary(as.factor(sourceData$barLength))
sourceData <- sourceData[sourceData$barLength == 14, ]
sourceData <- sourceData[, c("Country",
			     "CATEGORY",
			     "What.is.your.current.employment.status.",
			     "What.is.your.product.interest..MAIN..Big.5.", 
			     "What.is.your.product.interest..SUB..Big.5.", 
			     "What.is.your.company.s.primary.activity...Big.5.",
			     "Are.you.a.key.buyer.with.direct.purchasing.authority.for.your.company.",
			     "barcode")]
names(sourceData) <- c("country", "category", "employment", "mainProductInterest", "subProductInterest", "companyActivity", 
		       "purchasingAuthority", "barcode")
sourceData <- sourceData[, c("barcode", "country", "category", "employment", "mainProductInterest", "subProductInterest", 
			     "companyActivity", "purchasingAuthority")]
sourceData <- sourceData[!(sourceData$category =="EXH]" | sourceData$category =="ORG]" | sourceData$category ==""), ]
sourceData <- sourceData[, -c("category")]
str(sourceData)
sourceData <- melt(sourceData,  measure.vars = names(sourceData)[3:length(sourceData)], variable.name = "attribute", value.name = "value")
sourceNames <- paste0("v", 1:17)
sourceData[, c(sourceNames) := tstrsplit(value, "]", fixed = TRUE)]
sourceData[, c(3,4) := NULL]
sourceData <- melt(sourceData, measure.vars = sourceNames, variable.name = "attribute", value.name = "code")
sourceData <- sourceData[!is.na(code), ]
sourceData <- sourceData[sourceData$code != " ", ]
sourceData <- merge(sourceData, codeData, by = "code", all.x = TRUE)
sourceData <- sourceData[!is.na(sourceData$decode), ]
sourceData[, c(1,4) := NULL]
sourceData <- dcast(sourceData, barcode + country ~ decode)
sourceData[, c(3:44)] <- lapply(sourceData[, c(3:44)], function(x) ifelse(x > 0, 1, 0))

# save the cleaned data
saveRDS(sourceData, file = "./RData/sourceData.RData")



## 2. tidying the touch data ##

str(touchData)
touchData <- unique(touchData[, c("badgeMAC", "exhibitorId", "companyName")])
names(touchData)[1] <- "barcode"
touchData <- touchData[order(touchData$barcode),]
touchData <- touchData[!is.na(touchData$exhibitorId),]
touchData <- touchData[!is.na(touchData$barcode),]
touchData <- touchData[touchData$barcode != "",]
touchData$barcode <- as.character(touchData$barcode)
str(touchData)
any(is.na(touchData))
# save the cleaned touch data
saveRDS(touchData, file = "./RData/touchData.RData")


## 3. tidying the convo data ##
str(convoData)
convoData <- unique(convoData[, c("badgeMAC", "exhibitorId", "companyName")])
names(convoData)[1] <- "barcode"
convoData <- convoData[order(convoData$barcode),]
convoData <- convoData[!is.na(convoData$exhibitorId),]
convoData <- convoData[!is.na(convoData$barcode),]
convoData <- convoData[convoData$barcode != "",]
convoData$barcode <- as.character(convoData$barcode)
str(convoData)
any(is.na(convoData))
# save the cleaned touch data
saveRDS(convoData, file = "./RData/convoData.RData")

## 4. appending convo and touch data ##
visitData <- rbind(touchData, convoData)
visitData <- unique(visitData[, c("barcode", "companyName")])
visitData <- dcast(visitData, barcode ~ companyName)
visitData[, c(2:584)] <- lapply(visitData[, c(2:584)], function(x) ifelse(is.na(x), 0, 1))
varNames <- names(visitData[, c(2:584)])
visitData <- melt(visitData, measure.vars = varNames, variable.name = "attribute", value.name = "code")

names(visitData) <- c("userID", "itemid", "rating")
visitData <- visitData[order(userID),]

# create a matrix of barcode vs companyName
# visitData <- dcast(visitData, barcode ~ companyName)
# visitData <- as.matrix(visitData)

saveRDS(visitData, file = "./RData/visitData.RData")





