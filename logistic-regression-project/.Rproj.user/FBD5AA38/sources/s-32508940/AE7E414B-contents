rm(list = ls())
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(magrittr)
library(DT)
library(xgboost)
library(caret)
set.seed(43211)

## LOAD THE SOURCE CODES DATA
data_1 <- fread("./dta/cleanData1.csv")
codeData <- fread("./dta/sourceCode_logit.csv", sep = ",", na.strings = c(" ", ""))  ### data for regional groups
names(codeData)[2] <- "value"

fasData <- fread("./dta/fasData.csv")
fasData <- merge(fasData, codeData, by = "value", all.x = TRUE)
fasData$country <- ifelse(fasData$country == "United Arab Emirates", "Local", "International")
fasData <- fasData[, -c(1,5)]
names(fasData)[c(2,4)] <- c("region", "value")

fasData <- dcast(fasData, id + region ~ value)
fasData$region <- ifelse(fasData$region == "Local", 1, 0)

fasData$id <- as.character(fasData$id)
data_1$id <- as.character(data_1$id)
fasData <- merge(fasData, data_1, by = "id", all = TRUE)
fasData <- fasData[!(is.na(Show)), ]
fasData <- fasData[!(Show == "CONFERENCE"), ]
fasData <- fasData[!(Show == "MAJLIS Lounge"), ]
fasData <- fasData[!(Show == "Connexions Lounge"), ]
fwrite(fasData, "./dta/cleanData3_Logit.csv", row.names = FALSE)


## LOAD SHOW DATA

showData <- fread("./dta/showData.csv")
fasData <- fread("./dta/cleanData3_Logit.csv")
fasData <- merge(fasData, showData, by = "Show", all.x = TRUE)

names(showData)[c(1,2)] <- c("visited_show", "visited")

fasData <- merge(fasData, showData, by = "visited_show", all.x = TRUE)
fasData <- fasData[!(is.na(region)), ]
fasData <- fasData[region != "", ]
fasData <- fasData[, -c(1,2,3)]
fwrite(fasData, "./dta/cleanData3_Logit.csv", row.names = FALSE)


##### ########## ########## ########## ########## ########## ########## ########## ########## ########## #####

## 1. Network and Security
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 1, 1, 0)

logit1 <- glm(label ~., dta, family = binomial(link = "logit"))

significantVars <- coef(summary(logit1))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables1.csv")


## 2. Security and Consumer Technology
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 2, 1, 0)

logit2 <- glm(label ~., dta, family = binomial(link = "logit"))

significantVars <- coef(summary(logit2))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables2.csv")


## 3. Value Added Distributors
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 3, 1, 0)

logit3 <- glm(label ~., dta, family = binomial(link = "logit"))

significantVars <- coef(summary(logit3))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables3.csv")


## 4. Printing and Business Automation
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 4, 1, 0)

logit4 <- glm(label ~., dta, family = binomial(link = "logit"))

significantVars <- coef(summary(logit4))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables4.csv")


## 5. Oracle
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 5, 1, 0)

logit5 <- glm(label ~., dta, family = binomial(link = "logit"))

significantVars <- coef(summary(logit5))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables5.csv")


## 6. Big Data or Cloud
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 6, 1, 0)

logit6 <- glm(label ~., dta, family = binomial(link = "logit"))

significantVars <- coef(summary(logit6))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables6.csv")



## 7. AR VR AI UAV
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 7, 1, 0)

logit7 <- glm(label ~., dta, family = binomial(link = "logit"))


significantVars <- coef(summary(logit7))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables7.csv")


## 8. IoT
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 8, 1, 0)

logit8 <- glm(label ~., dta, family = binomial(link = "logit"))

significantVars <- coef(summary(logit8))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables8.csv")



## 9. Global Smart Cities
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 9, 1, 0)

logit9 <- glm(label ~., dta, family = binomial(link = "logit"))

significantVars <- coef(summary(logit9))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables9.csv")



## 10. Gulfcomms and Mobility
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 10, 1, 0)

logit10 <- glm(label ~., dta, family = binomial(link = "logit"))

significantVars <- coef(summary(logit10))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables10.csv")



## 11. GFS
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 11, 1, 0)

logit11 <- glm(label ~., dta, family = binomial(link = "logit"))


significantVars <- coef(summary(logit11))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables11.csv")


## 12. Global SMEs
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 12, 1, 0)

logit12 <- glm(label ~., dta, family = binomial(link = "logit"))


significantVars <- coef(summary(logit12))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables12.csv")


## 13. Future Tech
dta <- fread("./dta/cleanData3_Logit.csv")
dta <- dta[!(is.na(dta$label)),]

dta$label <- ifelse(dta$label == 13, 1, 0)

logit13 <- glm(label ~., dta, family = binomial(link = "logit"))

significantVars <- coef(summary(logit13))
significantVars <- as.data.frame(significantVars)
significantVars <- significantVars[significantVars[, "Pr(>|z|)"] < 0.1, ]
write.csv(significantVars, "./dta/SignificantVariables13.csv")
