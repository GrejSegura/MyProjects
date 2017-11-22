
## Date: Oct 04/2017
## This will identify the unique visits by a card.number per day 


library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(DT)
set.seed(12)


data_1 <- read.csv(file.choose())


data_1 <- data_1[data_1$Len == 10,] ## remove Length of card.number != 10
data_1 <- data_1[, -2] ## remove Len


ind_char <- which((grepl("[^0-9]", data_1$Card.Number))) ## identify the non-digits characters

data_1 <- setDT(data_1[-ind_char, ]) ## remove card.numbers with non-digits character
data_1$Card.Number <- as.character(data_1$Card.Number)


data_1 <- data_1[order(data_1$Card.Number, data_1$Date, data_1$Time),] ## sort by card.number, date, time

##data_1[, flag_Dup_visit := c(1:.N), by = .("Card.Number", "Date", "Show")]

data_1 <- data_1 %>% group_by(Card.Number, Date, Show) %>% mutate(flag_dup_visit = c(1:n()))

## remove dupe visits -- if an ID visits >2 halls in succession in a given day
index <- which(data_1$flag_dup_visit > 1)
data_1 <- data_1[-index, -(length(names(data_1)))] ## remove dupes and flag_dup_visit column

data_1$Date <- mdy(data_1$Date) ## date formatting
data_1$day <- wday(data_1$Date) ## generates 2,3,4 which means 2 = monday, 3 = tue, 3 = wed

data_1$Show <- as.character(data_1$Show)

table(data_1$Show, data_1$Date) ## this will give the frequency count for Show visits per day
