

## This is a new project for Info Salons ##

library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
set.seed(123)


data_1 <- read.csv("C:/Users/Grejell/Documents/Data Analysis/GULFHOST 2017/GFH-AFS 2017 Gate Scans.csv")


ind_char <- !is.na(as.numeric(data_1$Card.Number))

data_1 <- setDT(data_1[ind_char, ])
data_1$Card.Number <- as.numeric(data_1$Card.Number)

data_1 <- data_1[data_1$Len == 10,] ## remove Length of card.number != 10
data_1 <- data_1[, -2] ## remove Len
data_1 <- data_1[order(data_1$Card.Number, data_1$Date, data_1$Time),] ## sort by card.number, date, time

data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(flag_dup_visit = ifelse(Show == lag(Show), 1, 0))

index <- which(data_1$flag_dup_visit == 1)

data_1 <- data_1[-index, -6 ] ## remove dupes and flag_dup_visit column

data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(seq_id = c(1:n())) ## create a sequence for visits

data_1$Date <- mdy(data_1$Date)
data_1$day <- wday(data_1$Date)

## data_1$Card.Number <- as.numeric(data_1$Card.Number)

data_1$Show <- as.character(data_1$Show)
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v1 = ifelse(seq_id >= 1, lag(Show,max(seq_id)-1), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v2 = ifelse(seq_id >= 2, lag(Show,max(seq_id)-2), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v3 = ifelse(seq_id >= 3, lag(Show,max(seq_id)-3), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v4 = ifelse(seq_id >= 4, lag(Show,max(seq_id)-4), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v5 = ifelse(seq_id >= 5, lag(Show,max(seq_id)-5), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v6 = ifelse(seq_id >= 6, lag(Show,max(seq_id)-6), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v7 = ifelse(seq_id >= 7, lag(Show,max(seq_id)-7), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v8 = ifelse(seq_id >= 8, lag(Show,max(seq_id)-8), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v9 = ifelse(seq_id >= 9, lag(Show,max(seq_id)-9), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v10 = ifelse(seq_id >= 10, lag(Show,max(seq_id)-10), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v11 = ifelse(seq_id >= 11, lag(Show,max(seq_id)-11), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v12 = ifelse(seq_id >= 12, lag(Show,max(seq_id)-12), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v13 = ifelse(seq_id >= 13, lag(Show,max(seq_id)-13), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v14 = ifelse(seq_id >= 14, lag(Show,max(seq_id)-14), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v15 = ifelse(seq_id >= 15, lag(Show,max(seq_id)-15), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v16 = ifelse(seq_id >= 16, lag(Show,max(seq_id)-16), ""))
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(v17 = ifelse(seq_id >= 17, lag(Show,max(seq_id)-17), ""))

## filter again the unique card number and date combination
data_1 <- data_1 %>% group_by(Card.Number, Date) %>% mutate(max = ifelse(seq_id == max(seq_id), 1, 0))
data_1 <- data_1[data_1$max == 1, ]

data_1$flow <- paste(data_1$v1, data_1$v2, data_1$v3, data_1$v4, data_1$v5, data_1$v6, data_1$v7, data_1$v8, data_1$v9, data_1$v10, data_1$v11, data_1$v12, data_1$v13, data_1$v14, data_1$v15, data_1$v16, data_1$v17, sep = "-")

data <- data_1[, c("Card.Number", "Date", "flow", "day")]

a <- str_split_fixed(data$flow, "--", 2)

a <- as.data.frame(a)

data$flow <- a$V1
#data <- data[data$flow == 2,]
b <- as.data.frame(table(data$flow[data$day == 3]))
names(b)[1] <- "Flow"
datatable(b[order(-b$Freq), ])

write.csv(data, "C:/Users/Grejell/Documents/Data Analysis/InfoData/GFH-AFS 2017 Gate Scans_FINAL.csv")
