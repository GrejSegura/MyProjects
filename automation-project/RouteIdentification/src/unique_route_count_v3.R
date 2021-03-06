

## PURPOSE : THIS SCRIPT WILL IDENTIFY THE ROUTE OF THE VISITORS IN A SHOW
## DATE : 10/16/2017
## AUTHOR : GREJELL SEGURA


## CHECK IF PACKAGES WERE REQUIRED INSTALLED IN R
depends <- c("tidyverse", "data.table", "dplyr", "stringr", "lubridate", "magrittr", "DT")
pkgs <- rownames(installed.packages())

for(d in depends){
	if(!(d %in% pkgs)){
		install.packages(d)
	}
}


library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(magrittr)
library(DT)
set.seed(123)

data_1 <- read.csv("./dta/inputData.csv")


data_1 <- data_1[data_1$Len == 10,] ## remove Length of Key != 10
data_1 <- data_1[, -2] ## remove Len


ind_char <- which((grepl("[^0-9]", data_1$Key))) ## identify the non-digits characters

data_1 <- setDT(data_1[-ind_char, ]) ## remove Keys with non-digits character
data_1$Key <- as.character(data_1$Key)


data_1 <- data_1[order(data_1$Key, data_1$Date, data_1$Time),] ## sort by Key, date, time

data_1 <- data_1 %>% group_by(Key, Date) %>% mutate(flag_dup_visit = ifelse(Show == lag(Show), 1, 0))

## remove dupe visits -- if an ID visits >2 halls in succession in a given day
index <- which(data_1$flag_dup_visit == 1)
data_1 <- data_1[-index, -6 ] ## remove dupes and flag_dup_visit column

data_1 <- data_1 %>% group_by(Key, Date) %>% mutate(seq_id = c(1:n())) ## create a sequence for visits

data_1$Date <- mdy(data_1$Date) ## date formatting
data_1$day <- wday(data_1$Date) ## generates 2,3,4 which means 2 = monday, 3 = tue, 3 = wed


data_1$Show <- as.character(data_1$Show)

data_1 <- setDT(data_1)
k <- length(names(data_1)) + 1  ## used as index to create the variable 'flow'

## CREATE VARIABLES BY LOOPING 
varnames <- paste("V", 1:max(data_1$seq_id), sep = "")
n <- 1:max(data_1$seq_id)

for (i in n){
  
  data_1[seq_id >= i, varnames[i] := lag(Show, max(seq_id) - i), by = .(Key, Date)]
  data_1[seq_id <= i, varnames[i] := "", by = .(Key, Date)]
  data_1[seq_id == i, varnames[i] := Show, by = .(Key, Date)]
}


## IDENTIFY THE MAXIMUM NUMBER OF VISITS PER ID
data_1 <- data_1 %>% group_by(Key, Date) %>% mutate(flag_max = ifelse(seq_id == max(seq_id), 1, 0))
data_1 <- setDT(data_1)
data_1 <- data_1[flag_max == 1, -c("flag_max")]


## paste the variables/stops to create the flow ##
l <- length(names(data_1))  ## used as index to create the variable 'flow'
varnames_2 <- names(data_1)
data_1$flow <- apply(data_1[, k:l], 1, paste, collapse = "-")


## remove the unwanted variables ##
data <- data_1[, c("Key", "Date", "flow", "Date")]

## formatting the 'flow' variable
a <- str_split_fixed(data$flow, "--", 2)
a <- as.data.frame(a)
data$flow <- a$V1

## FOR TABLE VIEWING ##
b <- as.data.frame(table(data$flow)) # <----- CHANGE DAY
names(b)[1] <- "Flow"
datatable(b[order(-b$Freq), ])

## SAVE OUTPUT DATA TO EXCEL ##
write.csv(data, "./dta/finalData.csv", row.names = FALSE)
