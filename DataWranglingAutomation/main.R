gc()
rm(list = ls())
options(java.parameters = "-Xmx4g")  ## memory control in RJava 
library(tidyverse)
library(lubridate)
library(data.table)
library(xlsx)
library(XLConnect)

fileDirectory <- winDialogString("Please enter the file path below.", "")
setwd(fileDirectory)

source("./src/save_one_sheet.R")

data <- read.csv(file.choose())

options(warn = -1)
data <- save_on_one_sheet(data)
options(warn = 0)

sessions <- unique(data$session)
write.xlsx(data, "./output/final_output.xlsx")


