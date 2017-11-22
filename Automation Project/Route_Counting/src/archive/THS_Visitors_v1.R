

data <- read.csv("C:/Users/Grejell/Documents/Data Analysis/InfoData/GFH-AFS 2017 Gate Scans_FINAL.csv")

ths_2nd <- data$Card.Number[data$day == 3 & data$flow == "Crossover into Hotel / Leisure"]
# 1066 guests

ths_1st <- data[(data$Card.Number %in% ths_2nd),]
ths_1st <- ths_1st[ths_1st$day == 2,]
# 163 guests

ths_3rd <- data[(data$Card.Number %in% ths_2nd),]
ths_3rd <- ths_3rd[ths_3rd$day == 4,]
# 239 guests