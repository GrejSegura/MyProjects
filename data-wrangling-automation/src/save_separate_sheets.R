gc()
rm(list = ls())
options(java.parameters = "-Xmx4g")  ## memory control in RJava 
library(xlsx)
library(XLConnect)

final2 <- read.csv("C:/Users/Grejell/Documents/Data Analysis/Automation Project/gessSessions/input/gessformatted_v2.csv")

sessions <- unique(final2$Session)
sessions <- sessions[order(sessions)]
sessions <- as.character(unlist(sessions))
index <- strsplit(sessions, "E")
index <- as.data.frame(index)
index <- t(index)
index <- as.data.frame(index)
index$sessions <- sessions
index$V2 <- as.numeric(as.character(unlist(index$V2)))
index <- index[order(index$V2),]

sessions <- index$sessions

for (i in sessions){
	
	data <- final2[final2$Session == i, ]
	
	write.xlsx(data, paste("C:/Users/Grejell/Documents/Data Analysis/Automation Project/gessSessions/output/data.xlsx"), sheetName = i, append = TRUE, row.names = FALSE)
	xlcFreeMemory()  ## memory control in excel - this prevents the error "Error in .jcall("RJavaTools", "Ljava/lang/Object;", "invokeMethod", cl,  : java.lang.OutOfMemoryError: GC overhead limit exceeded
}

library(readxl)
length(excel_sheets("C:/Users/Grejell/Documents/Data Analysis/Automation Project/gessSessions/output/data.xlsx")) ## to check if all are saved accordingly - match the number to the actual number of sessions/shows
