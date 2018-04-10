

## PURPOSE : THIS IS TO IDENTIFY THE PEOPLE WHO WERE ALSO VISITORA IN 2017 AND 2016 GULFOOD SHOWS
## CREATION DATE : 3/3/2018
## LAST UPDATE : 
## VERSION : 1
## AUTHOR : GREJELL SEGURA


## CHECK IF PACKAGES REQUIRED WERE INSTALLED IN R
rm(list = ls())
depends <- c("tidyverse", "data.table")
pkgs <- rownames(installed.packages())

for(d in depends){
	if(!(d %in% pkgs)){
		install.packages(d)
	}
}

library(tidyverse)
library(data.table)
library(combinat)
library(gtools)

####### ####### ####### ####### ####### ####### ####### ####### ####### ####### 
## LOAD THE GULFOOD 2018 VISITOR DATA
vis2018Data <- fread("./dta/2018vis.csv", colClasses = "character")
vis2018 <- setDT(vis2018Data)
str(vis2018)
names(vis2018)

vis2018[] <- lapply(vis2018[], function(x) gsub("[^A-z0-9]", "", x))  ## REMOVE NON-LETTER AND NON-INTEGER CHARACTERS

vis2018[] <- lapply(vis2018, tolower)
vis2018[] <- lapply(vis2018, function(x) gsub("^$", NA, x)) ## REPLACE BLANKS WITH NA

vis2018names <- tolower(names(vis2018))
vis2018names[] <- lapply(vis2018names, function(x) gsub(" ", "", x)) ## REPLACE BLANKS WITH NA
vis2018names <- as.vector(unlist(vis2018names))
names(vis2018) <- vis2018names

naCount <- rowSums(is.na(vis2018)) # count NAs per row
vis2018 <- setDT(vis2018[naCount < 5, ]) # remove rows that has > 4 entries
vis2018[is.na(vis2018)] <- ""   # replace NAs with blanks

####### ####### ####### ####### ####### ####### ####### ####### ####### ####### 
## LOAD THE GULFOOD 2017 VISITOR DATA
vis2017Data <- fread("./dta/2017vis.csv", colClasses = "character")
vis2017 <- setDT(vis2017Data)
str(vis2017)


vis2017[] <- lapply(vis2017[], function(x) gsub("[^A-z0-9]", "", x))  ## REMOVE NON-LETTER AND NON-INTEGER CHARACTERS

vis2017[] <- lapply(vis2017, tolower)
vis2017[] <- lapply(vis2017, function(x) gsub("^$", NA, x)) ## REPLACE BLANKS WITH NA

vis2017names <- tolower(names(vis2017))
vis2017names[] <- lapply(vis2017names, function(x) gsub(" ", "", x)) ## REPLACE BLANKS WITH NA
vis2017names <- as.vector(unlist(vis2017names))
names(vis2017names) <- vis2017names

naCount <- rowSums(is.na(vis2017)) # count NAs per row
vis2017 <- setDT(vis2017[naCount < 5, ]) # remove rows that has > 4 entries
vis2017[is.na(vis2017)] <- ""  # replace NAs with blanks


####### ####### ####### ####### ####### ####### ####### ####### ####### ####### 
## create permutation / combination of names
vis2018names <- names(vis2018[, -1])
listcombi <- data.frame()
for (i in c(1:length(vis2018names))) {
	combi <- combn(vis2018names, m = i)
	combi <- as.data.frame(combi)
	combi <- as.data.frame(t(combi))
	listcombi <- smartbind(listcombi, combi)
	listcombi <- unique(listcombi)
}

listcombi <- listcombi[-c(1:4),] ## remove single combination except for email

listcombi <- listcombi[-c(4:59),]
listcombi$na <- rowSums(is.na(listcombi))
listcombi <- listcombi[with(listcombi, order(na)),]
listcombi <- listcombi[, -7]

####### ####### ####### ####### ####### ####### ####### ####### ####### ####### 
## create a concatenate function ##
concatenate <- function(data){
	for (j in (1:nrow(listcombi))){
		c <- unname(unlist(listcombi[j, ]))
		c <- c[!is.na(c)]
		combiname <- paste("combi", j, collapse = "")
		data[, (combiname) := apply(data[, c, with = FALSE], 1, paste, collapse = ""), with = FALSE]
	}
}

concatenate(data = vis2018)
concatenate(data = vis2017)

vis2018 <- vis2018[, -c(2:8)]
vis2017 <- vis2017[, -c(2:8)]
names(vis2018)[1] <- "id"
names(vis2017)[1] <- "id"


## MERGE VIS AND vis2018

allnames <- names(vis2018[, -1])


mergeData <- data.frame()

for (i in allnames) {
	
	cols <- c("id", i)
	
	x <- vis2018[, cols, with = FALSE]
	x <- unique(x)
	x$n <- nchar(x[, 2])
	x <- x[x$n > 7, ]
	x <- x[, -3]
	
	y <- vis2017[, cols, with = FALSE]
	y <- unique(y)
	y$n <- nchar(y[, 2])
	y <- y[y$n > 7, ]
	y <- y[, -3]
	
	z <- merge(x, y, by = i)
	names(z)[1] <- "combi"
	
	mergeData <- rbind(mergeData, z)
}

mergeData <- mergeData[mergeData$combi != "", ]
names(mergeData)[c(2,3)] <- c("id", "visitor ID 2017")

## merge with visitor data ##

names(vis2018Data)[1] <- "id"


mergeData <- merge(mergeData, vis2018Data, by = "id", all.y = TRUE)
mergeData <- setDT(mergeData)
mergeData[, seq := seq(.N), by = id]
mergeData <- mergeData[seq == 1, ]
mergeData[is.na(mergeData), ] <- "NF"

fwrite(mergeData, "./dta/2017_2018finalData.csv")


###########################################################################
###########################################################################
###########################################################################

## MATCHING MERGEDATA(2017 AND 2018) TO VISITOR DATA OF 2016 ##

vis1718Data <- fread("./dta/2017_2018finalData.csv", colClasses = "character")
vis1718Data <- vis1718Data[vis1718Data$`visitor ID 2017` != "NF", ]
vis1718 <- vis1718Data[, -c(2,3,11)]

vis1718[] <- lapply(vis1718[], function(x) gsub("[^A-z0-9]", "", x))  ## REMOVE NON-LETTER AND NON-INTEGER CHARACTERS

vis1718[] <- lapply(vis1718, tolower)
vis1718[] <- lapply(vis1718, function(x) gsub("^$", NA, x)) ## REPLACE BLANKS WITH NA

vis1718names <- tolower(names(vis1718))
vis1718names[] <- lapply(vis1718names, function(x) gsub(" ", "", x)) ## REPLACE BLANKS WITH NA
vis1718names <- as.vector(unlist(vis1718names))
names(vis1718) <- vis1718names

naCount <- rowSums(is.na(vis1718)) # count NAs per row
vis1718 <- setDT(vis1718[naCount < 5, ]) # remove rows that has > 4 entries
vis1718[is.na(vis1718)] <- ""   # replace NAs with blanks


## LOAD THE GULFOOD 2016 VISITOR DATA
vis2016Data <- fread("./dta/2016vis.csv", colClasses = "character")
vis2016 <- setDT(vis2016Data)
str(vis2016)
names(vis2016)

vis2016[] <- lapply(vis2016[], function(x) gsub("[^A-z0-9]", "", x))  ## REMOVE NON-LETTER AND NON-INTEGER CHARACTERS

vis2016[] <- lapply(vis2016, tolower)
vis2016[] <- lapply(vis2016, function(x) gsub("^$", NA, x)) ## REPLACE BLANKS WITH NA

vis2016names <- tolower(names(vis2016))
vis2016names[] <- lapply(vis2016names, function(x) gsub(" ", "", x)) ## REPLACE BLANKS WITH NA
vis2016names <- as.vector(unlist(vis2016names))
names(vis2016) <- vis2016names

naCount <- rowSums(is.na(vis2016)) # count NAs per row
vis2016 <- setDT(vis2016[naCount < 5, ]) # remove rows that has > 4 entries
vis2016[is.na(vis2016)] <- ""   # replace NAs with blanks


####### ####### ####### ####### ####### ####### ####### ####### ####### ####### 
## create permutation / combination of names
'vis2016names <- names(vis2016[, -1])
listcombi <- data.frame()
for (i in c(1:length(vis2016names))) {
	combi <- combn(vis2016names, m = i)
	combi <- as.data.frame(combi)
	combi <- as.data.frame(t(combi))
	listcombi <- smartbind(listcombi, combi)
	listcombi <- unique(listcombi)
}

listcombi <- listcombi[-c(1:4),] ## remove single combination except for email

listcombi <- listcombi[-c(4:59),]
listcombi$na <- rowSums(is.na(listcombi))
listcombi <- listcombi[with(listcombi, order(na)),]
listcombi <- listcombi[, -7]
'
################### ################# ################
concatenate <- function(data){
	for (j in (1:nrow(listcombi))){
		c <- unname(unlist(listcombi[j, ]))
		c <- c[!is.na(c)]
		combiname <- paste("combi", j, collapse = "")
		data[, (combiname) := apply(data[, c, with = FALSE], 1, paste, collapse = ""), with = FALSE]
	}
}



concatenate(data = vis1718)
concatenate(data = vis2016)

vis1718 <- vis1718[, -c(2:8)]
vis2016 <- vis2016[, -c(2:8)]


names(vis1718)[1] <- "id"
names(vis2016)[1] <- "id"


## MERGE VIS AND vis2018

allnames <- names(vis1718[, -1])


mergeData2 <- data.frame()

for (i in allnames) {
	
	cols <- c("id", i)
	
	x <- vis1718[, cols, with = FALSE]
	x <- unique(x)
	x$n <- nchar(x[, 2])
	x <- x[x$n > 7, ]
	x <- x[, -3]
	
	y <- vis2016[, cols, with = FALSE]
	y <- unique(y)
	y$n <- nchar(y[, 2])
	y <- y[y$n > 7, ]
	y <- y[, -3]
	
	z <- merge(x, y, by = i)
	names(z)[1] <- "combi"
	
	mergeData2 <- rbind(mergeData2, z)
}

mergeData2 <- mergeData2[mergeData2$combi != "", ]
names(mergeData2)[c(2,3)] <- c("id", "visitor ID 2016")

## merge with visitor data ##


vis1718Data <- vis1718Data[, -c(2,3,11)]


mergeData2 <- merge(mergeData2, vis1718Data, by = "id", all.y = TRUE)
mergeData2 <- setDT(mergeData2)
mergeData2[, seq := seq(.N), by = id]
mergeData2 <- mergeData2[seq == 1, ]
mergeData2[is.na(mergeData2), ] <- "NF"


mergeData2[, seq := seq(.N), by = `visitor ID 2016`]
mergeData2 <- mergeData2[seq == 1, ]


fwrite(mergeData2, "./dta/finalData.csv")
