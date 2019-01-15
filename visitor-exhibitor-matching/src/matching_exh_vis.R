

## PURPOSE : THIS IS TO IDENTIFY THE PEOPLE WHO WERE EXHIBITORS THIS YEAR THAT WERE VISITORS LAST YEAR
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
## LOAD THE 2018 DATA
exhData <- fread("./dta/2018exh.csv", colClasses = "character")
exh <- setDT(exhData)
str(exh)
names(exh)

exh[] <- lapply(exh[], function(x) gsub("[^A-z0-9]", "", x))  ## REMOVE NON-LETTER AND NON-INTEGER CHARACTERS

exh[] <- lapply(exh, tolower)
exh[] <- lapply(exh, function(x) gsub("^$", NA, x)) ## REPLACE BLANKS WITH NA

exhnames <- tolower(names(exh))
exhnames[] <- lapply(exhnames, function(x) gsub(" ", "", x)) ## REPLACE BLANKS WITH NA
exhnames <- as.vector(unlist(exhnames))
names(exh) <- exhnames

naCount <- rowSums(is.na(exh)) # count NAs per row
exh <- setDT(exh[naCount < 5, ]) # remove rows that has > 4 entries
exh[is.na(exh)] <- ""   # replace NAs with blanks

####### ####### ####### ####### ####### ####### ####### ####### ####### ####### 
## LOAD THE 2017 DATA
visData <- fread("./dta/2017vis.csv", colClasses = "character")
vis <- setDT(visData)
str(vis)


vis[] <- lapply(vis[], function(x) gsub("[^A-z0-9]", "", x))  ## REMOVE NON-LETTER AND NON-INTEGER CHARACTERS

vis[] <- lapply(vis, tolower)
vis[] <- lapply(vis, function(x) gsub("^$", NA, x)) ## REPLACE BLANKS WITH NA

visnames <- tolower(names(vis))
visnames[] <- lapply(visnames, function(x) gsub(" ", "", x)) ## REPLACE BLANKS WITH NA
visnames <- as.vector(unlist(visnames))
names(vis) <- visnames

naCount <- rowSums(is.na(vis)) # count NAs per row
vis <- setDT(vis[naCount < 5, ]) # remove rows that has > 4 entries
vis[is.na(vis)] <- ""  # replace NAs with blanks


####### ####### ####### ####### ####### ####### ####### ####### ####### ####### 
## create permutation / combination of names
exhnames <- names(exh[, -1])
listcombi <- data.frame()
for (i in c(1:length(exhnames))) {
	combi <- combn(exhnames, m = i)
	combi <- as.data.frame(combi)
	combi <- as.data.frame(t(combi))
	listcombi <- smartbind(listcombi, combi)
	listcombi <- unique(listcombi)
}

listcombi <- listcombi[-c(1:4),] ## remove single combination except for email

listcombi <- listcombi[-c(3,4,5,8,9,12),]

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

concatenate(data = exh)
concatenate(data = vis)

exh <- exh[, -c(2:7)]
vis <- vis[, -c(2:7)]
names(exh)[1] <- "id"
names(vis)[1] <- "id"


## MERGE VIS AND EXH

allnames <- names(exh[, -1])


mergeData <- data.frame()

for (i in allnames) {
	
	cols <- c("id", i)
	
	x <- exh[, cols, with = FALSE]
	x <- unique(x)
	x$n <- nchar(x[, 2])
	x <- x[x$n > 7, ]
	x <- x[, -3]
	
	y <- vis[, cols, with = FALSE]
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

names(exhData)[1] <- "id"


mergeData <- merge(mergeData, exhData, by = "id", all.y = TRUE)
mergeData <- setDT(mergeData)
mergeData[, seq := seq(.N), by = id]
mergeData <- mergeData[seq == 1, ]
mergeData[is.na(mergeData), ] <- "NF"

fwrite(mergeData, "./dta/finalData.csv")
