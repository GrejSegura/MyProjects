
## PURPOSE : MATCHING VISITORS
## CREATION DATE : 10/16/2017
## LAST UPDATE : 
## VERSION : 1
## AUTHOR : GREJELL SEGURA


## CHECK IF PACKAGES WERE REQUIRED INSTALLED IN R
depends <- c("tidyverse", "data.table")
pkgs <- rownames(installed.packages())

for(d in depends){
	if(!(d %in% pkgs)){
		install.packages(d)
	}
}

library(tidyverse)
library(data.table)


## LOAD THE REFERENCE DATA
dbData_1 <- read.csv("./dta/inputData.csv", sep = ",")
dbData_1 <- setDT(dbData_1)

dbmsnames <- grep("Attended", names(dbData_1), value = TRUE)
dbData_2 <- dbData_1[, c("name", "surname", "company", "position", "email", "id", dbmsnames), with = FALSE]  ## RETAIN ONLY c("id", "name", "surname", "company", "position", "email", dbmsnames)
dbData_2[] <- lapply(dbData_2, as.character)
dbData_2[] <- lapply(dbData_2, function(x) gsub("[^A-z0-9]", "", x))  ## REMOVE NON-LETTER AND NON-INTEGER CHARACTERS
dbData_2[] <- lapply(dbData_2, tolower)
dbData_2[] <- lapply(dbData_2, function(x) gsub("^$", NA, x)) ## REPLACE BLANKS WITH NA
## IDENTIFY ATTENDANCE ##
dbData_2[, attended := apply(dbData_2[ , dbmsnames, with = FALSE] , 1 , paste , collapse = ""), ] ## PASTE ATTENDANCE COLUMNS
dbData_2$grepll <- grepl("[td]", dbData_2$attended)
dbData_2[grepll == TRUE, attended := "Yes"]
dbData_2[grepll == FALSE, attended := "No"]
dbData_2 <- dbData_2[, c("name", "surname", "company", "position", "email", "id", "attended")]

## LOAD AND PROCESS MAIN DATA 1
igData_1a <- fread("./dta/input_1Data.csv", sep = ",")
igData_1b <- igData_1a[, c("name", "surname", "company", "position", "email")]  ## RETAIN ONLY c("name", "surname", "company", "position", "email")
igData_1b[] <- lapply(igData_1b, as.character)
igData_1b[] <- lapply(igData_1b, function(x) gsub("[^A-z0-9]", "", x))
igData_1b[] <- lapply(igData_1b, tolower)

igData_1b$source <- "acquisition"


## LOAD AND PROCESS MAIN DATA 2
igData_2a <- fread("./dta/input_2Data.csv", sep = ",")
igData_2b <- igData_2a[, c("name", "surname", "company", "position", "email")]  ## RETAIN ONLY c("name", "surname", "company", "position", "email")
igData_2b[] <- lapply(igData_2b, as.character)
igData_2b[] <- lapply(igData_2b, function(x) gsub("[^A-z0-9]", "", x))
igData_2b[] <- lapply(igData_2b, tolower)

igData_2b$source <- "advocates"


igData <- rbind(igData_1b, igData_2b)
igData[] <- lapply(igData, function(x) gsub("^$", NA, x))
n <- length(igData) + 1 ## used as starting value in the loop (line 80 - line 83)

## CREATE CONCATENATES FOR UNIQUE IDENTIFICATION

igData[, combi1 := apply(igData[ , c(1:5)] , 1 , paste , collapse = ""), ]
igData[, combi2 := apply(igData[ , c(1,2,3,5)] , 1 , paste , collapse = ""), ]
igData[, combi3 := apply(igData[ , c(1,2,4,5)], 1 , paste , collapse = ""), ]
igData[, combi4 := apply(igData[ , c(1,2,5)] , 1 , paste , collapse = ""), ]
igData[, combi5 := apply(igData[ , c(1,2,3)] , 1 , paste , collapse = ""), ]
igData[, combi6 := apply(igData[ , c(3,4,5)] , 1 , paste , collapse = ""), ]
igData[, combi7 := apply(igData[ , c(1,2,4)] , 1 , paste , collapse = ""), ]
igData[, combi8 := email, ]
igData[, combi9 := apply(igData[ , c(3,4)] , 1 , paste , collapse = ""), ]
igData[, combi10 := apply(igData[ , c(1,2)] , 1 , paste , collapse = ""), ]
igData[, combi11 := apply(igData[ , c(3,5)] , 1 , paste , collapse = ""), ]



## dbms Data
dbData_2[, combi1 := apply(dbData_2[ , c(1:5)] , 1 , paste , collapse = ""), ]
dbData_2[, combi2 := apply(dbData_2[ , c(1,2,3,5)] , 1 , paste , collapse = ""), ]
dbData_2[, combi3 := apply(dbData_2[ , c(1,2,4,5)], 1 , paste , collapse = ""), ]
dbData_2[, combi4 := apply(dbData_2[ , c(1,2,5)] , 1 , paste , collapse = ""), ]
dbData_2[, combi5 := apply(dbData_2[ , c(1,2,3)] , 1 , paste , collapse = ""), ]
dbData_2[, combi6 := apply(dbData_2[ , c(3,4,5)] , 1 , paste , collapse = ""), ]
dbData_2[, combi7 := apply(dbData_2[ , c(1,2,4)] , 1 , paste , collapse = ""), ]
dbData_2[, combi8 := email, ]
dbData_2[, combi9 := apply(dbData_2[ , c(3,4)] , 1 , paste , collapse = ""), ]
dbData_2[, combi10 := apply(dbData_2[ , c(1,2)] , 1 , paste , collapse = ""), ]
dbData_2[, combi11 := apply(dbData_2[ , c(3,5)] , 1 , paste , collapse = ""), ]


## REPLACE COMBI WITH "NA" WITH NA ##
igData$grepll <- grepl("[NA]", igData$combi1)
igData[grepll == TRUE, combi1 := NA]

igData$grepll <- grepl("[NA]", igData$combi2)
igData[grepll == TRUE, combi2 := NA]

igData$grepll <- grepl("[NA]", igData$combi3)
igData[grepll == TRUE, combi3 := NA]

igData$grepll <- grepl("[NA]", igData$combi4)
igData[grepll == TRUE, combi4 := NA]

igData$grepll <- grepl("[NA]", igData$combi5)
igData[grepll == TRUE, combi5 := NA]

igData$grepll <- grepl("[NA]", igData$combi6)
igData[grepll == TRUE, combi6 := NA]

igData$grepll <- grepl("[NA]", igData$combi7)
igData[grepll == TRUE, combi7 := NA]

igData$grepll <- grepl("[NA]", igData$combi8)
igData[grepll == TRUE, combi8 := NA]

igData$grepll <- grepl("[NA]", igData$combi9)
igData[grepll == TRUE, combi9 := NA]

igData$grepll <- grepl("[NA]", igData$combi10)
igData[grepll == TRUE, combi10 := NA]

igData$grepll <- grepl("[NA]", igData$combi11)
igData[grepll == TRUE, combi11 := NA]

igData <- igData[, -c("grepll")]

	
## combi1
x <- igData[, "combi1"]
x <- unique(x)
y <- unique(dbData_2[ , c("id", "combi1")])
y[, seq := seq(.N), by = combi1]
y <- y[seq == 1, ]
z <- merge(x, y, by = "combi1")
z <- unique(z[combi1 != " ", c("combi1", "id")])

igData <- merge(igData, z, by = "combi1", all.x = TRUE)
igData$id1 <- igData$id
igData <- igData[, -c("combi1", "id")]


## combi2
x <- igData[, "combi2"]
x <- unique(x)
y <- unique(dbData_2[ , c("id", "combi2")])
y[, seq := seq(.N), by = combi2]
y <- y[seq == 1, ]
z <- merge(x, y, by = ("combi2"))
z <- unique(z[combi2 != " ", c("combi2", "id")])

igData <- merge(igData, z, by = ("combi2"), all.x = TRUE)
igData$id2 <- igData$id
igData <- igData[, -c("combi2", "id")]



## combi3
x <- igData[, "combi3"]
x <- unique(x)
y <- unique(dbData_2[ , c("id", "combi3")])
y[, seq := seq(.N), by = combi3]
y <- y[seq == 1, ]
z <- merge(x, y, by = ("combi3"))
z <- unique(z[combi3 != " ", c("combi3", "id")])

igData <- merge(igData, z, by = ("combi3"), all.x = TRUE)
igData$id3 <- igData$id
igData <- igData[, -c("combi3", "id")]


## combi4
x <- igData[, "combi4"]
x <- unique(x)
y <- unique(dbData_2[ , c("id", "combi4")])
y[, seq := seq(.N), by = combi4]
y <- y[seq == 1, ]
z <- merge(x, y, by = ("combi4"))
z <- unique(z[combi4 != " ", c("combi4", "id")])

igData <- merge(igData, z, by = ("combi4"), all.x = TRUE)
igData$id4 <- igData$id
igData <- igData[, -c("combi4", "id")]

## combi5
x <- igData[, "combi5"]
x <- unique(x)
y <- unique(dbData_2[ , c("id", "combi5")])
y[, seq := seq(.N), by = combi5]
y <- y[seq == 1, ]
z <- merge(x, y, by = ("combi5"))
z <- unique(z[combi5 != " ", c("combi5", "id")])

igData <- merge(igData, z, by = ("combi5"), all.x = TRUE)
igData$id5 <- igData$id
igData <- igData[, -c("combi5", "id")]


## combi6
x <- igData[, "combi6"]
x <- unique(x)
y <- unique(dbData_2[ , c("id", "combi6")])
y[, seq := seq(.N), by = combi6]
y <- y[seq == 1, ]
z <- merge(x, y, by = ("combi6"))
z <- unique(z[combi6 != " ", c("combi6", "id")])

igData <- merge(igData, z, by = ("combi6"), all.x = TRUE)
igData$id6 <- igData$id
igData <- igData[, -c("combi6", "id")]


## combi7
x <- igData
y <- unique(dbData_2[ , c("id", "combi7")])
y[, seq := seq(.N), by = combi7]
y <- y[seq == 1, ]
z <- merge(x, y, by = ("combi7"))
z <- unique(z[combi7 != " ", c("combi7", "id")])

igData <- merge(igData, z, by = ("combi7"), all.x = TRUE)
igData$id7 <- igData$id
igData <- igData[, -c("combi7", "id")]

## combi8
x <- igData
y <- unique(dbData_2[ , c("id", "combi8")])
y[, seq := seq(.N), by = combi8]
y <- y[seq == 1, ]
z <- merge(x, y, by = ("combi8"))
z <- unique(z[combi8 != " ", c("combi8", "id")])

igData <- merge(igData, z, by = ("combi8"), all.x = TRUE)
igData$id8 <- igData$id
igData <- igData[, -c("combi8", "id")]


## combi9
x <- igData
y <- unique(dbData_2[ , c("id", "combi9")])
y[, seq := seq(.N), by = combi9]
y <- y[seq == 1, ]
z <- merge(x, y, by = ("combi9"))
z <- unique(z[combi9 != " ", c("combi9", "id")])

igData <- merge(igData, z, by = ("combi9"), all.x = TRUE)
igData$id9 <- igData$id
igData <- igData[, -c("combi9", "id")]



## combi10
x <- igData
y <- unique(dbData_2[ , c("id", "combi10")])
y[, seq := seq(.N), by = combi10]
y <- y[seq == 1, ]
z <- merge(x, y, by = ("combi10"))
z <- unique(z[combi10 != " ", c("combi10", "id")])

igData <- merge(igData, z, by = ("combi10"), all.x = TRUE)
igData$id10 <- igData$id
igData <- igData[, -c("combi10", "id")]



## combi11
x <- igData
y <- unique(dbData_2[ , c("id", "combi11")])
y[, seq := seq(.N), by = combi11]
y <- y[seq == 1, ]
z <- merge(x, y, by = ("combi11"))
z <- unique(z[combi11 != " ", c("combi11", "id")])

igData <- merge(igData, z, by = ("combi11"), all.x = TRUE)
igData$id11 <- igData$id
igData <- igData[, -c("combi11", "id")]



######################################################################


## CONDITIONS AND RESTRICTIONS ##
igData_3 <- igData

## for ids

igData$id <- ifelse(!(is.na(igData$id1)), igData$id1,
			ifelse(!(is.na(igData$id2)), igData$id2,
			       ifelse(!(is.na(igData$id3)), igData$id3,
			              ifelse(!(is.na(igData$id4)), igData$id4,
			                     ifelse(!(is.na(igData$id5)), igData$id5,
			                            ifelse(!(is.na(igData$id6)), igData$id6,
			                                   ifelse(!(is.na(igData$id7)), igData$id7,
			                                          ifelse(!(is.na(igData$id8)), igData$id8,
			                                                 ifelse(!(is.na(igData$id9)), igData$id9,
			                                                        ifelse(!(is.na(igData$id10)), igData$id10,
			                                                               ifelse(!(is.na(igData$id11)), igData$id11, "No Match")
			                                                               )
			                                                        )
			                                                 )
			                                          )
			                                   )
			                            )
			                     )
			              )
			       )
			)

igData <- igData[,  c("name", "surname", "company", "position", "email", "source", "id")]


## CREATE A TABLE FOR ATTENDANCE AND ID ##
db <- unique(dbData_2[, c("id", "attended")])
db[, seq := seq(.N), by = id]
db <- db[seq == 1, ]
db <- db[, c("id", "attended")]


## MERGE TO IDENTIFY THE ATTENDEES ##

igDataFinal <- merge(igData, db, by = "id", all.x = TRUE)

write.csv(igDataFinal ,"./dta/matchedData.csv", row.names = FALSE)
rm(list = ls())
