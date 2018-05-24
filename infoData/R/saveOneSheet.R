
#' @title Creates a session data for Knect that will be saved in MS Excel as one sheet.
#'
#' @description This function will create a session data and will be saved in excel as one sheet. This should only be used by onsite managers doing a report for Knect.
#'
#' @param csvFileName
#'
#' @param fileLocation
#'
#' @export saveOneSheet
#'
#' @import data.table


saveOneSheet <- function(csvFileName, fileLocation){
	options(warn = -1)
	setwd(fileLocation)

	data <- read.csv(csvFileName)
	data$date <- lubridate::dmy(data$date)
	data$time <- lubridate::hms(data$time)
	data <- data.table::setDT(data)
	#data <- data[order(id, session, date, time, type_of_log),]

	data$uniq <- apply(data[,c("id","session","date")], 1, paste, collapse = "")
	data2 <- data[, c('uniq','time','type_of_log')]
	data2$time <- as.character(data2$time)

	adata <- data.table::setDT(data2[data2$type_of_log == "IN",])
	adata <- adata[order(uniq, time),]
	names(adata)[2] <- "IN"
	adata <- adata[, seq := seq(.N), by = uniq]
	#adata <- adata %>% group_by(uniq) %>% mutate(seq = c(1:n()))
	#adata <- as.data.frame(adata)
	adata <- adata[adata$seq == 1,]
	#return(names(adata)) #####
	adata <- adata[, c('type_of_log', 'seq') := NULL,]


	bdata <- data.table::setDT(data2[data2$type_of_log == "OUT",])
	bdata <- bdata[order(uniq, time),]
	names(bdata)[2] <- "OUT"
	bdata <- bdata[, seq := seq(.N), by = uniq]
	#bdata <- bdata %>% group_by(uniq) %>% mutate(seq = c(1:n()))
	#bdata <- data.table::setDT(bdata)
	bdata[, max := ifelse(seq == max(seq), 1, 0), by = uniq]
	bdata <- bdata[max == 1,]
	bdata <- bdata[, c('max', 'type_of_log', 'seq') := NULL,]

	final <- merge(adata, bdata, by = "uniq", all = TRUE)
	final <- merge(final, data, by = "uniq", all = TRUE)
	final <- final[,-c('uniq', 'time', 'type_of_log')]
	final <- unique(final)
	final2 <- final
	final2$IN <- lubridate::hms(final2$IN)
	final2$OUT <- lubridate::hms(final2$OUT)

	final2$secondsDifference <- as.numeric(final2$OUT)-as.numeric(final2$IN)
	final2$minuteDifference <- final2$secondsDifference/60
	final2$hourDifference <- final2$minuteDifference/60
	final2 <- final2[order(final2$session),]
	xlsx::write.xlsx(final2, "./session_data.xlsx")
}
