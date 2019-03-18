

save_on_one_sheet <- function(data){
    data$date <- dmy(data$date)
    data$time <- hms(data$time)
    data <- setDT(data)
    #data <- data[order(id, session, date, time, type_of_log),]


    data$uniq <- apply(data[,c("id","session","date")], 1, paste, collapse= "")
    data2 <- data[, -c(1,2,3)]
    data2$time <- as.character(data2$time)

    adata <- data2[data2$type_of_log == "IN",]
    adata <- adata[order(uniq, time),]
    names(adata)[1] <- "IN"
    adata <- adata %>% group_by(uniq) %>% mutate(seq = c(1:n()))
    adata <- as.data.frame(adata)
    adata <- adata[adata$seq == 1,]
    adata <- adata[, -c(2,4)]


    bdata <- data2[data2$type_of_log == "OUT",-2]
    bdata <- bdata[order(uniq, time),]
    names(bdata)[1] <- "OUT"
    bdata <- bdata %>% group_by(uniq) %>% mutate(seq = c(1:n()))
    bdata <- setDT(bdata)
    bdata[,max := ifelse(seq == max(seq), 1, 0), by = uniq]
    bdata <- bdata[max == 1,]
    bdata <- bdata[, -c(3,4)]

    final <- merge(adata, bdata, by = "uniq", all = TRUE)
    final <- merge(final, data, by = "uniq", all = TRUE)

    final <- final[,-c(1,7,8)]
    final <- unique(final)
    final2 <- final
    final2$IN <- hms(final2$IN)
    final2$OUT <- hms(final2$OUT)


    final2$secondsDifference <- as.numeric(final2$OUT)-as.numeric(final2$IN)
    final2$minuteDifference <- final2$secondsDifference/60
    final2$hourDifference <- final2$minuteDifference/60
    final2 <- final2[order(final2$session),]
}
