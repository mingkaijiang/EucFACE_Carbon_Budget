make_delta_coarseroot_pool_function <- function(inDF,var.col) {
    
    ### extract start and end date
    s.date <- min(inDF$Date)
    e.date <- max(inDF$Date)
    
    ### Change column name of value variable
    colnames(inDF)[var.col] <- "Value"
    
    inDF$Date <- as.character(inDF$Date)
    eDF <- subset(inDF, Date == "2014-09-22")
    lDF <- subset(inDF, Date == "2015-09-23")
    
    eDF$late <- lDF$Value
    eDF$ndays <- as.numeric(as.Date("2015-09-23") - as.Date("2014-09-22")) + 1
    eDF$diff_g_yr <- (eDF$late - eDF$Value ) / eDF$ndays * 365
    
    # format dataframe to return
    out <- eDF[,c("Date", "Date", "Ring", "diff_g_yr")]
    names(out) <- c("Start_date", "Date", "Ring", "delta")
    out$End_date <- as.Date("2015-09-23")

    out <- out[,c("Start_date", "End_date", "Date", "Ring", "delta")]
    
    return(out)
}