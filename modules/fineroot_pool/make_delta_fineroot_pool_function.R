make_delta_fineroot_pool_function <- function(inDF,var.col) {
    
    ### 
    inDF <- inDF[order(inDF$Date),]
    inDF$Date <- as.character(inDF$Date)
    
    ### Change column name of value variable
    colnames(inDF)[var.col] <- "Value"
    
    ### 
    subDF <- subset(inDF, Date%in%c("2014-02-18","2016-02-26"))

    ### date list
    d.list <- unique(subDF$Date)
    d.list <- d.list[order(d.list)]
    
    ### as. date
    subDF$Date <- as.Date(subDF$Date)

    ### create delta df
    delta <- subset(subDF, Date != d.list[1])
    delta$Start_date <- delta$Date  
    
    #### calculate differences
    for (i in 1:length(delta$Date)) {
        delta$Start_date[i] <- d.list[which(d.list == delta$Date[i]) - 1]
        delta$prev_biom[i] <- subDF$Value[subDF$Ring == delta$Ring[i] &
                                                as.numeric(subDF$Date-delta$Start_date[i])==0]
    }

    
    ### Length of period
    delta$length <- as.numeric(delta$Date - delta$Start_date)
    
    
    ### annualize the difference
    delta$diff_g_yr <- (delta$Value - delta$prev_biom) / delta$length * 365
    
    #- format dataframe to return
    out <- delta[,c("Start_date", "Date", "Date", "Ring", "diff_g_yr")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "delta")
    
    return(out)
}