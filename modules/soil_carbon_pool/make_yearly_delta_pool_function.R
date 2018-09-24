make_yearly_delta_pool_function <- function(inDF,var.col) {
    
    ### extract start and end date
    s.date <- min(inDF$Date)
    e.date <- max(inDF$Date)
    
    ### Change column name of value variable
    colnames(inDF)[var.col] <- "Value"
    
    ### Add year
    inDF$Year <- year(inDF$Date)

    myDF <- summaryBy(Value~Year+Ring, FUN=mean, data=inDF, keep.names=T)
    
    ### date list
    d.list <- unique(myDF$Year)
    d.list <- d.list[order(d.list)]
    
    ### create delta df
    delta <- subset(myDF, Year != d.list[1])
    delta$Start_year <- delta$Year  

    
    #### calculate differences
    for (i in 1:length(delta$Year)) {
        delta$Start_year[i] <- d.list[which(d.list == delta$Year[i]) - 1]
        delta$prev_biom[i] <- myDF$Value[myDF$Ring == delta$Ring[i] &
                                                     as.numeric(myDF$Year-delta$Start_year[i])==0]
    }
    
    ### Length of period
    delta$length <- as.numeric(delta$Year - delta$Start_year)
    
    ### annualize the difference
    delta$diff_g_yr <- (delta$Value - delta$prev_biom)
    
    #- format dataframe to return
    out <- delta[,c("Start_year", "Year", "Year", "Ring", "diff_g_yr")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "delta")
    
    return(out)
}