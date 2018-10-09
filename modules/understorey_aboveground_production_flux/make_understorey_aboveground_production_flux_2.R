make_understorey_aboveground_production_flux_2 <- function(poolDF) {
    
    
    dates <- unique(poolDF$Date)
    dates <- dates[order(dates)]
    
    prod <- subset(poolDF, Date != dates[1])
    prod$Start_date <- prod$Date  # just to make this a date format! 
    for (i in 1:length(prod$Date)) {
        prod$Start_date[i] <- dates[which(dates == prod$Date[i]) - 1]
        prod$prev_biom[i] <- poolDF$Total_g_C_m2[poolDF$Ring == prod$Ring[i] &
                                                          as.numeric(poolDF$Date-prod$Start_date[i])==0]
    }
    
    # Length of period
    prod$length <- as.numeric(prod$Date - prod$Start_date)
    
    # C increment in mg C d-1
    prod$understorey_production_flux <- (prod$Total_g_C_m2 - prod$prev_biom) * 1000/prod$length
    
    # format dataframe to return
    out <- prod[,c("Start_date", "Date", "Date", "Ring", "understorey_production_flux")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "understorey_production_flux")
    
    out$ndays <- as.numeric(out$End_date - out$Start_date) + 1
    
    # Only use data period 2012-2016
    out <- out[out$Date<="2016-12-31",]
    
    return(out)

}


