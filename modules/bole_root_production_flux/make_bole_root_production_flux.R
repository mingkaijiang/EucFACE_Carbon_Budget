make_bole_root_production_flux <- function(br_pool) {

    dates <- unique(br_pool$Date)
    dates <- dates[order(dates)]
    
    prod <- subset(br_pool, Date != dates[1])
    prod$Start_date <- prod$Date  # just to make this a date format! 
    for (i in 1:length(prod$Date)) {
        prod$Start_date[i] <- dates[which(dates == prod$Date[i]) - 1]
        prod$prev_biom[i] <- br_pool$bole_root_pool[br_pool$Ring == prod$Ring[i] &
                                            as.numeric(br_pool$Date-prod$Start_date[i])==0]
    }
    
    # Length of period
    prod$length <- as.numeric(prod$Date - prod$Start_date)
    
    # C increment in mg C d-1
    prod$br_production_flux <- (prod$bole_root_pool - prod$prev_biom) * 1000/prod$length
    
    # format dataframe to return
    br.out <- prod[,c("Start_date", "Date", "Date", "Ring", "br_production_flux")]
    
    names(br.out) <- c("Start_date", "End_date", "Date", "Ring", "bole_root_production_flux")
    
    br.out$ndays <- as.numeric(br.out$End_date - br.out$Start_date) + 1
    
    # Only use data period 2012-2016
    br.out <- br.out[br.out$Date<="2016-12-31",]
    
    return(br.out)
    
}