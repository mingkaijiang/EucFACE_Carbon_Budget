#- Make the change in pool size over time
# This is done by differencing values in the pool
# The pool variable needs to be passed to this function
make_change_in_pool_ann <- function(mypool){
    
    mypool$Date <- as.Date(as.character(mypool$Datef))
    
    dates <- unique(mypool$Date)
    dates <- dates[order(dates)]
    
    mypool <- mypool[order(mypool$Date),]
    
    prod <- subset(mypool, Date != dates[1])
    prod$Start_date <- prod$Date  # just to make this a date format! 
    for (i in 1:length(prod$Date)) {
        prod$Start_date[i] <- dates[which(dates == prod$Date[i]) - 1]
        prod$prev_biom[i] <- mypool[mypool$Ring == prod$Ring[i] &
                                                     as.numeric(mypool$Date-prod$Start_date[i])==0, "predicted"]
    }
    
    # Length of period
    prod$length <- as.numeric(prod$Date - prod$Start_date)
    
    # C increment in g C d-1
    prod$production_flux <- (prod[,"predicted"] - prod$prev_biom) / prod$length 
    
    # format dataframe to return
    out <- prod[,c("Start_date", "Date", "Date", "Ring", "production_flux")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "daily_biomass_change")
    out$ndays <- as.numeric(out$End_date - out$Start_date) + 1
    
    # biomass increment over the ndays period
    out$biomass_increment <- with(out, daily_biomass_change*ndays)
    
    # cumulative biomass increment
    baseDF <- subset(out, End_date==dates[2])
    
    out$cumulative_biomass_increment[out$End_date==dates[2]] <- out$biomass_increment[out$End_date==dates[2]]
    out$cumulative_ndays[out$End_date==dates[2]] <- out$ndays[out$End_date==dates[2]]
    
    for (i in 1:6) {
        for (j in 3:length(dates)) {
            ### Calculate cumulative biomass increment
            out$cumulative_biomass_increment[out$Ring==i & out$End_date==dates[j]] <- out$cumulative_biomass_increment[out$Ring==i & out$End_date==dates[j-1]] +
                out$biomass_increment[out$Ring==i & out$End_date==dates[j]]
            
            ### Get cumulative ndays
            out$cumulative_ndays[out$Ring==i & out$End_date==dates[j]] <- out$cumulative_ndays[out$Ring==i & out$End_date==dates[j-1]] +
                out$ndays[out$Ring==i & out$End_date==dates[j]]
            
        }
    }
    
    return(out)
}

