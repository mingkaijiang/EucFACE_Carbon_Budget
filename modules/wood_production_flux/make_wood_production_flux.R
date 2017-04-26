#- Make the wood production flux
# This is done by differencing values in the wood pool
# The wood pool needs to be passed to this function
make_wood_production_flux <- function(wood_pool){
  
  dates <- unique(wood_pool$Date)
  dates <- dates[order(dates)]
  
  prod <- subset(wood_pool, Date != dates[1])
  prod$Start_date <- prod$Date  # just to make this a date format! 
  for (i in 1:length(prod$Date)) {
    prod$Start_date[i] <- dates[which(dates == prod$Date[i]) - 1]
    prod$prev_biom[i] <- wood_pool$wood_pool[wood_pool$Ring == prod$Ring[i] &
                                     as.numeric(wood_pool$Date-prod$Start_date[i])==0]
  }
  
  # Length of period
  prod$length <- as.numeric(prod$Date - prod$Start_date)
  # C increment in mg C d-1
  prod$wood_production_flux <- (prod$wood_pool - prod$prev_biom) * 1000/prod$length
  
  #- format dataframe to return
  wp.out <- prod[,c("Date","Ring","wood_production_flux","Start_date")]
  names(wp.out) <- c("End_date", "Ring", "wood_production_flux", "Start_date")
  return(wp.out)
}

