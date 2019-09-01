make_delta_pool_function_2 <- function(inDF,var.col) {
    
    ### Change column name of value variable
    colnames(inDF)[var.col] <- "Value"
    
    myDF <- inDF
    
    ### set year
    myDF$year <- year(myDF$Date)
    yr.list <- unique(year(inDF$Date))
    l1 <- length(yr.list) 
    
    ### create delta df
    delta <- data.frame(rep(c(1:6), each=l1), rep(yr.list, 6), NA, NA, NA)
    colnames(delta) <- c("Ring", "Year", "Start_date", "End_date", "delta")
    
    ### assign values
    for (i in yr.list) {
        s.date <- min(myDF$Date[myDF$year == i])
        e.date <- max(myDF$Date[myDF$year == i])
        
        ### Length of period
        l2 <- as.numeric(e.date - s.date)
        
        ### per ring
        for (j in 1:6) {
            delta$Start_date[delta$Ring == j & delta$Year == i] <- as.character(s.date)
            delta$End_date[delta$Ring == j & delta$Year == i] <- as.character(e.date)
            
            ### unnormalized
            v1 <- myDF$Value[myDF$Date == e.date & myDF$Ring == j] - myDF$Value[myDF$Date == s.date & myDF$Ring == j]
            v2 <- v1 / l2 * 365
            
            delta$delta[delta$Ring == j & delta$Year == i] <- v2
        }
    }
    
    delta <- subset(delta, Year > 2012)

    
    #- format dataframe to return
    out <- delta[,c("Start_date", "End_date", "End_date", "Ring", "delta")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "delta")
    
    return(out)
}