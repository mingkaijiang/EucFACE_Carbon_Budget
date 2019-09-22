make_delta_leaf_pool_function_2 <- function(inDF,var.col) {
    ### average december values and then calculate delta pools
    
    ### Change column name of value variable
    colnames(inDF)[var.col] <- "Value"
    
    myDF <- inDF
    
    ### set year
    myDF$year <- year(myDF$Date)
    yr.list1 <- unique(year(inDF$Date))
    l1 <- length(yr.list1) 
    
    ### set month
    myDF$month <- month(myDF$Date)
    
    ### subset
    subDF <- subset(myDF, month == 12)
    
    smDF <- summaryBy(Value~year+Ring, data=subDF, FUN=mean, keep.names=T)
    
    ### create delta df
    delta <- data.frame(rep(c(1:6), each=l1), rep(yr.list1, 6), NA, NA, NA)
    colnames(delta) <- c("Ring", "Year", "Start_date", "End_date", "delta")
    
    ### assign values
    for (i in 2:5) {
        ### per ring
        for (j in 1:6) {
            v1 <- smDF$Value[smDF$year == yr.list1[i] & smDF$Ring == j] - smDF$Value[smDF$year == yr.list1[i-1] & smDF$Ring == j]

            delta$delta[delta$Ring == j & delta$Year == yr.list1[i]] <- v1
        }
    }
    
    for (i in yr.list) {
        for (j in 1:6) {
            delta$Date[delta$Ring==j&delta$Year==i] <- as.character(subDF$Date[subDF$Ring==j&subDF$year==i][1])
        }
    }

    delta <- subset(delta, Year > 2012)

    ### delta start and end years
    delta$Start_date <- rep(yr.list1[-5], 6)
    delta$End_date <- delta$Year

    ### format dataframe to return
    out <- delta[,c("Start_date", "End_date", "Date", "Ring", "delta")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "delta")
    
    return(out)
}