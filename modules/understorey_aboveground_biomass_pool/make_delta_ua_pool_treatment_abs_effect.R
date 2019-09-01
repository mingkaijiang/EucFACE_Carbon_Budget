make_delta_ua_pool_treatment_abs_effect <- function(inDF,var.col) {
    
    ### Change column name of value variable
    inDF$Date <- as.Date(as.character(inDF$Datef))
    
    colnames(inDF)[var.col] <- "Value"
    
    myDF <- inDF
    
    ### create delta df
    delta <- data.frame(rep(c(1:6), each=1), NA, NA, NA)
    colnames(delta) <- c("Ring","Start_date", "End_date", "delta")
    
    s.date <- "2015-02-01"
    e.date <- "2016-02-01"
    
    delta$Start_date <- as.Date(s.date)
    delta$End_date <- as.Date(e.date)
    
    ### assign values
    
    ### per ring
    for (j in 1:6) {
        ### unnormalized
        v1 <- myDF$Value[myDF$Date == e.date & myDF$Ring == j] - myDF$Value[myDF$Date == s.date & myDF$Ring == j]
        delta$delta[delta$Ring == j] <- v1
    }
    
    
    #- format dataframe to return
    out <- delta[,c("Start_date", "End_date", "End_date", "Ring", "delta")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "predicted")
    
    return(out)
}