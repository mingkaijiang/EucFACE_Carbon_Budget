make_delta_fineroot_pool_treatment_abs_effect_2 <- function(inDF,var.col) {
    
    ### 
    inDF$Date <- as.Date(as.character(inDF$Datef))
    
    inDF <- inDF[order(inDF$Date),]
    inDF$Date <- as.character(inDF$Date)
    
    ### Change column name of value variable
    colnames(inDF)[var.col] <- "Value"
    
    ### 
    subDF1 <- subset(inDF, Date%in%c("2014-06-01","2015-05-01"))
    subDF2 <- subset(inDF, Date%in%c("2014-09-01","2015-09-01"))
    
    ### date list
    d.list1 <- unique(subDF1$Date)
    d.list1 <- d.list1[order(d.list1)]
    
    d.list2 <- unique(subDF2$Date)
    d.list2 <- d.list2[order(d.list2)]
    
    ### as. date
    subDF1$Date <- as.Date(subDF1$Date)
    subDF2$Date <- as.Date(subDF2$Date)
    
    ### create delta df
    delta1 <- subset(subDF1, Date != d.list1[1])
    delta1$Start_date <- delta1$Date  
    
    delta2 <- subset(subDF2, Date != d.list2[1])
    delta2$Start_date <- delta2$Date  
    
    
    #### calculate differences
    for (i in 1:length(delta1$Date)) {
        delta1$Start_date[i] <- d.list1[which(d.list1 == delta1$Date[i]) - 1]
        delta1$prev_biom[i] <- subDF1$Value[subDF1$Ring == delta1$Ring[i] &
                                                as.numeric(subDF1$Date-delta1$Start_date[i])==0]
    }
    
    for (i in 1:length(delta2$Date)) {
        delta2$Start_date[i] <- d.list2[which(d.list2 == delta2$Date[i]) - 1]
        delta2$prev_biom[i] <- subDF2$Value[subDF2$Ring == delta2$Ring[i] &
                                                as.numeric(subDF2$Date-delta2$Start_date[i])==0]
    }
    
    
    ### combine
    delta <- rbind(delta1, delta2)
    
    ### Length of period
    delta$length <- as.numeric(delta$Date - delta$Start_date)
    
    
    ### annualize the difference
    delta$diff_g_yr <- (delta$Value - delta$prev_biom) / delta$length * 365
    
    #- format dataframe to return
    out <- delta[,c("Start_date", "Date", "Date", "Ring", "diff_g_yr")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "predicted")
    
    return(out)
}