make_delta_microbial_pool_treatment_abs_effect <- function(inDF,var.col) {
    
    ### 
    inDF$Date <- as.Date(as.character(inDF$Datef))
    
    inDF <- inDF[order(inDF$Date),]
    inDF$Date <- as.character(inDF$Date)
    
    ### Change column name of value variable
    colnames(inDF)[var.col] <- "Value"
    
    ### 
    subDF1 <- subset(inDF, Date%in%c("2013-03-11","2014-03-10","2015-03-10"))
    subDF2 <- subset(inDF, Date%in%c("2012-12-11","2013-12-10","2014-11-17","2015-11-30"))
    subDF3 <- subset(inDF, Date%in%c("2012-06-13","2013-06-11","2014-06-17","2015-06-09"))
    subDF4 <- subset(inDF, Date%in%c("2012-09-03","2013-09-03","2014-09-09","2015-09-09"))

    
    ### date list
    d.list1 <- unique(subDF1$Date)
    d.list1 <- d.list1[order(d.list1)]
    
    d.list2 <- unique(subDF2$Date)
    d.list2 <- d.list2[order(d.list2)]
    
    d.list3 <- unique(subDF3$Date)
    d.list3 <- d.list3[order(d.list3)]
    
    d.list4 <- unique(subDF4$Date)
    d.list4 <- d.list4[order(d.list4)]
    
    ### as. date
    subDF1$Date <- as.Date(subDF1$Date)
    subDF2$Date <- as.Date(subDF2$Date)
    subDF3$Date <- as.Date(subDF3$Date)
    subDF4$Date <- as.Date(subDF4$Date)
    
    ### create delta df
    delta1 <- subset(subDF1, Date != d.list1[1])
    delta1$Start_date <- delta1$Date  
    
    delta2 <- subset(subDF2, Date != d.list2[1])
    delta2$Start_date <- delta2$Date  
    
    delta3 <- subset(subDF3, Date != d.list3[1])
    delta3$Start_date <- delta3$Date  
    
    delta4 <- subset(subDF4, Date != d.list4[1])
    delta4$Start_date <- delta4$Date  

    
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
    
    for (i in 1:length(delta3$Date)) {
        delta3$Start_date[i] <- d.list3[which(d.list3 == delta3$Date[i]) - 1]
        delta3$prev_biom[i] <- subDF3$Value[subDF3$Ring == delta3$Ring[i] &
                                                as.numeric(subDF3$Date-delta3$Start_date[i])==0]
    }
    
    for (i in 1:length(delta4$Date)) {
        delta4$Start_date[i] <- d.list4[which(d.list4 == delta4$Date[i]) - 1]
        delta4$prev_biom[i] <- subDF4$Value[subDF4$Ring == delta4$Ring[i] &
                                                as.numeric(subDF4$Date-delta4$Start_date[i])==0]
    }
    
    ### combine
    #delta <- rbind(delta1, delta2, delta3, delta4)
    #delta <- rbind(delta2, delta3, delta4)
    delta <- delta2 # use december value only to be consistent with soil
    
    ### Length of period
    delta$length <- as.numeric(delta$Date - delta$Start_date)

    
    ### annualize the difference
    delta$diff_g_yr <- (delta$Value - delta$prev_biom) / delta$length * 365
    
    #- format dataframe to return
    out <- delta[,c("Start_date", "Date", "Date", "Ring", "diff_g_yr")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "predicted")
    
    return(out)
}