make_delta_ua_pool_function_3 <- function(inDF,var.col) {
    
    ### Change column name of value variable
    colnames(inDF)[var.col] <- "Value"
    
    inDF$Date <- as.character(inDF$Date)
    
    ### 
    subDF1 <- subset(inDF, Date%in%c("2015-02-02","2016-02-16"))
    subDF2 <- subset(inDF, Date%in%c("2015-03-22","2016-03-15"))
    subDF3 <- subset(inDF, Date%in%c("2015-08-20","2016-08-14"))
    subDF4 <- subset(inDF, Date%in%c("2015-09-23","2016-09-26"))
    subDF5 <- subset(inDF, Date%in%c("2015-10-22","2016-11-10"))
    subDF6 <- subset(inDF, Date%in%c("2015-12-02","2016-12-15"))
    
    ### date list
    #d.list1 <- unique(subDF1$Date)
    #d.list1 <- d.list1[order(d.list1)]
    #
    #d.list2 <- unique(subDF2$Date)
    #d.list2 <- d.list2[order(d.list2)]
    #
    #d.list3 <- unique(subDF3$Date)
    #d.list3 <- d.list3[order(d.list3)]
    #
    #d.list4 <- unique(subDF4$Date)
    #d.list4 <- d.list4[order(d.list4)]
    #
    #d.list5 <- unique(subDF5$Date)
    #d.list5 <- d.list5[order(d.list5)]
    
    d.list6 <- unique(subDF6$Date)
    d.list6 <- d.list6[order(d.list6)]
    
    ### as. date
    #subDF1$Date <- as.Date(subDF1$Date)
    #subDF2$Date <- as.Date(subDF2$Date)
    #subDF3$Date <- as.Date(subDF3$Date)
    #subDF4$Date <- as.Date(subDF4$Date)
    #subDF5$Date <- as.Date(subDF5$Date)
    subDF6$Date <- as.Date(subDF6$Date)
    
    ### create delta df
    #delta1 <- subset(subDF1, Date != d.list1[1])
    #delta1$Start_date <- delta1$Date  
    #
    #delta2 <- subset(subDF2, Date != d.list2[1])
    #delta2$Start_date <- delta2$Date  
    #
    #delta3 <- subset(subDF3, Date != d.list3[1])
    #delta3$Start_date <- delta3$Date  
    #
    #delta4 <- subset(subDF4, Date != d.list4[1])
    #delta4$Start_date <- delta4$Date  
    #
    #delta5 <- subset(subDF5, Date != d.list5[1])
    #delta5$Start_date <- delta5$Date  
    
    delta6 <- subset(subDF6, Date != d.list6[1])
    delta6$Start_date <- delta6$Date  
    
    
    #### calculate differences
    #for (i in 1:length(delta1$Date)) {
    #    delta1$Start_date[i] <- d.list1[which(d.list1 == delta1$Date[i]) - 1]
    #    delta1$prev_biom[i] <- subDF1$Value[subDF1$Ring == delta1$Ring[i] &
    #                                            as.numeric(subDF1$Date-delta1$Start_date[i])==0]
    #}
    #
    #for (i in 1:length(delta2$Date)) {
    #    delta2$Start_date[i] <- d.list2[which(d.list2 == delta2$Date[i]) - 1]
    #    delta2$prev_biom[i] <- subDF2$Value[subDF2$Ring == delta2$Ring[i] &
    #                                            as.numeric(subDF2$Date-delta2$Start_date[i])==0]
    #}
    #
    #for (i in 1:length(delta3$Date)) {
    #    delta3$Start_date[i] <- d.list3[which(d.list3 == delta3$Date[i]) - 1]
    #    delta3$prev_biom[i] <- subDF3$Value[subDF3$Ring == delta3$Ring[i] &
    #                                            as.numeric(subDF3$Date-delta3$Start_date[i])==0]
    #}
    #
    #for (i in 1:length(delta4$Date)) {
    #    delta4$Start_date[i] <- d.list4[which(d.list4 == delta4$Date[i]) - 1]
    #    delta4$prev_biom[i] <- subDF4$Value[subDF4$Ring == delta4$Ring[i] &
    #                                            as.numeric(subDF4$Date-delta4$Start_date[i])==0]
    #}
    #
    #for (i in 1:length(delta5$Date)) {
    #    delta5$Start_date[i] <- d.list5[which(d.list5 == delta5$Date[i]) - 1]
    #    delta5$prev_biom[i] <- subDF5$Value[subDF5$Ring == delta5$Ring[i] &
    #                                            as.numeric(subDF5$Date-delta5$Start_date[i])==0]
    #}
    
    for (i in 1:length(delta6$Date)) {
        delta6$Start_date[i] <- d.list6[which(d.list6 == delta6$Date[i]) - 1]
        delta6$prev_biom[i] <- subDF6$Value[subDF6$Ring == delta6$Ring[i] &
                                                as.numeric(subDF6$Date-delta6$Start_date[i])==0]
    }
    
    ### combine
    delta <- delta6
    #delta <- rbind(delta1, delta2, delta3, delta4, delta5, delta6)
    
    ### Length of period
    delta$length <- as.numeric(delta$Date - delta$Start_date)
    
    
    ### annualize the difference
    delta$diff_g_yr <- (delta$Value - delta$prev_biom) / delta$length * 365
    
    #- format dataframe to return
    out <- delta[,c("Start_date", "Date", "Date", "Ring", "diff_g_yr")]
    
    names(out) <- c("Start_date", "End_date", "Date", "Ring", "delta")
    
    return(out)
}