make_understory_lai_variable <- function(){
    
    # read in height - biomass data
    myDF1 <- read.csv("download/EucFACE_vegplots_HEIGHTS_AND_PREDICTED_ABOVEGROUNDBIOMASS_v8.csv")
    
    # read in biomass - lai data
    myDF2 <- read.csv("download/EucFACE_GrassStrip_Harvest_20170523.csv")
    
    # prepare output df
    out <- matrix(ncol = 8, nrow=length(unique(myDF1$ring)) * length(unique(myDF1$date)))
    out <- as.data.frame(out)
    colnames(out) <- c("Ring", "Date", "AGB", "SLA", "LAI", "RWC", "AGB_DW", "Date_fac")
    out$Ring <- rep(1:6, each=length(unique(myDF1$date)))
    out$Date <- rep(unique(myDF1$date))
    out$Date_fac <- as.numeric(out$Date)    
    
    myDF1$Date_fac <- as.numeric(myDF1$date)
    
    # allocate AGB onto each data point
    for (i in unique(out$Ring)) {
        # ring specific SLA
        out[out$Ring == i, "SLA"] <- mean(myDF2[myDF2$Ring == i, "LiveSubsampleSLA"])
        
        # ring specific RWC
        out[out$Ring == i, "RWC"] <- mean(myDF2[myDF2$Ring == i, "LiveSubSampleRWC"])
        
        # ring and date specific AGB
        for (j in unique(out$Date_fac)) {
            # fresh weight
            out[out$Ring == i & out$Date_fac == j, "AGB"] <- mean(myDF1[myDF1$ring == i & myDF1$Date_fac == j, "AGBpred"]) * 1000.0
        }
    }
    
    # dry weight
    out$AGB_DW <- out$AGB / (1 + out$RWC) 
    
    # lai
    out$LAI <- out$AGB_DW * out$SLA * 0.0001
    
    #test <- rep(1:6)
    #test <- data.frame(test, NA)
    #colnames(test) <- c("Ring", "LAI")
    #for (i in 1:6){
    #    test[test$Ring == i, "LAI"] <- mean(out[out$Ring == i, "LAI"])
    #}
    #lot(test)
    
    # clear dataframe
    out$Date_fac <- NULL
    
    return(out)
}



download_understory_lai_variable <- function(){
    
    # data not on Hiev yet
    
}
