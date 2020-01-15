make_understorey_sla_variable <- function() {
    
    # read in sla data mannually
    myDF <- read.csv("temp_files/EucFACE_GrassStrip_Harvest_20170523.csv")
    
    #- average across rings, dates and depths
    outDF <- summaryBy(LiveSubsampleSLA ~ Ring, data=myDF, FUN=mean, keep.names=TRUE, na.rm=TRUE)
    names(outDF) <- c("Ring", "Understorey_sla_variable")
    
    return(outDF)
}

