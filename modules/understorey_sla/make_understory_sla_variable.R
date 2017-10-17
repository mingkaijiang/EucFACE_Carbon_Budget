make_understorey_sla_variable <- function() {
    
    # now it is useless as Matthias's SLA data not on HIEv yet
    download_understorey_aboveground_biomass_data()
    
    # read in sla data mannually
    myDF <- read.csv("temp_files/EucFACE_GrassStrip_Harvest_20170523.csv")
    
    #- average across rings, dates and depths
    outDF <- summaryBy(LiveSubsampleSLA~Ring,data=myDF,FUN=mean,keep.names=T, na.rm=T)
    names(outDF) <- c("Ring", "Understorey_sla_variable")
    
    return(outDF)
}

