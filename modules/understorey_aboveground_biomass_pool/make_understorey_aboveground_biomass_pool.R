make_understorey_aboveground_biomass_pool <- function() {
    
    # currently only Varsha's harvest data on HIEv
    download_understorey_aboveground_biomass_data()
    
    #- read in the data 
    inDF1 <- read.csv(file.path(getToPath(), 
                               "FACE_P0061_RA_PATHARE_UNDERSTORY_ABOVEGROUND_BIOMASS_L2_20150201_20160730.csv"))
    
    # read in Matthias's harvest data 
    inDF2 <- read.csv("temp_files/EucFACE_GrassStrip_Harvest_20170523.csv")
    
    # process inDFs
    inDF1$live_g <- inDF1$grasses_live_g + inDF1$forbs_g
    
    tempDF1 <- data.frame(inDF1$ring, inDF1$month, 
                       inDF1$live_g, inDF1$dead_g)
    colnames(tempDF1) <- c("Ring", "Month", "Live_g", "Dead_g")
    
    tempDF2 <- data.frame(inDF2$Ring, "17-May",
                          inDF2$LiveBiomassDW, inDF2$DeadBiomassFW)
    colnames(tempDF2) <- c("Ring", "Month", "Live_g", "Dead_g")
    
    # combine data
    myDF <- rbind(tempDF1, tempDF2)
    
    # Fix Date format:
    # Assume that e.g. 'Jan-13' means the last day of that month (2013-01-31).
    myDF$Date <- as.Date(paste0("1-", myDF$Month), format = "%d-%y-%b") + months(1) - days(1)
    
    #- average across rings and dates
    liveDF <- summaryBy(Live_g~Date+Ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    deadDF <- summaryBy(Dead_g~Date+Ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    # convert from g per 0.1 m-2 to g/m2, and make an assumption for C fraction
    outDF <- cbind(liveDF, deadDF$Dead_g)
    names(outDF) <- c("Date", "Ring", "Live_g", "Dead_g")
    outDF$Live_g_C_m2 <- outDF$Live_g / strip_area * c_fraction
    outDF$Dead_g_C_m2 <- outDF$Dead_g / strip_area * c_fraction
    outDF$Total_g_C_m2 <- outDF$Live_g_C_m2 + outDF$Dead_g_C_m2
    
    out <- outDF[,c("Date", "Ring", "Live_g_C_m2", "Dead_g_C_m2", "Total_g_C_m2")]
    
    return(out)
}


