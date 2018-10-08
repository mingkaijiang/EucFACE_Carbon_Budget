make_wood_respiration_flux_3 <- function() {
    ### "main" module function for wood respiration. 
    ### Needs temperature, and wood surface

    ### download the data
    download_wood_respiration()
    
    ### calculate stem surface area
    sfcDF <- make_stem_surface_area(ring_area)
    
    ######## Download below canopy Tair data
    metDF <- download_tair_below_canopy()
    
    ### Assign ring information
    metDF$Ring <- sub("FACE_R", "", metDF$Source)
    metDF$Ring <- sub("_B1.*", "", metDF$Ring)
    metDF$Ring <- as.numeric(metDF$Ring)  
    metDF$DateHour <- as.POSIXct(paste0(metDF$Date, " ", hour(metDF$DateTime), ":00:00"),format = "%Y-%m-%d %H:%M:%S")
    
    ### Calculate hourly mean
    hDF <-aggregate(metDF[c("AirTC_1_Avg","AirTC_2_Avg")], 
                    by=metDF[c("DateHour", "Ring")], 
                    FUN=mean, na.rm = T, keep.names=T) 

    ### Only include pre-2017 data
    hDF <- hDF[hDF$Date <= "2016-12-31",]
    hDF <- hDF[hDF$Date >= "2012-08-01",]
    
    ### Add stem area data
    for (i in 1:6) {
        hDF$SA[hDF$Ring==i] <- sfcDF$wood_surface_area[sfcDF$Ring==i]
    }
    
    ### Add DOY and Year information
    hDF$Yr <- year(hDF$DateHour)
    hDF$DOY <- yday(hDF$DateHour)
    
    ### obtain basal stem respiration rate 
    bDF <- make_stem_basal_respiration_rate()
    
    ####### read in the stem respiration data, unit in umol CO2 m-2 of wood area s-1
    for (i in 1:6) {
        hDF$a[hDF$Ring==i] <- bDF$int[bDF$Ring==i]
        hDF$b[hDF$Ring==i] <- bDF$coef[bDF$Ring==i]
    }

    ### Calculate respiration rate (umol CO2 m-2 h-1)
    hDF$Resp <- exp(hDF$a + hDF$b * hDF$AirTC_1_Avg) * hDF$SA * 3600
    
    ### Convert unit from umol CO2 m-2 h-1 to mg C m-2 h-1
    hDF$Resp_mg <- hDF$Resp * 1e-6 * 12.01 * 1000
    
    ### daily sums of stem respiration
    hDF$Date <- strptime(hDF$DateHour, format="%Y-%m-%d")
    dDF <- summaryBy(Resp_mg~Date+Ring, data=hDF, FUN=sum, keep.names=T, na.rm=T)
    colnames(dDF) <- c("Date", "Ring", "wood_respiration")
    dDF$Date <- as.Date(as.character(dDF$Date))
    
    dDF$End_date <- dDF$Start_date <- dDF$Date
    dDF$ndays <- 1
    
    out <- dDF[,c("Date", "Start_date", "End_date", "Ring", "wood_respiration", "ndays")]
    

    return(out)
    
}