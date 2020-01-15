make_wood_respiration_flux <- function() {
    ### "main" module function for wood respiration. 
    ### Needs temperature, and wood surface
    
    ### calculate stem surface area
    sfcDF <- make_stem_surface_area(ring_area)

    ######## Download below canopy Tair data
    hDF <- read.csv("download/EucFACE_Tair_below_canopy.csv")
    
    ### Add stem area data
    for (i in 1:6) {
        hDF$SA[hDF$Ring==i] <- sfcDF$wood_surface_area[sfcDF$Ring==i]
    }
    
    ### Add DOY and Year information
    hDF$Yr <- year(hDF$DateHour)
    hDF$DOY <- yday(hDF$DateHour)
    
    ####### read in the stem respiration data, unit in umol CO2 m-2 of wood area s-1
    hDF$a <- 0.1866
    hDF$b <- 0.1042
    
    a.factor <- 1/mean(c(0.82, 0.96, 0.94))  
    e.factor <- 1/mean(c(1.11, 1.02, 0.97))
    hDF$scale_factor[hDF$Ring%in%c(2,3,6)] <- a.factor
    hDF$scale_factor[hDF$Ring%in%c(1,4,5)] <- e.factor
    
    ### Calculate respiration rate (umol CO2 m-2 h-1)
    hDF$Resp <- hDF$a * exp(hDF$b * hDF$AirTC_1_Avg) * hDF$SA * 3600

    ### Convert unit from umol CO2 m-2 h-1 to mg C m-2 h-1
    hDF$Resp_mg <- hDF$Resp * 1e-6 * 12.01 * 1000 #* hDF$scale_factor

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