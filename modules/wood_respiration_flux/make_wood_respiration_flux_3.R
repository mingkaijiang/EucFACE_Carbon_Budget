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
    hDF <- hDF[hDF$Date >= "2013-01-01",]
    
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
    
    ### Calculate mean air temperature
    hDF$AirT <- rowMeans(data.frame(hDF$AirTC_1_Avg, hDF$AirTC_2_Avg), na.rm=T)

    ### Calculate respiration rate (umol CO2 m-2 h-1)
    hDF$Resp <- exp(hDF$a + hDF$b * hDF$AirT) * hDF$SA * 3600
    
    ### Convert unit from umol CO2 m-2 h-1 to mg C m-2 h-1
    hDF$Resp_mg <- hDF$Resp * 1e-6 * 12.01 * 1000
    
    ### Add a scaling factor for aCO2 and eCO2
    ### which acounts for relative contribution of wood efllux to total stem respiration
    ### values taken from Roberto's paper (in review in GCB)
    #a.factor <- 1/mean(c(0.82, 0.96, 0.94))  
    #e.factor <- 1/mean(c(1.11, 1.02, 0.97))
    #hDF$scale_factor[hDF$Ring%in%c(2,3,6)] <- a.factor
    #hDF$scale_factor[hDF$Ring%in%c(1,4,5)] <- e.factor
    
    hDF$Resp_scaled <- hDF$Resp_mg #* hDF$scale_factor
    
    ### daily sums of stem respiration, in mg m-2 d-1
    hDF$Date <- strptime(hDF$DateHour, format="%Y-%m-%d")
    dDF <- summaryBy(Resp_scaled~Date+Ring, data=hDF, FUN=sum, keep.names=T, na.rm=T)
    colnames(dDF) <- c("Date", "Ring", "wood_respiration")
    dDF$Date <- as.Date(as.character(dDF$Date))
    
    
    #test <- summaryBy(Resp_mg~Date+Ring, data=hDF, FUN=sum, keep.names=T, na.rm=T)
    #colnames(test) <- c("Date", "Ring", "wood_respiration")
    #test$Date <- as.Date(as.character(test$Date))
    #
    #testDF2 <- merge(dDF, test, by=c("Date", "Ring"))
    #testDF2$Trt[testDF2$Ring%in%c(2,3,6)] <- "aCO2"
    #testDF2$Trt[testDF2$Ring%in%c(1,4,5)] <- "eCO2"
    #testDF2$year <- year(testDF2$Date)
    #
    #testDF3 <- summaryBy(wood_respiration.x+wood_respiration.y~Ring+year+Trt, FUN=sum, data=testDF2, keep.names=T)
    #testDF3$wood_respiration.x <- testDF3$wood_respiration.x/1000
    #testDF3$wood_respiration.y <- testDF3$wood_respiration.y/1000
    #
    #trtDF <- summaryBy(wood_respiration.x+wood_respiration.y~year+Trt, FUN=mean, data=testDF3, keep.names=T)
    #
    #p2 <- ggplot(trtDF)+
    #    geom_point(data=trtDF[trtDF$Trt=="aCO2",], aes(x=year, y=wood_respiration.x, fill="corrected_aCO2"),shape=21, size=5)+
    #    geom_point(data=trtDF[trtDF$Trt=="eCO2",], aes(x=year, y=wood_respiration.x, fill="corrected_eCO2"),shape=21, size=5)+
    #    geom_point(data=trtDF[trtDF$Trt=="aCO2",], aes(x=year, y=wood_respiration.y, fill="uncorrected_aCO2"),shape=21, size=5)+
    #    geom_point(data=trtDF[trtDF$Trt=="eCO2",], aes(x=year, y=wood_respiration.y, fill="uncorrected_eCO2"),shape=21, size=5)+
    #    scale_fill_manual(name="Method", 
    #                      values = c("corrected_aCO2" = "blue", "corrected_eCO2" = "darkblue",
    #                                 "uncorrected_aCO2" = "yellow", "uncorrected_eCO2" = "orange"),
    #                      labels = c("corrected_aCO2", "corrected_eCO2","uncorrected_aCO2", "uncorrected_eCO2"))
    #
    #pdf("R_other/Rwood_correction_check.pdf")
    #plot(p2)
    #dev.off()
    
    dDF$End_date <- dDF$Start_date <- dDF$Date
    dDF$ndays <- 1
    
    out <- dDF[,c("Date", "Start_date", "End_date", "Ring", "wood_respiration", "ndays")]
    

    return(out)
    
}