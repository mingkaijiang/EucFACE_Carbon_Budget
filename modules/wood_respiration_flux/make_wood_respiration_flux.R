make_wood_respiration_flux <- function() {
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
    
    ####### read in the stem respiration data, unit in umol CO2 m-2 of wood area s-1
    ### Dec to mid feb: Ea = 0.0736 * exp(0.1336 * Tstem)
    ### Mid Feb to Nov: Ea = 0.6055 * exp(0.0416 * Tstem)
    hDF$a[hDF$DOY>=46 & hDF$DOY <= 305] <- 0.6055
    hDF$b[hDF$DOY>=46 & hDF$DOY <= 305] <- 0.0416
    hDF$a[hDF$DOY<46 | hDF$DOY > 305] <- 0.0736
    hDF$b[hDF$DOY<46 | hDF$DOY > 305] <- 0.1336
    
    ### Calculate respiration rate (umol CO2 m-2 h-1)
    hDF$Resp <- hDF$a * exp(hDF$b * hDF$AirTC_1_Avg) * hDF$SA * 3600
    
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
    
    #out$Yr <- year(out$Date)
    #test <- summaryBy(wood_respiration~Yr+Ring, data=out, FUN=sum, keep.names=T, na.rm=T)
    #test <- test[test$Yr>2012,]
    #test$wood_respiration <- test$wood_respiration/1000
    #test$Treatment[test$Ring%in%c(2,3,6)] <- "aCO2"
    #test$Treatment[test$Ring%in%c(1,4,5)] <- "eCO2"
    #
    #p3 <- ggplot(test, aes(x=as.character(Ring),y=wood_respiration, fill=as.factor(Treatment)))+
    #    geom_bar(stat="identity")+facet_grid(~Yr)+
    #    labs(x="Ring", y=expression(paste(R[wood], " (g C ", m^-2, yr^-1, ")")))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_line(color="grey"),
    #          legend.position="right")+
    #    scale_y_continuous(position="left")+
    #    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
    #                      labels=c(expression(aCO[2]), expression(eCO[2])))
    #
    #plot(p3)
    #
    #test2 <- summaryBy(wood_respiration~Yr, FUN=mean, data=test, keep.names=T)
    #
    #
    #with(dDF[dDF$Ring==1,], plot(wood_respiration~Date, 
    #                             xlim=c(as.Date("2013-02-01"), as.Date("2013-02-28"))))
    #abline(v=as.Date("2013-02-15"))
    #
    #with(dDF[dDF$Ring==1,], plot(wood_respiration~Date, 
    #                             xlim=c(as.Date("2013-11-01"), as.Date("2013-12-31"))))
    #abline(v=as.Date("2013-12-01"))
    
    
    return(out)
    
}