make_wood_respiration_flux <- function(wood.pool) {
    ### "main" module function for wood respiration. 
    ### Needs temperature, sapwood mass, and branch wood mass as inputs
    ### right now we don't have branch wood biomass so ignores this
    
    ### download the data
    download_wood_respiration()
    
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
    
    ####### read in the stem respiration data, calculate mean rate of ambient data. 
    Rstem.dat <- read.csv("download/WTC_TEMP_CM_WTCFLUX-STEM_20140528_L1_v1.csv")
    Rstem <- mean(Rstem.dat[which(Rstem.dat$T_treatment=="ambient"),"R_stem_nmol"]) # units are nmol CO2 g-1 s-1 at 15 deg C
    
    ### read in the leaf and branch respiration data, calculate mean rate of ambient data
    Rbranch.dat1 <- read.csv("download/WTC_TEMP_CM_GX-RBRANCH_20140513-20140522_L1_v1.csv")
    Rbranch.dat <- subset(Rbranch.dat1, date == "2014-05-13") # ignore post-girdling data
    Rleaf <- mean(Rbranch.dat[which(Rbranch.dat$T_treatment=="ambient"),"Rleaf"]) # units are nmol CO2 g-1 s-1 at 15 deg C
    Rbranch <- mean(Rbranch.dat[which(Rbranch.dat$T_treatment=="ambient"),"Rbranch"]) # units are nmol CO2 g-1 s-1 at 15 deg C
    
    ### So, Rstem and Rbranch are tissue-specific rates of R (nmol CO2 g-1 s-1) at 15 deg C.
    ### These need to be scaled to the correct temperature and multiplied by mass.
    
    ###### One wood pool per year
    wood.pool$yr <- year(wood.pool$Date)
    
    ### obtain sapwood c to calcualte sapwood DM
    sap.c <- make_sapwood_c_n_fraction()
    wood.pool$sap.c.frac[wood.pool$Ring%in%c(2,3,6)] <- sap.c$aCO2[sap.c$variable=="C"]
    wood.pool$sap.c.frac[wood.pool$Ring%in%c(1,4,5)] <- sap.c$eCO2[sap.c$variable=="C"]
    wood.pool$sap.dm <- wood.pool$sap_pool / wood.pool$sap.c.frac
    
    ### assign sapwood DM to hourly Tair data
    hDF$yr <- year(hDF$DateHour)
    for (i in unique(wood.pool$yr)) {
        for (j in 1:6) {
            hDF$Sap.DM[hDF$yr==i & hDF$Ring==j] <- wood.pool$sap.dm[wood.pool$yr==i & wood.pool$Ring==j]
        }
    }
    
    ### Calculate respiration rate (nmol CO2 m-2 h-1)
    hDF$Resp <- Rstem * Rbase^((hDF$AirTC_1_Avg - 15)/10) * hDF$Sap.DM * 3600
    
    ### Convert unit from nmol CO2 m-2 h-1 to mg C m-2 h-1
    hDF$Resp_mg <- hDF$Resp * 1e-9 * 12.01 * 1000
    
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