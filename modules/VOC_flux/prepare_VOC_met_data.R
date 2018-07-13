prepare_VOC_met_data <- function(laiDF) {
    
    ### Prepare hourly met data
    rainDF <- prepare_rainfall_data(plot.image=F, timestep="Hourly")
    tair_rh_par_presDF <- prepare_met_air_data(timestep="Hourly")
    
    ### Read met input
    DF1 <- read.csv("R_other/met_air_flux_data_hourly.csv")
    DF2 <- read.csv("R_other/rainfall_data_hourly.csv")
    
    ### Read VOC basal rate and LAI
    #download_voc_data()
    #vocDF <- read.csv(file.path(getToPath(), 
    #                           "FACE_P0044_RA_ISOPRENE_2013-15_RAW_V1.csv"))
    

    ## example DF
    #bDF <- read.table("~/Documents/Research/Projects/EucFACE_C_Balance/VOC/data.txt")
    
    
    ### Update date column names to be consistent
    names(DF1)[1] <- names(DF2)[1] <- "DateHour"
    myDF <- merge(DF1, DF2, by.x="DateHour", all=T)
    colnames(myDF) <- c("DateHour", "Tair", "RH", "PAR", "Pressure", "Wind", "Rain")
    myDF$DateHour <- as.POSIXct(as.character(myDF$DateHour))
    
    ### Create a time series dataset to cover the entire period
    time_index <- seq(from = as.POSIXct("2012-06-25 16:00"), 
                      to = as.POSIXct("2018-07-13 05:00"), by = "hour")
    baseDF <- data.frame(time_index, NA)
    colnames(baseDF) <- c("DateHour", "index")
    baseDF$index <- 1:length(time_index)
    
    ### Consider all data
    cDF <- merge(baseDF, myDF, by.x="DateHour", all=T)
    cDF$Date <- as.Date(cDF$DateHour)
    
    ### Convert units
    # Temperature from C to K
    cDF$TempK <- cDF$Tair + 273.15

    # Pressure from hPa to Pa
    cDF$D2PRES <- cDF$Pressure * 100
    
    ### Add avg Temp of past 24 hours (1 d) and 240 hours (10 d)
    date.list <- unique(cDF$Date)
    
    # 24 hour
    cDF$TEMP_24[cDF$Date==date.list[1]] <- mean(cDF$TempK[cDF$Date==date.list[1]], na.rm=T)
    cDF$PAR_24[cDF$Date==date.list[1]] <- mean(cDF$PAR[cDF$Date==date.list[1]], na.rm=T)
    
    for (i in 2:length(date.list)) {
        cDF$TEMP_24[cDF$Date==date.list[i]] <- mean(cDF$TempK[cDF$Date==date.list[i-1]], na.rm=T)
        cDF$PAR_24[cDF$Date==date.list[i]] <- mean(cDF$PAR[cDF$Date==date.list[i-1]], na.rm=T)
    }
        
    # 240 hour
    cDF$TEMP_240[cDF$Date %in% date.list[1:10]] <- mean(cDF$TempK[cDF$Date %in% date.list[1:10]], na.rm=T)
    cDF$PAR_240[cDF$Date %in% date.list[1:10]] <- mean(cDF$PAR[cDF$Date %in% date.list[1:10]], na.rm=T)
    
    for (i in 11:length(date.list)) {
        s<- i - 10 + 1
        cDF$TEMP_240[cDF$Date == date.list[i]] <- mean(cDF$TempK[cDF$Date %in% date.list[s:i]], na.rm=T)
        cDF$PAR_240[cDF$Date == date.list[i]] <- mean(cDF$PAR[cDF$Date %in% date.list[s:i]], na.rm=T)
    }
    
    
    ### Assign LAI data onto met data frame
    d.list <- unique(laiDF$Date)
    
    for (i in 1:length(d.list)) {
        cDF$LAI_R1[cDF$Date==d.list[i]] <- laiDF$lai_variable[laiDF$Ring==1 & laiDF$Date==d.list[i]]
        cDF$LAI_R2[cDF$Date==d.list[i]] <- laiDF$lai_variable[laiDF$Ring==2 & laiDF$Date==d.list[i]]
        cDF$LAI_R3[cDF$Date==d.list[i]] <- laiDF$lai_variable[laiDF$Ring==3 & laiDF$Date==d.list[i]]
        cDF$LAI_R4[cDF$Date==d.list[i]] <- laiDF$lai_variable[laiDF$Ring==4 & laiDF$Date==d.list[i]]
        cDF$LAI_R5[cDF$Date==d.list[i]] <- laiDF$lai_variable[laiDF$Ring==5 & laiDF$Date==d.list[i]]
        cDF$LAI_R6[cDF$Date==d.list[i]] <- laiDF$lai_variable[laiDF$Ring==6 & laiDF$Date==d.list[i]]
    }
    
    ### Cut off dates 2012-10-26 to 2017-01-01
    cDF <- subset(cDF, Date>="2012-10-26")
    cDF <- subset(cDF, Date<="2017-01-01")
    
    ### Linear interpolation of missing values
    require(zoo)
    cDF$LAI_R1 <- na.approx(cDF$LAI_R1, na.rm=T)
    cDF$LAI_R2 <- na.approx(cDF$LAI_R2, na.rm=T)
    cDF$LAI_R3 <- na.approx(cDF$LAI_R3, na.rm=T)
    cDF$LAI_R4 <- na.approx(cDF$LAI_R4, na.rm=T)
    cDF$LAI_R5 <- na.approx(cDF$LAI_R5, na.rm=T)
    cDF$LAI_R6 <- na.approx(cDF$LAI_R6, na.rm=T)
    
    cDF$Tair <- na.approx(cDF$Tair, na.rm=T)
    cDF$RH <- na.approx(cDF$RH, na.rm=T)
    cDF$PAR <- na.approx(cDF$PAR, na.rm=T)
    cDF$Pressure <- na.approx(cDF$Pressure, na.rm=T)
    cDF$Wind <- na.approx(cDF$Wind, na.rm=T)
    cDF$Rain <- na.approx(cDF$Rain, na.rm=T)
    cDF$TempK <- na.approx(cDF$TempK, na.rm=T)
    cDF$D2PRES <- na.approx(cDF$D2PRES, na.rm=T)
    
    ### Complete cases
    cDF <- cDF[complete.cases(cDF),]
 
    ### Add soil moisture per ring
    smDF <- prepare_soil_moisture_data(plot.image=F, monthly=F)
    d.list <- unique(smDF$R1$Date)
    
    for (i in 1:length(d.list)) {
        cDF$SM_R1[cDF$Date==d.list[i]] <- ifelse(length(smDF$R1$Theta30_1_Avg[smDF$R1$Date==d.list[i]]) >0,
                                                 smDF$R1$Theta30_1_Avg[smDF$R1$Date==d.list[i]], NA)
        
        cDF$SM_R2[cDF$Date==d.list[i]] <- ifelse(length(smDF$R2$Theta30_1_Avg[smDF$R2$Date==d.list[i]]) >0,
                                                 smDF$R2$Theta30_1_Avg[smDF$R2$Date==d.list[i]], NA)
        
        cDF$SM_R3[cDF$Date==d.list[i]] <- ifelse(length(smDF$R3$Theta30_1_Avg[smDF$R3$Date==d.list[i]]) >0,
                                                 smDF$R3$Theta30_1_Avg[smDF$R3$Date==d.list[i]], NA)
        
        cDF$SM_R4[cDF$Date==d.list[i]] <- ifelse(length(smDF$R4$Theta30_1_Avg[smDF$R4$Date==d.list[i]]) >0,
                                                 smDF$R4$Theta30_1_Avg[smDF$R4$Date==d.list[i]], NA)
        
        cDF$SM_R5[cDF$Date==d.list[i]] <- ifelse(length(smDF$R5$Theta30_1_Avg[smDF$R5$Date==d.list[i]]) >0,
                                                 smDF$R5$Theta30_1_Avg[smDF$R5$Date==d.list[i]], NA)
        
        cDF$SM_R6[cDF$Date==d.list[i]] <- ifelse(length(smDF$R6$Theta30_1_Avg[smDF$R6$Date==d.list[i]]) >0,
                                                 smDF$R6$Theta30_1_Avg[smDF$R6$Date==d.list[i]], NA)
    }

    ### Fill gap
    cDF$SM_R1 <- na.approx(cDF$SM_R1, na.rm=T)
    cDF$SM_R2 <- na.approx(cDF$SM_R2, na.rm=T)
    cDF$SM_R3 <- na.approx(cDF$SM_R3, na.rm=T)
    cDF$SM_R4 <- na.approx(cDF$SM_R4, na.rm=T)
    cDF$SM_R5 <- na.approx(cDF$SM_R5, na.rm=T)
    cDF$SM_R6 <- na.approx(cDF$SM_R6, na.rm=T)
    
    ### Convert unit
    cDF$SM_R1 <- cDF$SM_R1 / 100
    cDF$SM_R2 <- cDF$SM_R2 / 100
    cDF$SM_R3 <- cDF$SM_R3 / 100
    cDF$SM_R4 <- cDF$SM_R4 / 100
    cDF$SM_R5 <- cDF$SM_R5 / 100
    cDF$SM_R6 <- cDF$SM_R6 / 100
    
    out <- cDF[,c("DateHour", "TempK", "TEMP_24", "TEMP_240", 
                  "PAR", "PAR_24", "PAR_240", "RH", "Wind", "Rain",
                  "D2PRES", "LAI_R1", "LAI_R2", "LAI_R3", "LAI_R4",
                  "LAI_R5", "LAI_R6", "SM_R1", "SM_R2", "SM_R3",
                  "SM_R4", "SM_R5", "SM_R6")]
    
    write.csv(out, "R_other/VOC_met_data.csv")
    
}