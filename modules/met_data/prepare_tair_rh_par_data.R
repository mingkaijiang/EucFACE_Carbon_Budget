prepare_tair_rh_par_data <- function(timestep) {
    #### Download the data - takes time to run
    myDF <- download_tair_rh_par_data()
    
    #### Assign ring information
    myDF$Ring <- sub("FACE_R", "", myDF$Source)
    myDF$Ring <- sub("_T1.*", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)  
    myDF <- myDF[order(myDF$DateTime),]
    myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
    myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
    myDF$DateHour <- as.POSIXct(paste0(myDF$Date, " ", hour(myDF$DateTime), ":00:00"),format = "%Y-%m-%d %H:%M:%S")
    
    myDF$AirTc_Avg <- as.numeric(myDF$AirTc_Avg)
    myDF$RH_Avg <- as.numeric(myDF$RH_Avg)
    myDF$LI190SB_PAR_Den_Avg <- as.numeric(myDF$LI190SB_PAR_Den_Avg)
    
    ### Calculate hourly mean
    hDF <-aggregate(myDF[c("AirTc_Avg","RH_Avg","LI190SB_PAR_Den_Avg")], 
                    by=myDF[c("DateHour")], 
                    FUN=mean, na.rm = T, keep.names=T) 
    
    ### Calculate daily mean
    dDF <- aggregate(myDF[c("AirTc_Avg","RH_Avg","LI190SB_PAR_Den_Avg")], 
                     by=myDF[c("Date")], 
                     FUN=mean, na.rm=T, keep.names=T)
    
    ### Calculate monthly mean
    mDF <- aggregate(myDF[c("AirTc_Avg","RH_Avg","LI190SB_PAR_Den_Avg")], 
                     by=myDF[c("Month")], 
                     FUN=mean, na.rm=T, keep.names=T)
    
    ### Colnames
    colnames(hDF) <- c("DateHour", "AirT", "RH", "PAR")
    colnames(dDF) <- c("Date", "AirT", "RH", "PAR")
    colnames(mDF) <- c("Month", "AirT", "RH", "PAR")
    
    ### Air temperature from degree C to K
    hDF$AirT <- hDF$AirT + 273.15
    dDF$AirT <- dDF$AirT + 273.15
    mDF$AirT <- mDF$AirT + 273.15
    
    ### Save  data
    write.csv(hDF, "R_other/tair_rh_par_data_hourly.csv", row.names=F)
    write.csv(dDF, "R_other/tair_rh_par_data_daily.csv", row.names=F)
    write.csv(mDF, "R_other/tair_rh_par_data_monthly.csv", row.names=F)
    
        
    if (timestep=="Monthly") {
        return(mDF)
    } else if (timestep=="Daily") {
        return(dDF)
    } else if (timestep=="Hourly") {
        return(hDF)
    }
    
}