prepare_met_air_data <- function(timestep) {
    #### Download the data - takes time to run
    myDF <- download_wind_data()
    
    #### Assign ring information
    myDF$Ring <- sub("FACE_R", "", myDF$Source)
    myDF$Ring <- sub("_T1.*", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)  
    myDF <- myDF[order(myDF$DateTime),]
    myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
    myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
    myDF$DateHour <- as.POSIXct(paste0(myDF$Date, " ", hour(myDF$DateTime), ":00:00"),format = "%Y-%m-%d %H:%M:%S")
    
    ### Calculate hourly mean
    hDF <-aggregate(myDF[c("Ts_mean","rh_hmp_mean","LI190SB_PAR_Den_Avg", "Pressure_hPa_Avg", "wnd_spd")], 
                    by=myDF[c("DateHour")], 
                    FUN=mean, na.rm = T, keep.names=T) 
    
    ### Calculate daily mean
    dDF <-aggregate(myDF[c("Ts_mean","rh_hmp_mean","LI190SB_PAR_Den_Avg", "Pressure_hPa_Avg", "wnd_spd")], 
                    by=myDF[c("Date")], 
                    FUN=mean, na.rm = T, keep.names=T) 
    
    ### Calculate monthly mean
    mDF <- aggregate(myDF[c("Ts_mean","rh_hmp_mean","LI190SB_PAR_Den_Avg", "Pressure_hPa_Avg", "wnd_spd")], 
                     by=myDF[c("Month")], 
                     FUN=mean, na.rm = T, keep.names=T) 
    
 
    ### Save  data
    write.csv(hDF, "R_other/met_air_flux_data_hourly.csv", row.names=F)
    write.csv(dDF, "R_other/met_air_flux_data_daily.csv", row.names=F)
    write.csv(mDF, "R_other/met_air_flux_data_monthly.csv", row.names=F)
        
    if (timestep=="Monthly") {
        return(mDF)
    } else if (timestep=="Daily") {
        return(dDF)
    } else if (timestep=="Hourly") {
        return(hDF)
    }
    
}