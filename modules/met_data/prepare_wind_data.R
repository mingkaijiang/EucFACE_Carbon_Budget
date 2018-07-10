prepare_wind_data <- function(plot.image, timestep) {
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
    hDF <-aggregate(wnd_spd~DateHour, FUN=mean, na.rm = T, keep.names=T, data=myDF) 
    
    ### Calculate daily mean
    dDF <- aggregate(wnd_spd~Date, FUN=mean, na.rm=T, keep.names=T, data=myDF)
    
    ### Calculate monthly mean
    mDF <- aggregate(wnd_spd~Month, FUN=mean, na.rm=T, keep.names=T, data=myDF)
    
    
    if (plot.image == T) {
        #### Plotting
        if (timestep=="Monthly") {
            p1 <- ggplot(mDF, aes(Month, wnd_spd)) +
                geom_line()
            plot(p1)
        } else if (timestep=="Daily") {
            p1 <- ggplot(dDF, aes(Date, wnd_spd)) +
                geom_line()
            plot(p1) 
        } else if (timestep=="Hourly") {
            p1 <- ggplot(hDF, aes(DateHour, wnd_spd)) +
                geom_line()
            plot(p1) 
        }
        
    } 
    
    ### Save  data
    write.csv(hDF, "R_other/wind_data_hourly.csv", row.names=F)
    write.csv(dDF, "R_other/wind_data_daily.csv", row.names=F)
    write.csv(mDF, "R_other/wind_data_monthly.csv", row.names=F)
        
    if (timestep=="Monthly") {
        return(mDF)
    } else if (timestep=="Daily") {
        return(dDF)
    } else if (timestep=="Hourly") {
        return(hDF)
    }
    
}