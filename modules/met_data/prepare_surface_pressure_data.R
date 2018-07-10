prepare_surface_pressure_data <- function(plot.image, timestep) {
    #### Download the data - takes time to run
    myDF <- download_pressure_data()
    
    #### Assign ring information
    myDF$Ring <- sub("FACE_R", "", myDF$Source)
    myDF$Ring <- sub("_T1.*", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)  
    myDF <- myDF[order(myDF$DateTime),]
    myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
    myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
    myDF$DateHour <- as.POSIXct(paste0(myDF$Date, " ", hour(myDF$DateTime), ":00:00"),format = "%Y-%m-%d %H:%M:%S")
    
    myDF$Pressure_hPa_Avg <- as.numeric(myDF$Pressure_hPa_Avg)
    myDF$Pressure_Pa <- myDF$Pressure_hPa_Avg * 100
    
    
    ### Calculate hourly average
    hDF <-aggregate(Pressure_Pa~DateHour, FUN=mean, na.rm = T, keep.names=T, data=myDF) 
    
    
    ### Calculate daily average
    dDF <- aggregate(Pressure_Pa~Date, FUN=mean, na.rm=T, keep.names=T, data=myDF)
    
    ### Calculate monthly average
    mDF <- aggregate(Pressure_Pa~Month, FUN=mean, na.rm=T, keep.names=T, data=myDF)
    
    
    if (plot.image == T) {
        #### Plotting
        if (timestep=="Monthly") {
            p1 <- ggplot(mDF, aes(Month, Pressure_Pa)) +
                geom_line()
            plot(p1)
        } else if (timestep=="Daily") {
            p1 <- ggplot(dDF, aes(Date, Pressure_Pa)) +
                geom_line()
            plot(p1) 
        } else if (timestep=="Hourly") {
            p1 <- ggplot(hDF, aes(DateHour, Pressure_Pa)) +
                geom_line()
            plot(p1) 
        }
        
    } 
    
    ### Save  data
    write.csv(hDF, "R_other/pressure_data_hourly.csv", row.names=F)
    write.csv(dDF, "R_other/pressure_data_daily.csv", row.names=F)
    write.csv(mDF, "R_other/pressure_data_monthly.csv", row.names=F)
        
    if (timestep=="Monthly") {
        return(mDF)
    } else if (timestep=="Daily") {
        return(dDF)
    } else if (timestep=="Hourly") {
        return(hDF)
    }
    
}