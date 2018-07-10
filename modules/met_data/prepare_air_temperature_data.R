prepare_air_temperature_data <- function(plot.image, timestep) {
    #### Download the data - takes time to run
    myDF <- download_air_temperature_data()
    
    #### Assign ring information
    myDF$Ring <- sub("FACE_R", "", myDF$Source)
    myDF$Ring <- sub("_T1.*", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)  
    myDF <- myDF[order(myDF$DateTime),]
    myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
    myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
    myDF$DateHour <- as.POSIXct(paste0(myDF$Date, " ", hour(myDF$DateTime), ":00:00"),format = "%Y-%m-%d %H:%M:%S")
    
    myDF$Ts_mean <- as.numeric(myDF$Ts_mean)
    
    ### Calculate hourly sum
    hDF <-aggregate(Ts_mean~DateHour, FUN=mean, na.rm = T, keep.names=T, data=myDF) 
    
    ### Calculate daily sum
    dDF <- aggregate(Ts_mean~Date, FUN=mean, na.rm=T, keep.names=T, data=myDF)
    
    ### Calculate monthly sum
    mDF <- aggregate(Ts_mean~Month, FUN=mean, na.rm=T, keep.names=T, data=myDF)
    
    
    if (plot.image == T) {
        #### Plotting
        if (timestep=="Monthly") {
            p1 <- ggplot(mDF, aes(Month, Ts_mean)) +
                geom_line()
            plot(p1)
        } else if (timestep=="Daily") {
            p1 <- ggplot(dDF, aes(Date, Ts_mean)) +
                geom_line()
            plot(p1) 
        } else if (timestep=="Hourly") {
            p1 <- ggplot(hDF, aes(DateHour, Ts_mean)) +
                geom_line()
            plot(p1) 
        }
        
    } 
        
    if (timestep=="Monthly") {
        return(mDF)
    } else if (timestep=="Daily") {
        return(dDF)
    } else if (timestep=="Hourly") {
        return(hDF)
    }
    
}