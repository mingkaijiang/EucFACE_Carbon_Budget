prepare_rainfall_data <- function(plot.image, timestep) {
    #### Download the data - takes time to run
    myDF <- download_rainfall_data()
    
    #### Assign ring information
    myDF$Ring <- sub("FACE_R", "", myDF$Source)
    myDF$Ring <- sub("_T1.*", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)  
    myDF <- myDF[order(myDF$DateTime),]
    myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
    myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
    myDF$DateHour <- as.POSIXct(paste0(myDF$Date, " ", hour(myDF$DateTime), ":00:00"),format = "%Y-%m-%d %H:%M:%S")
    
    myDF$Rain_mm_Tot <- as.numeric(myDF$Rain_mm_Tot)
    
    ### Calculate hourly sum
    hDF <-aggregate(Rain_mm_Tot~DateHour, FUN=sum, na.rm = T, keep.names=T, data=myDF) 
    
    
    ### Calculate daily sum
    dDF <- aggregate(Rain_mm_Tot~Date, FUN=sum, na.rm=T, keep.names=T, data=myDF)
    
    ### Calculate monthly sum
    mDF <- aggregate(Rain_mm_Tot~Month, FUN=sum, na.rm=T, keep.names=T, data=myDF)
    
    
    if (plot.image == T) {
        #### Plotting
        if (timestep=="Monthly") {
            p1 <- ggplot(mDF, aes(Month, Rain_mm_Tot)) +
                geom_line()
            plot(p1)
        } else if (timestep=="Daily") {
            p1 <- ggplot(dDF, aes(Date, Rain_mm_Tot)) +
                geom_line()
            plot(p1) 
        } else if (timestep=="Hourly") {
            p1 <- ggplot(hDF, aes(DateHour, Rain_mm_Tot)) +
                geom_line()
            plot(p1) 
        }
        
    } 
    
    ### Save  data
    write.csv(hDF, "R_other/rainfall_data_hourly.csv", row.names=F)
    write.csv(dDF, "R_other/rainfall_data_daily.csv", row.names=F)
    write.csv(mDF, "R_other/rainfall_data_monthly.csv", row.names=F)
    
    
        
    if (timestep=="Monthly") {
        return(mDF)
    } else if (timestep=="Daily") {
        return(dDF)
    } else if (timestep=="Hourly") {
        return(hDF)
    }
    
}