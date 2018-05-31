prepare_soil_moisture_data <- function() {
    #### Download the data - takes time to run
    myDF <- download_soil_moisture_data()
    
    #### Assign ring information
    myDF$Ring <- sub("FACE_R", "", myDF$Source)
    myDF$Ring <- sub("_B1.*", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)  
    myDF <- myDF[order(myDF$Date),]
    myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
    
    ### Daily time series
    d.series <- unique(myDF$Date)
    x.d <- c(1:length(d.series))
    
    ### Monthly time series
    m.series <- unique(myDF$Month)
    x.m <- c(1:length(m.series))
    
    ### Color scheme
    cols <- colorRampPalette(c('red','blue', "darkblue"))(24)
    
    ### depth
    depth <- seq(from=0, to=3.5, by=4/8)
    t.depth <- c(5, 30, 75)
    
    #### Need to split into 6 rings 
    for (i in 1:6) {
        
        ### Calculate daily average
        dDF <- aggregate(myDF[myDF$Ring == i,3:46], by=list(myDF[myDF$Ring == i, "Date"]), mean, na.action=na.rm)

        ### Calculate monthly average
        mDF <- aggregate(myDF[myDF$Ring == i,3:46], by=list(myDF[myDF$Ring == i, "Month"]), mean, na.action=na.rm)
        
        
        ### Prepare vwc dataframe at daily timestep
        vwcDF <- dDF[, c("VWC_1_Avg","VWC_2_Avg","VWC_3_Avg",
                         "VWC_4_Avg","VWC_5_Avg","VWC_6_Avg",
                         "VWC_7_Avg","VWC_8_Avg")]
        
        vwc <- as.matrix(vwcDF)
        vwc[vwc > 1] <- NA
        filled.contour(x = x.d, y = depth, z=vwc, xlab = "Date", ylab = "Depth",
                       col=cols)
        
        ### Prepare vwc dataframe at month timestep
        vwcDF <- mDF[, c("VWC_1_Avg","VWC_2_Avg","VWC_3_Avg",
                         "VWC_4_Avg","VWC_5_Avg","VWC_6_Avg",
                         "VWC_7_Avg","VWC_8_Avg")]
        
        vwc <- as.matrix(vwcDF)
        vwc[vwc > 1] <- NA
        filled.contour(x = x.m, y = depth, z=vwc, xlab = "Date", ylab = "Depth",
                       col=cols)
        
        
        ### Prepare theta data frame at daily timestep
        thDF <- dDF[, c("Theta5_1_Avg","Theta30_1_Avg","Theta75_1_Avg")]
        
        theta <- as.matrix(thDF)
        filled.contour(x = x.d, y = t.depth, z=theta, xlab = "Date", ylab = "Depth",
                       col=cols)
        
        ### Prepare theta data frame at daily timestep
        thDF <- mDF[, c("Theta5_1_Avg","Theta30_1_Avg","Theta75_1_Avg")]
        
        theta <- as.matrix(thDF)
        filled.contour(x = x.m, y = t.depth, z=theta, xlab = "Date", ylab = "Depth",
                       col=cols)
    }
   
    
}