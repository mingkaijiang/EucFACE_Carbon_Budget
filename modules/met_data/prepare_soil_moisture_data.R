prepare_soil_moisture_data <- function(plot.image) {
    #### Download the data - takes time to run
    myDF <- download_soil_moisture_data()
    
    #### Assign ring information
    myDF$Ring <- sub("FACE_R", "", myDF$Source)
    myDF$Ring <- sub("_B1.*", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)  
    myDF <- myDF[order(myDF$Date),]
    myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
    myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
    
    ### Color scheme
    cols <- colorRampPalette(c('red','blue', "darkblue"))(24)
    
    if (plot.image == T) {
        #### Plotting
        ### Need to split into 6 rings 
        for (i in 1:6) {
            ### Depth
            if (i == 2) {
                depth <- c(5, 30, 44, 75)
            } else {
                depth <- c(5, 30, 35, 75)
            } 
            
            ### Calculate daily average
            dDF <- aggregate(myDF[myDF$Ring == i,3:46], by=list(myDF[myDF$Ring == i, "Date"]), mean, na.action=na.rm)
            
            ### Calculate monthly average
            mDF <- aggregate(myDF[myDF$Ring == i,3:46], by=list(myDF[myDF$Ring == i, "Month"]), mean, na.action=na.rm)
            
            ### Daily time series
            d.series <- unique(myDF[myDF$Ring == i, "Date"])
            x.d <- c(1:length(d.series))
            
            ### Monthly time series
            m.series <- unique(myDF[myDF$Ring == i, "Month"])
            x.m <- c(1:length(m.series))
            
            
            ### Prepare theta data frame at daily timestep
            thDF <- dDF[, c("Theta5_1_Avg","Theta30_1_Avg","ThetaHL_1_Avg","Theta75_1_Avg")]
            theta <- as.matrix(thDF)
            
            ### Countour plotting
            filled.contour(x = d.series, y = depth, z=theta, xlab = "Date", ylab = "Depth (cm)",
                           col=cols, main = paste0("Ring ", i))
            
            ### Prepare theta data frame at daily timestep
            thDF <- mDF[, c("Theta5_1_Avg","Theta30_1_Avg","ThetaHL_1_Avg","Theta75_1_Avg")]
            
            theta <- as.matrix(thDF)
            filled.contour(x = m.series, y = depth, z=theta, xlab = "Date", ylab = "Depth  (cm)",
                           col=cols, main = paste0("Ring ", i))
        }
        
    } else {
        
        ### Calculate daily average
        dDF1 <- aggregate(myDF[myDF$Ring == 1,3:10],
                          by=list(myDF[myDF$Ring == 1, "Date"]), mean, na.action=na.rm)
        
        dDF2 <- aggregate(myDF[myDF$Ring == 2,3:10],
                          by=list(myDF[myDF$Ring == 2, "Date"]), mean, na.action=na.rm)
        
        dDF3 <- aggregate(myDF[myDF$Ring == 3,3:10],
                          by=list(myDF[myDF$Ring == 3, "Date"]), mean, na.action=na.rm)
        
        dDF4 <- aggregate(myDF[myDF$Ring == 4,3:10],
                          by=list(myDF[myDF$Ring == 4, "Date"]), mean, na.action=na.rm)
        
        dDF5 <- aggregate(myDF[myDF$Ring == 5,3:10],
                          by=list(myDF[myDF$Ring == 5, "Date"]), mean, na.action=na.rm)
        
        dDF6 <- aggregate(myDF[myDF$Ring == 6,3:10],
                          by=list(myDF[myDF$Ring == 6, "Date"]), mean, na.action=na.rm)
        
        colnames(dDF1)[1] <- colnames(dDF2)[1] <-colnames(dDF3)[1] <-colnames(dDF4)[1] <-colnames(dDF5)[1] <-colnames(dDF6)[1] <-"Date"
        
        ### Save to out df
        out.list <- list(R1 = data.table(dDF1), 
                         R2 = data.table(dDF2), 
                         R3 = data.table(dDF3),
                         R4 = data.table(dDF4),
                         R5 = data.table(dDF5),
                         R6 = data.table(dDF6))
        
        return(out.list)
    }
 
    
}