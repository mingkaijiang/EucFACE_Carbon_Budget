prepare_soil_moisture_data <- function() {
    #### Download the data - takes time to run
    myDF <- download_soil_moisture_data()
    
    #### Assign ring information
    myDF$Ring <- sub("FACE_R", "", myDF$Source)
    myDF$Ring <- sub("_B1.*", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)    
    
    #### Create a daily and a monthly DF to store output
    t.series <- unique(myDF$Date)
    nc <- ncol(myDF)-2
    dDF <- matrix(ncol=nc, nrow=length(t.series)*6)
    dDF <- as.data.frame(dDF)
    colnames(dDF) <- names(myDF)[3:length(names(myDF))]
    dDF$Date <- rep(t.series, each=6)
    dDF$Ring <- rep(1:6, length(t.series))
    dDF$Month <- format(as.Date(dDF$Date), "%Y-%m")

    #### Need to split into 6 rings 
    for (i in 1:6) {
        for (j in t.series) {
            for (k in 1:(nc-4)) {
                dDF[dDF$Ring == i & dDF$Date == j, k] <- mean(myDF[myDF$Ring == i & myDF$Date == j, k+2], na.rm=T)
            }        
        }
    }
    
    
    with(dDF[dDF$Ring == 1, ], plot(T5cm_2_Avg~Date))
    
}