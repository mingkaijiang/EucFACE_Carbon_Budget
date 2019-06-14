make_six_ring_combined_plot <- function(inList) {
    
    pdf("output/soil_moisture.pdf", width=8, height=16)
    layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = TRUE))
    
    for (i in 1:6) {
        ### Depth
        if (i == 2) {
            depth <- c(5, 30, 44, 75)
        } else {
            depth <- c(5, 30, 35, 75)
        } 
        
        myDF <- inList[[i]]
        
        myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
        myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
        
        ### Calculate monthly average
        mDF <- aggregate(myDF[,2:9], by=list(myDF$Month), mean, na.action=na.rm)
        
        ### Monthly time series
        m.series <- unique(myDF$Month)
        x.m <- c(1:length(m.series))
        
        ### Prepare theta data frame at daily timestep
        thDF <- mDF[, c("Theta5_1_Avg","Theta30_1_Avg","ThetaHL_1_Avg","Theta75_1_Avg")]
        
        theta <- as.matrix(thDF)
        filled.contour(x = m.series, y = depth, z=theta, xlab = "Date", ylab = "Depth  (cm)",
                       col=cols, main = paste0("Ring ", i), drawlabels=F)   
        
    }
    dev.off()
}