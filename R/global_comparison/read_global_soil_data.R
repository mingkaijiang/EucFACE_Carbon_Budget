read_global_soil_data <- function(faceDF) {
    #### data from Dai
    #### files are in nc, and are large
    
    require(ncdf4)
    require(raster)
    
    #### read in data
    inName1 <- "data/Dai/TN1.nc"
    inName2 <- "data/Dai/BD1.nc"
    
    ### use raster brick to get all the data
    tn <- brick(inName1, varname = "TN")
    tp <- brick(inName2, varname = "BD")
    
    ### subset the first layer
    tn1 <- subset(tn, 1) * 0.01
    tp1 <- subset(tp, 1) * 0.01 
    
    #np1 <- tn1 / tp1

    ### prepare FACE color list
    #col.list.face <- c("red", brewer.pal(9,"Paired"))
    
    ###
    #pdf("output/global_soil_NP.pdf")
    #par(mfrow=c(3,1))
    #
    #plot(tn1, xlim=c(-180, 180), ylim=c(-90, 90))
    #points(faceDF$Lon, faceDF$Lat, 
    #       type="p", pch = c(17, rep(15, 9)),
    #       col=col.list.face)
    #
    #plot(tp1, xlim=c(-180, 180), ylim=c(-90, 90))
    #points(faceDF$Lon, faceDF$Lat, 
    #       type="p", pch = c(17, rep(15, 9)),
    #       col=col.list.face)
    #
    #plot(np1, xlim=c(-180, 180), ylim=c(-90, 90))
    #points(faceDF$Lon, faceDF$Lat, 
    #       type="p", pch = c(17, rep(15, 9)),
    #       col=col.list.face)
    #
    #dev.off()
    
    ### extract face grids
    tn1@crs 
    
    face.coords <- SpatialPointsDataFrame(faceDF[,4:3], proj4string=tn1@crs,
                                          faceDF)
    
    tn1_mean <- extract(tn1,             # raster layer
                        face.coords,     # coordinates to buffer
                        buffer = 100000,     # buffer size, units depend on CRS
                        fun=mean,        # what to value to extract
                        df=TRUE) 
    
    tp1_mean <- extract(tp1,             # raster layer
                        face.coords,     # coordinates to buffer
                        buffer = 100000,     # buffer size, units depend on CRS
                        fun=mean,        # what to value to extract
                        df=TRUE) 
    
    faceDF$TN1 <- tn1_mean$X4.5
    faceDF$BD1 <- tp1_mean$X4.5
    
    return(faceDF)
    
}