read_Yang_soil_P_data <- function(faceDF) {
    #### data from Dai
    #### files are in nc, and are large
    
    require(ncdf4)
    require(raster)
    require(spatstat)
    
    #### read in data
    inName1 <- "data/Dai/pforms_den.nc"
    
    nc <- nc_open(inName1)
    
    lab <- ncvar_get(nc, "lab")
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    
    #### convert into x y dataframe
    DF <- im(lab, xcol = lat, 
             yrow = lon) 
    DF <- as.data.frame(DF)
    colnames(DF) <- c("y","x","value")
    
    #### convert into raster
    library(sp)
    library(rgdal)
    coordinates(DF)=~x+y
    gridded(DF) = TRUE
    r <- raster(DF)
    
    ### extract face grids
    face.coords <- faceDF[,c("Lon", "Lat")]
    
    subDF <- extract(r,               # raster layer
                     face.coords,     # coordinates to buffer
                     buffer = 25000,      # buffer size, units depend on CRS
                     fun=mean,        # what to value to extract
                     df=F) 
    
    faceDF$labP <- subDF


    return(faceDF)
    
}