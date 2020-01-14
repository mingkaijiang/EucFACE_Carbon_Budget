
make_stem_surface_area <- function(ring_area){
  
    download_wood_volume()
    
    ### read in DF
    myDF <- read.csv("data/lidar_data_eucface_HIEv.csv")
    
    ### trees to discard due to death or duplicated registration in lidar image
    myDF1 <- myDF[myDF$Ring==1&!myDF$tree%in%c(16, 5, 15, 17, 27), ]
    myDF2 <- myDF[myDF$Ring==2&!myDF$tree%in%c(12, 23, 1, 2, 7, 24, 25), ]
    myDF3 <- myDF[myDF$Ring==3&!myDF$tree%in%c(2, 6, 8, 18, 1, 23), ]
    myDF4 <- myDF[myDF$Ring==4&!myDF$tree%in%c(18, 2, 6, 8, 23, 5, 14, 25), ]
    myDF5 <- myDF[myDF$Ring==5&!myDF$tree%in%c(10, 23, 26, 37, 40, 21, 30), ]
    myDF6 <- myDF[myDF$Ring==6&!myDF$tree%in%c(8), ]
    
    ### we still have 4 more trees in R3, 7 more trees in R4 and 2 more trees in R5
    ### removing the smallest trees, as those are not in census data
    myDF3 <- myDF3[order(myDF3$dbh_m),]
    myDF3 <- myDF3[-c(1:3),]
    
    myDF4 <- myDF4[order(myDF4$dbh_m),]
    myDF4 <- myDF4[-c(1:7),]
    
    myDF5 <- myDF5[order(myDF5$dbh_m),]
    myDF5 <- myDF5[-c(1:2),]
    
    ### more dead trees, R1 = 24, R2 = 25, R3 = 26, R4 = 27, R5 = 27, R6 = 16
    ### remove trees, R1 = 1, R2 = 3, R3 = 1, R4 = 1, R5 = 6, R6 = 5
    ### R1 tree to remove: diameter = 15.2
    myDF1 <- myDF1[order(myDF1$dbh_m),]
    myDF1$dbh_m <- round(myDF1$dbh_m, 4)
    myDF1 <- myDF1[!myDF1$dbh_m%in%c(0.1550),]
    
    ### R2 tree to remove: diameter = 12.66, 14.74, 14.56
    myDF2 <- myDF2[order(myDF2$dbh_m),]
    myDF2$dbh_m <- round(myDF2$dbh_m, 4)
    myDF2 <- myDF2[!myDF2$dbh_m%in%c(0.1320, 0.1530),]
    
    
    ### R5 tree to remove: diameter = 16.02, 15.72, 18.83, 15.1, 14.94, 18.33
    myDF5$dbh_m <- round(myDF5$dbh_m, 4)
    myDF5 <- myDF5[!myDF5$dbh_m%in%c(0.1563, 0.1340, 0.1580, 0.1590, 0.1873, 0.1983),]
    
    ### R6 tree to remove: diameter = 16.85, 45.53, 16.36, 17.73, 15.89
    myDF6 <- myDF6[order(myDF6$dbh_m),]
    myDF6$dbh_m <- round(myDF6$dbh_m, 4)
    myDF6 <- myDF6[!myDF6$dbh_m%in%c(0.1547, 0.1423, 0.1757, 0.1379, 0.6817),]
    
    ### combine
    myDF <- rbind(myDF1, myDF2, myDF3, myDF4, myDF5, myDF6)
  
    ### average by ring area
    outDF <- summaryBy(total.woodarea_m2 ~ Ring, FUN=sum, data=myDF, keep.names=TRUE) %>%
        mutate(wood_surface_area = total.woodarea_m2 / ring_area,
               Date = "2015-05-26",
               Ring = as.numeric(Ring)) %>%
        dplyr::select(Date, Ring, wood_surface_area)
    
    
    names(outDF)[3] <- "wood_surface_area"
    

    return(outDF)
}