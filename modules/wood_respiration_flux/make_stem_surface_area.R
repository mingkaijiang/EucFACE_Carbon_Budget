
make_stem_surface_area <- function(ring_area){
  
    download_wood_volume()
    
    # read in DF
    #myDF2 <- read.csv(file.path(getToPath(), "FACE_RA_P0037_STEMVOL-LIDAR_20150526_L2.csv")) 
    myDF <- read.csv("data/lidar_data_eucface_HIEv.csv")
  
    # average by ring area
    outDF <- summaryBy(stemarea_m2 ~ Ring, FUN=sum, data=myDF, keep.names=TRUE) %>%
        mutate(stemarea_m2 = stemarea_m2 / ring_area,
               Date = "2015-05-26",
               Ring = as.numeric(Ring)) %>%
        #rename(wood_surface_area = trunk_surfarea) %>%
        dplyr::select(Date, Ring, stemarea_m2)
    
    #outDF <- summaryBy(trunk_surfarea ~ Ring, FUN=sum, data=myDF2, keep.names=TRUE) %>%
    #  mutate(trunk_surfarea = trunk_surfarea / ring_area,
    #         Date = "2015-05-26",
    #         Ring = as.numeric(Ring)) %>%
    #  #rename(wood_surface_area = trunk_surfarea) %>%
    #  dplyr::select(Date, Ring, trunk_surfarea)
    
    names(outDF)[3] <- "wood_surface_area"
    
    #write.csv(outDF, "R_other/EucFACE_wood_surface_area.csv", row.names=F)
    
    return(outDF)
}