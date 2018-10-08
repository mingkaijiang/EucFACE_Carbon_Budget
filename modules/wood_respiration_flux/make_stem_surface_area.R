
make_stem_surface_area <- function(ring_area){
  
    download_wood_volume()
    
    # read in DF
    myDF <- read.csv(file.path(getToPath(), "FACE_RA_P0037_STEMVOL-LIDAR_20150526_L2.csv")) 

    # average by ring area
    outDF <- summaryBy(trunk_surfarea ~ Ring, FUN=sum, data=myDF, keep.names=TRUE) %>%
        mutate(trunk_surfarea = trunk_surfarea / ring_area,
               Date = "2015-05-26",
               Ring = as.numeric(Ring)) %>%
        #rename(wood_surface_area = trunk_surfarea) %>%
        dplyr::select(Date, Ring, trunk_surfarea)
    
    names(outDF)[3] <- "wood_surface_area"
    
    write.csv(outDF, "R_other/EucFACE_wood_surface_area.csv", row.names=F)
    
    return(outDF)
}