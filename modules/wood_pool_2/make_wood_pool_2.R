
make_wood_pool_2 <- function(ring_area,c_fraction,wood_density){
  
  download_wood_volume()
  
  # trunk_cmass in kg C
  stemvols <- read.csv(file.path(getToPath(), "FACE_RA_P0037_STEMVOL-LIDAR_20150526_L2.csv")) %>%
    mutate(trunk_cmass = c_fraction * wood_density * trunk_volume)
    
  # total by ring,
  # Divide by ring area, multiply with 1000 g/kg, to get gC m-2
  ringstemmass <- summaryBy(trunk_cmass ~ Ring, FUN=sum, data=stemvols, keep.names=TRUE) %>%
    mutate(trunk_cmass = 1000 * trunk_cmass / ring_area,
           Date = "2015-05-26",
           Ring = as.numeric(Ring)) %>%
    rename(wood_pool_2 = trunk_cmass) %>%
    dplyr::select(Date, Ring, wood_pool_2)
  
  
ringstemmass
}