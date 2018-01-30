download_soil_bulk_density_data <- function() {
    
    infile <- "FACE_P0088_RA_BULKDENSITY_L1_20170914.csv"
    
    if (!file.exists(paste0("download/", infile))) {
        # Bulk density data
        downloadHIEv(hiev=searchHIEv(infile))
    }
}
