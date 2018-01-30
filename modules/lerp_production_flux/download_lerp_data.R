download_lerp_data <- function() {
    
    infile1 <- "FACE_P0017_RA_psyllid_abundance_L3_20121101-20141219.csv"
    infile2 <- "FACE_P0017_RA_psyllid_lerp_weight_L3_20121201-20141219.csv"
    
    # download psyllid_abundance data
    if (!file.exists(paste0("download/", infile1))) {
        downloadHIEv(hiev=searchHIEv(infile1))
    }

    # download lerp weight data
    if (!file.exists(paste0("download/", infile2))) {
        downloadHIEv(hiev=searchHIEv(infile2))
    }
}
