download_lerp_data <- function(){
    # download psyllid_abundance data 
    downloadHIEv(hiev=searchHIEv("FACE_P0017_RA_psyllid_abundance_L3_20121101-20141219.csv"))
    
    # download lerp weight data
    downloadHIEv(hiev=searchHIEv("FACE_P0017_RA_psyllid_lerp_weight_L3_20121201-20141219.csv"))
}
