
# this function downloads the diameter files from HIEv
download_diameter_data <- function(){
  downloadHIEv(hiev=searchHIEv("FACE_P0025_RA_TREEMEAS_2012-13_RAW-V1.csv"))
  downloadHIEv(hiev=searchHIEv("FACE_P0025_RA_TREEMEAS_2013-14_RAW_V1.csv"))
  downloadHIEv(hiev=searchHIEv("FACE_P0045_RA_MORTALITY_RAW_20150501_v1.csv"))
  downloadHIEv(hiev=searchHIEv("FACE_P0079_RA_SAPWOOD_N_RAW_2015-11_v1.csv"))
  }