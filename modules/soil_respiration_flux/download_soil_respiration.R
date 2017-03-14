#- Function to download the soil respiration dataset from HIEv.
#  Note this only includes three years of data from 2012-2015. 
download_soil_respiration <- function(){
  downloadHIEv(hiev=searchHIEv("FACE_P0031_RA_Rsoil-PROCESSED"),topath="download/")
}