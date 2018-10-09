#- Function to download the wood respiration dataset from HIEv.
#  Note these measurements were done in WTC3 with E. tereticornis trees.
download_stem_basal_respiration_data <- function(){
  downloadHIEv(hiev=searchHIEv("FACE_A0089_RA_STEMCO2EFLUX_L1_20171218-20171220.txt")) 
  downloadHIEv(hiev=searchHIEv("FACE_A0089_RA_STEMCO2EFLUX_L1_20180115-20180117.txt")) 
  downloadHIEv(hiev=searchHIEv("FACE_A0089_RA_STEMCO2EFLUX_L1_20180205-20180207.txt")) 
  downloadHIEv(hiev=searchHIEv("FACE_A0089_RA_XYLEMCO2_L1_20171216-20180211.txt")) 

}