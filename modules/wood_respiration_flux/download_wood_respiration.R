#- Function to download the wood respiration dataset from HIEv.
#  Note these measurements were done in WTC3 with E. tereticornis trees.
download_wood_respiration <- function(){
  downloadHIEv(hiev=searchHIEv("WTC_TEMP_CM_WTCFLUX-STEM_20140528_L1_v1.csv")) # stem wood
  downloadHIEv(hiev=searchHIEv("WTC_TEMP_CM_GX-RBRANCH_20140513-20140522_L1_v1.csv")) # branch wood and leaf
}