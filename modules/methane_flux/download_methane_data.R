download_methane_data <- function(){
    
    downloadHIEv(hiev=searchHIEv("FACE_P0027_RA_GHG-FLUXES_L3_20130101-20131231 V3.csv"))
    downloadHIEv(hiev=searchHIEv("FACE_P0027_RA_GHG-FLUXES_L3_20140101-20141231.csv"))
    downloadHIEv(hiev=searchHIEv("FACE_P0027_RA_GHG-FLUXES_L3_20150101-20151231.csv"))
    downloadHIEv(hiev=searchHIEv("FACE_P0027_RA_GHG-FLUXES_L3_20160113.csv"))
    downloadHIEv(hiev=searchHIEv("FACE_P0027_RA_GHG-FLUXES_L3_20160218.csv"))
    downloadHIEv(hiev=searchHIEv("FACE_P0027_RA_GHG-FLUXES_L3_20160314.csv"))
    downloadHIEv(hiev=searchHIEv("FACE_P0027_RA_GHG-FLUXES_L3_20160420.csv"))
    
}
  