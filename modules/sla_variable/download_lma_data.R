download_lma_data <- function(){
    downloadHIEv(hiev=searchHIEv("FACE_P0020_RA_LMA_20160201-20161018_L2.csv"))
    downloadHIEv(hiev=searchHIEv("FACE_P0020_RA_LMA_20150129-20150416_L2.csv"))
    downloadHIEv(hiev=searchHIEv("FACE_P0020_RA_LMA_20140130-20141016_L2.csv"))
    downloadHIEv(hiev=searchHIEv("FACE_P0020_RA_LMA_L2_20130213-20131115.csv"))
    
}
