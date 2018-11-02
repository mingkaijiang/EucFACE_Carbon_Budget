download_insect_data <- function(){

    infile1 <- "FACE_P0051_RA_ARTHROPODS-2_L1_20131101-20150114.csv"
    infile2 <- "FACE_P0051_RA_ARTHROPODS-3_L1_20131101-20150114.csv"
    infile3 <- "FACE_P0051_RA_ARTHROPODS-5_L1_20130930-20141121.csv.csv"
    
    downloadHIEv(hiev=searchHIEv(infile1))
    downloadHIEv(hiev=searchHIEv(infile2))
    downloadHIEv(hiev=searchHIEv(infile3))
    
    
}
