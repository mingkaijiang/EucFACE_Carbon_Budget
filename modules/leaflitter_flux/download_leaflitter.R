
download_leaflitter <- function(){
  #downloadCSV("FACE_P0017_RA_Litter_[0-9]{8}-[0-9]{8}-L1-V2.csv")
    
    infile1 <- "FACE_P0017_RA_Litter_20160101-20161212-L1-V2.csv"
    infile2 <- "FACE_P0017_RA_Litter_20150101-20151217-L1-V2.csv"
    infile3 <- "FACE_P0017_RA_Litter_20140101-20141216-L1-V2.csv"
    infile4 <- "FACE_P0017_RA_Litter_20121001-20131231-L1-V2.csv"
    
    
    downloadHIEv(hiev=searchHIEv(infile1))
    downloadHIEv(hiev=searchHIEv(infile2))
    downloadHIEv(hiev=searchHIEv(infile3))
    downloadHIEv(hiev=searchHIEv(infile4))
}



