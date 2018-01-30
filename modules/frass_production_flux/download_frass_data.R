download_frass_data <- function() {
    
    infile1 <- "FRASSFALL_L2_20120914-20150209.csv"
    infile2 <- "FRASSCHEMISTRY_L2_20121112-20141016.csv"
    
    if (!file.exists(paste0("download/", infile1))) {
        downloadHIEv(hiev=searchHIEv(infile1))
    }
    
    if (!file.exists(paste0("download/", infile2))) {
        downloadHIEv(hiev=searchHIEv(infile2))
    }
}
