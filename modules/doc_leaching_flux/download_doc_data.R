download_doc_data <- function(){
    # download lysimeter based doc leaching data
    
    infile <- "FACE_RA_P0023_SOILLYSIMETERNUTRIENTS_L3_20120710-20140402.csv"
    
    if (!file.exists(paste0("download/", infile))) {
        downloadHIEv(hiev=searchHIEv(infile))
        
    }
    
}
