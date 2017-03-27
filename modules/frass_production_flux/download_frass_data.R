download_frass_data <- function(){
    # download frassfall data (for frass production per basket data)
    downloadHIEv(hiev=searchHIEv("FRASSFALL_L2_20120914-20150209.csv"))
    
    # download frass chemistry data (for C content)
    downloadHIEv(hiev=searchHIEv("FRASSCHEMISTRY_L2_20121112-20141016.csv"))
}
