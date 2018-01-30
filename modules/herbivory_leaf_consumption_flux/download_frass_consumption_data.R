download_frass_consumption_data <- function(){
    # download frass vs. leaf consumption data (for modifying leaf area data)
    infile <- "GHS36_AG-THESIS_CA_FRASS-LEAFAREA_L2_20110101-20151231.csv"
    
    if (!file.exists(paste0("download/", infile))) {
        downloadHIEv(hiev=searchHIEv("GHS36_AG-THESIS_CA_FRASS-LEAFAREA_L2_20110101-20151231.csv"))
    }

}
