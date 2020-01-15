
make_voc_emission_flux <- function(){

    # read in data
    myDF <- read.csv("data/support_data/VOC_emissions.csv")
    
    myDF$Date <- as.Date(paste0(myDF$Year, "-01-01"), format = "%Y-%m-%d")
    
    # sum all fluxes for each ring and date
    outDF <- summaryBy(Flux_g_C_m2_yr~Ring+Date, data=myDF, FUN=sum, keep.names=T)
    
    colnames(outDF) <- c("Ring",
                         "Date", 
                         "voc_flux")
    

    return(outDF)

}