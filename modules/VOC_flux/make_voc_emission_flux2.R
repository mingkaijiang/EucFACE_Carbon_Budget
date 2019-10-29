
make_voc_emission_flux2 <- function(){
    

    #- read in the data ug isoprene m-2 yr-1
    isoDF <- read.csv("data/annual_isoprene_emission_without_soil_moisture.csv")
    
    # covert unit into g C m-2 yr-1
    isoDF$Flux_g_C_m2_yr <- 0.000001 * isoDF$isoprene * (1/68) * 5 * 12 
    
    isoDF$Date <- as.Date(paste0(isoDF$year, "-01-01"), format = "%Y-%m-%d")
    
    isoDF$FluxName <- "Isoprene"
    subDF <- isoDF[,c("year", "Ring", "FluxName", "Flux_g_C_m2_yr", "Date")]
    colnames(subDF) <- c("Year", "Ring", "FluxName", "Flux_g_C_m2_yr", "Date")
    
    # read in data
    monoDF <- read.csv("data/VOC_emissions.csv")
    
    monoDF$Date <- as.Date(paste0(monoDF$Year, "-01-01"), format = "%Y-%m-%d")
    
    
    monoDF <- rbind(subDF, monoDF)
    
    # sum all fluxes for each ring and date
    outDF <- summaryBy(Flux_g_C_m2_yr~Ring+Date, data=monoDF, FUN=sum, keep.names=T)
    
    colnames(outDF) <- c("Ring",
                         "Date", 
                         "voc_flux")
    
    
    return(outDF)

}