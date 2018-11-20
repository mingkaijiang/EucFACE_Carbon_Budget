
make_voc_emission_flux <- function(){
    

    #- read in the data ug isoprene m-2 yr-1
    myDF <- read.csv("data/annual_isoprene_emission_with_soil_moisture.csv")
    
    # covert unit into g C m-2 yr-1
    myDF$voc_flux <- 0.000001 * myDF$isoprene * (1/68) * 5 * 12 
    
    myDF$Date <- as.Date(paste0(myDF$year, "-01-01"), format = "%Y-%m-%d")
    
    return(myDF[,c("Date", "Ring", "voc_flux")])

}