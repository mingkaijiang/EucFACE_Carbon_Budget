make_understorey_GPP_flux <- function() {
    
    ### all understorey gpp calculated
    myDF <- read.csv("data/MAESPA_output/understorey.gpp.csv")
    myDF <- subset(myDF, Species == 2)
    myDF$Ring <- gsub("R","", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)
    
    out <- myDF[,c("year", "Ring", "GPP.sum")]
    out$Date <- paste0(out$year, "-01-01")
    
    out$Trt[out$Ring%in%c(2,3,6)] <- "aCO2"
    out$Trt[out$Ring%in%c(1,4,5)] <- "eCO2"

    colnames(out) <- c("year", "Ring", "GPP", "Date", "Trt")
    
    
    return(out)
}