make_understorey_GPP_flux <- function() {
    ### read in MAESPA GPP output
    inDF <- read.csv("temp_files/maespa annual.csv")
    
    colnames(inDF) <- c("year", "Ring", "Treat", "GPP", "Rfoliage")
    
    ### swap ring characters
    inDF$Ring <- gsub("R1","1", inDF$Ring)
    inDF$Ring <- gsub("R2","2", inDF$Ring)
    inDF$Ring <- gsub("R3","3", inDF$Ring)
    inDF$Ring <- gsub("R4","4", inDF$Ring)
    inDF$Ring <- gsub("R5","5", inDF$Ring)
    inDF$Ring <- gsub("R6","6", inDF$Ring)
    
    outDF <- summaryBy(GPP~year+Ring, data=inDF, FUN=mean, keep.names=T, na.rm=T)
    
    outDF$GPP <- outDF$GPP * 0.4
    
    return(outDF)
}