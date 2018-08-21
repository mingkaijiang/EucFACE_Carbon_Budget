make_overstorey_gpp_flux_2 <- function() {
    ### read in MAESPA GPP output
    inDF <- read.csv("data/maespa.year.ring.csv")
    
    ### swap ring characters
    inDF$Ring.e <- gsub("R1","1", inDF$Ring.e)
    inDF$Ring.e <- gsub("R2","2", inDF$Ring.e)
    inDF$Ring.e <- gsub("R3","3", inDF$Ring.e)
    inDF$Ring.e <- gsub("R4","4", inDF$Ring.e)
    inDF$Ring.e <- gsub("R5","5", inDF$Ring.e)
    inDF$Ring.e <- gsub("R6","6", inDF$Ring.e)
    
    outDF1 <- summaryBy(GPP~year.e+Ring.e, data=inDF, FUN=mean, keep.names=T, na.rm=T)
    outDF2 <- summaryBy(GPP.e~year.e+Ring.e, data=inDF, FUN=mean, keep.names=T, na.rm=T)
    colnames(outDF1) <- colnames(outDF2) <- c("year", "Ring", "GPP")
    
    # Only use data period 2012-2016
    outDF <- rbind(outDF1, outDF2)
    outDF$Trt <- rep(c("aCO2", "eCO2"), each=24)
    
    outDF$Date <- as.Date(paste0(outDF$year, "-01-01"), format = "%Y-%m-%d")
    
    return(outDF)
}