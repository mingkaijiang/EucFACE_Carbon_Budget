make_overstorey_gpp_flux <- function() {
    ### read in MAESPA GPP output
    inDF <- read.csv("data/MAESPA_output/maespa.year.ring.csv")
    
    ### swap ring characters
    inDF$Ring <- gsub("R1","1", inDF$Ring)
    inDF$Ring <- gsub("R2","2", inDF$Ring)
    inDF$Ring <- gsub("R3","3", inDF$Ring)
    inDF$Ring <- gsub("R4","4", inDF$Ring)
    inDF$Ring <- gsub("R5","5", inDF$Ring)
    inDF$Ring <- gsub("R6","6", inDF$Ring)
    
    outDF1 <- summaryBy(GPP.sum.400~year+Ring, data=inDF, FUN=mean, keep.names=T, na.rm=T)
    outDF2 <- summaryBy(GPP.sum.550~year+Ring, data=inDF, FUN=mean, keep.names=T, na.rm=T)
    colnames(outDF1) <- colnames(outDF2) <- c("year", "Ring", "GPP")
    
    ### select only the real treatment data
    for (i in c(2,3,6)) {
        for (j in 2013:2016) {
            outDF2$GPP[outDF2$Ring==i&outDF2$year==j] <- outDF1$GPP[outDF1$Ring==i&outDF1$year==j]
        }
    }
    
    for (i in c(1,4,5)) {
        outDF2$PreTrt[outDF2$Ring==i] <- mean(outDF1$GPP[outDF1$Ring==i])
    }
    
    for (i in c(2,3,6)) {
        outDF2$PreTrt[outDF2$Ring==i] <- mean(outDF1$GPP[outDF1$Ring==i])
    }
    
    # Only use data period 2012-2016
    outDF <- outDF2

    outDF$Date <- as.Date(paste0(outDF$year, "-01-01"), format = "%Y-%m-%d")
    outDF$Trt[outDF$Ring%in%c(2,3,6)] <- "aCO2"
    outDF$Trt[outDF$Ring%in%c(1,4,5)] <- "eCO2"
    
    return(outDF)
}

