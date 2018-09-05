make_overstorey_gpp_flux <- function() {
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
    
    ### select only the real treatment data
    for (i in c(2,3,6)) {
        for (j in 2013:2016) {
            outDF2$GPP[outDF2$Ring==i&outDF2$year==j] <- outDF1$GPP[outDF1$Ring==i&outDF1$year==j]
        }
    }
    
    ### add "pre-treatment" GPP data
    #for (i in c(1,4,5)) {
    #    for (j in 2013:2016) {
    #        outDF2$preGPP[outDF2$Ring==i&outDF2$year==j] <- outDF1$GPP[outDF1$Ring==i&outDF1$year==j]
    #    }
    #}
    
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