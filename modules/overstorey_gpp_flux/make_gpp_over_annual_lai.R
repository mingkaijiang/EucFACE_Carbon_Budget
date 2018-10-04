make_gpp_over_annual_lai <- function(laiDF, gppDF) {
    ### summary LAI per ring per ring
    laiDF$Yr <- year(laiDF$Date)
    lai.ann <- summaryBy(lai_variable~Yr+Ring, data=laiDF, FUN=mean, keep.names=T, na.rm=T)
    
    ### prepare gppDF
    gppDF$year <- as.numeric(gppDF$year)
    gppDF$Ring <- as.numeric(gppDF$Ring)
    
    ### Assign data to gppDF
    for (i in 2013:2016) {
        for (j in 1:6) {
            gppDF$LAI[gppDF$year == i & gppDF$Ring== j] <- lai.ann$lai_variable[lai.ann$Yr==i & lai.ann$Ring==j]
        }
    }
    
    ### Standardize
    gppDF$gpp.s <- gppDF$GPP/gppDF$LAI
    
    outDF <- gppDF[,c("year", "Ring", "gpp.s", "Date")]
    colnames(outDF) <- c("year", "Ring", "GPP", "Date")
    
    return(outDF)
}