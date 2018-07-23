make_gpp_lai_over_ba <- function(laiDF, gppDF) {

    ### Read initial basal area data
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    f12$ba <- ((f12$X20.09.2012/2)^2) * pi
    baDF <- summaryBy(ba~Ring, data=f12, FUN=sum, na.rm=T, keep.names=T)
    ### return in unit of cm2/m2, which is m2 ha-1
    baDF$ba_ground_area <- baDF$ba / ring_area
    
    ### prepare gppDF
    gppDF$year <- as.numeric(gppDF$year)
    gppDF$Ring <- as.numeric(gppDF$Ring)
    
    gpp.ann.avg <- summaryBy(GPP~Ring, data=gppDF, FUN=mean, keep.names=T, na.rm=T)
    
    ### summary LAI per ring per ring
    laiDF$Yr <- year(laiDF$Date)
    lai.ann.avg <- summaryBy(lai_variable~Ring, data=laiDF, FUN=mean, keep.names=T, na.rm=T)
    
    
    ### Assign data to gppDF
    for (j in 1:6) {
        baDF$GPP[baDF$Ring== j] <- gpp.ann.avg$GPP[gpp.ann.avg$Ring==j]
        baDF$LAI[baDF$Ring== j] <- lai.ann.avg$lai_variable[lai.ann.avg$Ring==j]
        
    }
    
    ### Assign treatment
    baDF$Trt[baDF$Ring%in%c(2,3,6)] <- "aCO2"
    baDF$Trt[baDF$Ring%in%c(1,4,5)] <- "eCO2"
    
    pdf("R_other/BA_vs_GPP_and_LAI.pdf")
    ggplot(baDF, aes(x=ba_ground_area, y=GPP)) + 
        geom_smooth(method="lm") +
        geom_point(data=baDF, aes(ba_ground_area, GPP,color=as.factor(baDF$Trt)))
    
    ggplot(baDF, aes(x=ba_ground_area, y=LAI)) + 
        geom_smooth(method="lm") +
        geom_point(data=baDF, aes(ba_ground_area, LAI, color=as.factor(baDF$Trt)))
    dev.off()
    return(outDF)
}