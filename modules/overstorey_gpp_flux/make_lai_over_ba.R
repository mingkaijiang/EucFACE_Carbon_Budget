make_lai_over_ba <- function(laiDF) {

    ### Read initial basal area data
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    f12$ba <- ((f12$X17.02.2011/2)^2) * pi
    
    baDF <- summaryBy(ba~Ring, data=f12, FUN=sum, na.rm=T, keep.names=T)
    
    ### return in unit of cm2/m2, which is m2 ha-1
    baDF$ba_ground_area <- baDF$ba / ring_area
    
    ### summary LAI per ring per ring
    laiDF$Yr <- year(laiDF$Date)
    lai.ann <- summaryBy(lai_variable~Yr+Ring, data=laiDF, FUN=mean, keep.names=T, na.rm=T)
    
    
    ### Assign data to gppDF
    for (j in 1:6) {
        laiDF$BA[laiDF$Ring== j] <- baDF$ba_ground_area[baDF$Ring==j]
    }
    
    ### Standardize
    laiDF$lai.ba <- laiDF$lai_variable/gppDF$BA
    
    outDF <- laiDF[,c("Yr", "Ring", "lai.ba", "Date")]
    colnames(outDF) <- c("year", "Ring", "LAI", "Date")
    
    outDF$Trt[outDF$Ring%in%c(2,3,6)] <- "aCO2"
    outDF$Trt[outDF$Ring%in%c(1,4,5)] <- "eCO2"
    
    outDF <- outDF[outDF$year >2012,]
    
    ggplot(outDF, aes(x=as.character(year),y=LAI,color=Trt))+
        geom_point()

    test <- summaryBy(LAI~year+Trt, FUN=mean, keep.names=T, data=outDF)
    with(test, plot(LAI~year, col=as.factor(Trt)))
    
    
    ### LAI vs. BA
    ggplot(laiDF, aes(x=BA, y=lai_variable)) + geom_smooth(method="lm")
    
    return(outDF)
}