make_gpp_over_lai <- function(laiDF, gppDF) {
    ### summary LAI per ring per ring
    cov2 <- laiDF[laiDF$Date=="2012-10-26",]
    covDF <- summaryBy(lai_variable~Ring, data=cov2, FUN=mean, keep.names=T)
    
    ### prepare gppDF
    gppDF$year <- as.numeric(gppDF$year)
    gppDF$Ring <- as.numeric(gppDF$Ring)
    
    ### Assign data to gppDF
    for (j in 1:6) {
        gppDF$LAI[gppDF$Ring== j] <- covDF$lai_variable[covDF$Ring==j]
    }
    
    ### Standardize
    gppDF$gpp.s <- gppDF$GPP/gppDF$LAI
    
    outDF <- gppDF[,c("year", "Ring", "GPP", "LAI", "Date")]
    colnames(outDF) <- c("year", "Ring", "GPP", "LAI", "Date")
    
    ### Assign treatment
    outDF$Trt[outDF$Ring%in%c(2,3,6)] <- "aCO2"
    outDF$Trt[outDF$Ring%in%c(1,4,5)] <- "eCO2"
    outDF$Ring <- as.numeric(outDF$Ring)
    
    p1 <- ggplot(outDF, aes(x=LAI, y=GPP)) + 
        geom_smooth(aes(group=Trt), method="lm") + ylab("GPP") +
        geom_point(data=outDF, aes(LAI, GPP, color=as.factor(outDF$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))
    

    return(outDF)
}