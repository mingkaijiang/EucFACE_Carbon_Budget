make_gpp_over_lai <- function(laiDF, gppDF) {
    ### summary LAI per ring per ring
    cov2 <- lai_variable[lai_variable$Date<="2013-02-06",]
    #cov2 <- lai_variable[lai_variable$Date=="2012-10-26",]
    covDF <- summaryBy(lai_variable~Ring, data=cov2, FUN=mean, keep.names=T)
    
    covDF2 <- summaryBy(lai_variable~Ring, data=lai_variable, FUN=mean, keep.names=T)
    
    ### prepare gppDF
    gppDF$year <- as.numeric(gppDF$year)
    gppDF$Ring <- as.numeric(gppDF$Ring)
    
    ### Assign data to gppDF
    for (j in 1:6) {
        gppDF$LAI[gppDF$Ring== j] <- covDF$lai_variable[covDF$Ring==j]
        gppDF$LAI2[gppDF$Ring== j] <- covDF2$lai_variable[covDF2$Ring==j]
        
    }
    
    ### Standardize
    gppDF$gpp.s <- gppDF$GPP/gppDF$LAI
    
    outDF <- gppDF[,c("year", "Ring", "GPP", "LAI", "LAI2", "Date")]
    colnames(outDF) <- c("year", "Ring", "GPP", "LAI", "LAI2", "Date")
    
    ### Assign treatment
    outDF$Trt[outDF$Ring%in%c(2,3,6)] <- "aCO2"
    outDF$Trt[outDF$Ring%in%c(1,4,5)] <- "eCO2"
    outDF$Ring <- as.numeric(outDF$Ring)
    
    p1 <- ggplot(outDF, aes(x=LAI, y=GPP)) + 
        geom_smooth(aes(group=Trt), method="lm") + ylab("GPP") +
        geom_point(data=outDF, aes(LAI, GPP, color=as.factor(outDF$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))+
        xlab("Initial LAI (<2013-02-06)")
    
    
    ### Read initial basal area data
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    f12$ba <- ((f12$X20.09.2012/2)^2) * pi
    baDF <- summaryBy(ba~Ring, data=f12, FUN=sum, na.rm=T, keep.names=T)
    ### return in unit of cm2/m2, which is m2 ha-1
    baDF$ba_ground_area <- baDF$ba / ring_area
    
    baDF2 <- data.frame(c(1:6), c(611, 835, 795, 896, 1019, 815))
    colnames(baDF2) <- c("Ring", "BA")
    
    
    for (i in 1:6) {
        outDF[outDF$Ring==i, "BA"] <- baDF[baDF$Ring==i, "ba_ground_area"]
        outDF[outDF$Ring==i, "BA2"] <- baDF2[baDF2$Ring==i, "BA"]
        
    }
    
    p2 <- ggplot(outDF, aes(x=BA, y=GPP)) + 
        geom_smooth(aes(group=Trt), method="lm") + ylab("GPP") +
        geom_point(data=outDF, aes(BA, GPP, color=as.factor(outDF$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))+
        xlab("HIEv BA")
    
    p3 <- ggplot(outDF, aes(x=BA2, y=GPP)) + 
        geom_smooth(aes(group=Trt), method="lm") + ylab("GPP") +
        geom_point(data=outDF, aes(BA2, GPP, color=as.factor(outDF$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))+
        xlab("Remko BA")
    
    
    p4 <- ggplot(outDF, aes(x=LAI2, y=GPP)) + 
        geom_smooth(aes(group=Trt), method="lm") + ylab("GPP") +
        geom_point(data=outDF, aes(LAI2, GPP, color=as.factor(outDF$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))+
        xlab("Long term LAI")
    
    require(cowplot)
    pdf("R_other/LAI_GPP_comparison.pdf", width=12, height=8)
    plot_grid(p1, p4, labels="", ncol=1, align="v", axis = "l")
    
    dev.off()
    
    
    return(outDF)
}