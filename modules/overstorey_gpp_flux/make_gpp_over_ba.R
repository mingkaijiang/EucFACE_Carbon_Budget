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
    
    gpp.ann.avg <- summaryBy(GPP~Ring+year, data=gppDF, FUN=mean, keep.names=T, na.rm=T)
    
    ### summary LAI per ring per ring
    laiDF$Yr <- year(laiDF$Date)
    lai.ann.avg <- summaryBy(lai_variable~Ring+Yr, data=laiDF, FUN=mean, keep.names=T, na.rm=T)
    
    
    ### Assign data to gppDF
    for (j in 1:6) {
        lai.ann.avg$BA[lai.ann.avg$Ring== j] <- baDF$ba_ground_area[baDF$Ring==j]
        
        for (k in 2013:2016) {
            lai.ann.avg$GPP[lai.ann.avg$Ring== j & lai.ann.avg$Yr == k] <- gpp.ann.avg$GPP[gpp.ann.avg$Ring==j & gpp.ann.avg$year == k]
        }
    }
    
    plotDF <- subset(lai.ann.avg, Yr > 2012)
    
    ### Assign treatment
    plotDF$Trt[plotDF$Ring%in%c(2,3,6)] <- "aCO2"
    plotDF$Trt[plotDF$Ring%in%c(1,4,5)] <- "eCO2"
    
    pdf("R_other/BA_vs_GPP_and_LAI.pdf", width=12, height=12)
    
    plotDF1 <- subset(plotDF, Yr == 2013)
    p1 <- ggplot(plotDF1, aes(x=BA, y=GPP)) + 
        geom_smooth(method="lm") + ylab("GPP 2013") +
        geom_point(data=plotDF1, aes(BA, GPP,color=as.factor(plotDF1$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))
    
    
    p2 <- ggplot(plotDF1, aes(x=BA, y=lai_variable)) + 
        geom_smooth(method="lm") + ylab("LAI 2013") +
        geom_point(data=plotDF1, aes(BA, lai_variable, color=as.factor(plotDF1$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))
    
    plotDF2 <- subset(plotDF, Yr == 2014)
    p3 <- ggplot(plotDF2, aes(x=BA, y=GPP)) + 
        geom_smooth(method="lm") + ylab("GPP 2014") +
        geom_point(data=plotDF2, aes(BA, GPP,color=as.factor(plotDF2$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))
    
    
    p4 <- ggplot(plotDF2, aes(x=BA, y=lai_variable)) + 
        geom_smooth(method="lm") + ylab("LAI 2014") +
        geom_point(data=plotDF2, aes(BA, lai_variable, color=as.factor(plotDF2$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))
    
    plotDF3 <- subset(plotDF, Yr == 2015)
    p5 <- ggplot(plotDF3, aes(x=BA, y=GPP)) + 
        geom_smooth(method="lm") + ylab("GPP 2015") +
        geom_point(data=plotDF3, aes(BA, GPP,color=as.factor(plotDF3$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))
    
    
    p6 <- ggplot(plotDF3, aes(x=BA, y=lai_variable)) + 
        geom_smooth(method="lm") + ylab("LAI 2015") +
        geom_point(data=plotDF3, aes(BA, lai_variable, color=as.factor(plotDF3$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))
    
    plotDF4 <- subset(plotDF, Yr == 2016)
    p7 <- ggplot(plotDF4, aes(x=BA, y=GPP)) + 
        geom_smooth(method="lm") + ylab("GPP 2016") +
        geom_point(data=plotDF4, aes(BA, GPP,color=as.factor(plotDF4$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))
    
    
    p8 <- ggplot(plotDF4, aes(x=BA, y=lai_variable)) + 
        geom_smooth(method="lm") + ylab("LAI 2016") +
        geom_point(data=plotDF4, aes(BA, lai_variable, color=as.factor(plotDF4$Ring)))+
        scale_colour_manual(name="Ring", values = c("1" = "red", "4" = "pink",  "5" = "orange", 
                                                    "2" = "cyan",  "3" = "blue", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6"))
    
    require(cowplot)
    plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, labels="", ncol=2, align="v", axis = "l")
    
    
    
    dev.off()
    
    return(outDF)
}