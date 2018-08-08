make_lai_over_ba <- function(laiDF, gppDF) {

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
    laiDF$lai.ba <- laiDF$lai_variable/laiDF$BA
    
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
    t1 <- summaryBy(BA+lai_variable~Ring, FUN=mean, data=laiDF, keep.names=T, na.rm=T)
    p1 <- ggplot(laiDF, aes(x=BA, y=lai_variable)) + geom_smooth(method="lm")+
        geom_point(data=t1, aes(BA, lai_variable,color=as.factor(t1$Ring)))+
        ylab("LAI (2012-2016)")
    
    ## 2013
    lai2013 <- subset(laiDF, Yr==2013)
    t2 <- summaryBy(BA+lai_variable~Ring, FUN=mean, data=lai2013, keep.names=T, na.rm=T)
    
    p2 <- ggplot(lai2013, aes(x=BA, y=lai_variable)) + 
        geom_smooth(method="lm") + ylab("LAI") +
        geom_point(data=t2, aes(BA, lai_variable,color=as.factor(t2$Ring)), size=4)+
        scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        ggtitle("Year 2013")+
        ylim(1, 2.1)
    
    ## 2014
    lai2014 <- subset(laiDF, Yr==2014)
    t3 <- summaryBy(BA+lai_variable~Ring, FUN=mean, data=lai2014, keep.names=T, na.rm=T)
    p3 <- ggplot(lai2014, aes(x=BA, y=lai_variable)) + 
        geom_smooth(method="lm") + ylab("LAI") +
        geom_point(data=t4, aes(BA, lai_variable,color=as.factor(t3$Ring)), size=4)+
        scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        ggtitle("Year 2014")+
        ylim(1, 2.1)
    
    ## 2015
    lai2015 <- subset(laiDF, Yr==2015)
    t4 <- summaryBy(BA+lai_variable~Ring, FUN=mean, data=lai2015, keep.names=T, na.rm=T)
    p4 <- ggplot(lai2015, aes(x=BA, y=lai_variable)) + 
        geom_smooth(method="lm") + ylab("LAI") +
        geom_point(data=t4, aes(BA, lai_variable,color=as.factor(t4$Ring)), size=4)+
        scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="bottom")+
        ggtitle("Year 2015")+
        ylim(1, 2.1)
    
    ## 2016
    lai2016 <- subset(laiDF, Yr==2016)
    t5 <- summaryBy(BA+lai_variable~Ring, FUN=mean, data=lai2016, keep.names=T, na.rm=T)
    p5 <- ggplot(lai2016, aes(x=BA, y=lai_variable)) + 
        geom_smooth(method="lm") + ylab("LAI") +
        geom_point(data=t5, aes(BA, lai_variable,color=as.factor(t5$Ring)), size=4)+
        scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="bottom")+
        ggtitle("Year 2016")+
        ylim(1, 2.1)
    
    ### Plotting
    require(cowplot)
    pdf("R_other/lai_vs_BA.pdf", width=8, height=8)
    plot_grid(p2, p3, p4, p5, labels="AUTO", ncol=2, align="v", axis = "l",
              rel_heights = c(0.7, 1.0))
    dev.off()
    
    
    return(outDF)
}
