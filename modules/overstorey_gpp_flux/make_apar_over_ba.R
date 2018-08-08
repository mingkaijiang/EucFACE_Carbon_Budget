make_apar_over_ba <- function() {

    ### Read initial basal area data
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    f12$ba <- ((f12$X20.09.2012/2)^2) * pi
    baDF <- summaryBy(ba~Ring, data=f12, FUN=sum, na.rm=T, keep.names=T)
    ### return in unit of cm2/m2, which is m2 ha-1
    baDF$ba_ground_area <- baDF$ba / ring_area
    
    ### Read APAR data
    aparDF <- read.csv("temp_files/ca_response.csv")
    aparDF <- aparDF[,c("year.e", "Ring.e", "APAR.e")]
    colnames(aparDF) <- c("Yr", "Ring", "APAR")

    aparDF$Trt[aparDF$Ring%in%c("R1", "R4", "R5")] <- "eCO2"
    aparDF$Trt[aparDF$Ring%in%c("R2", "R3", "R6")] <- "aCO2"
    
    aparDF$Ring <- gsub("R", "", aparDF$Ring)
    aparDF$Ring <- as.numeric(aparDF$Ring)
    
    ### Assign data to gppDF
    for (j in 1:6) {
        aparDF$BA[aparDF$Ring== j] <- baDF$ba_ground_area[baDF$Ring==j]
    }
    
    ### Plotting
    
    plotDF1 <- subset(aparDF, Yr == 2013)
    p1 <- ggplot(plotDF1, aes(x=BA, y=APAR)) + 
        geom_smooth(method="lm") + ylab("APAR") +
        geom_point(data=plotDF1, aes(BA, APAR,color=as.factor(plotDF1$Ring)), size=4)+
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
        ylim(500, 2000)
    
    plotDF2 <- subset(aparDF, Yr == 2014)
    p2 <- ggplot(plotDF2, aes(x=BA, y=APAR)) + 
        geom_smooth(method="lm") + ylab("APAR") +
        geom_point(data=plotDF2, aes(BA, APAR, color=as.factor(plotDF2$Ring)), size=4)+
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
        ylim(500, 2000)
    
    
    plotDF3 <- subset(aparDF, Yr == 2015)
    p3 <- ggplot(plotDF3, aes(x=BA, y=APAR)) + 
        geom_smooth(method="lm") + ylab("APAR") +
        geom_point(data=plotDF3, aes(BA, APAR,color=as.factor(plotDF3$Ring)), size=4)+
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
        ylim(500, 2000)
    

    
    plotDF4 <- subset(aparDF, Yr == 2016)
    p4 <- ggplot(plotDF4, aes(x=BA, y=APAR)) + 
        geom_smooth(method="lm") + ylab("APAR") +
        geom_point(data=plotDF4, aes(BA, APAR, color=as.factor(plotDF4$Ring)), size=4)+
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
        ylim(500, 2000)
    
    pdf("R_other/APAR_vs_BA.pdf", width=12, height=8)
    require(cowplot)
    plot_grid(p1, p2, p3, p4, labels="AUTO", ncol=2, align="v", axis = "l",
              rel_heights = c(0.7, 1.0))
    dev.off()
    
}