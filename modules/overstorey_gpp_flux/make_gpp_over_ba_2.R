make_gpp_over_ba_2 <- function() {

    ### Read initial basal area data
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    f12$ba <- ((f12$X20.09.2012/2)^2) * pi
    baDF <- summaryBy(ba~Ring, data=f12, FUN=sum, na.rm=T, keep.names=T)
    ### return in unit of cm2/m2, which is m2 ha-1
    baDF$ba_ground_area <- baDF$ba / ring_area
    
    ### Read GPP data
    gppDF <- read.csv("temp_files/ca_response.csv")
    gDF1 <- gppDF[,c("year.e", "Ring.e", "GPP.e")]
    gDF2 <- gppDF[,c("year.e", "Ring.e", "GPP")]
    colnames(gDF1) <- colnames(gDF2) <- c("Yr", "Ring", "GPP")
    gDF1$Mod.Trt <- "e"
    gDF2$Mod.Trt <- "a"
    gppDF <- rbind(gDF1, gDF2) 
    gppDF$Trt[gppDF$Ring%in%c("R1", "R4", "R5")] <- "eCO2"
    gppDF$Trt[gppDF$Ring%in%c("R2", "R3", "R6")] <- "aCO2"
    
    gppDF$Ring <- gsub("R", "", gppDF$Ring)
    gppDF$Ring <- as.numeric(gppDF$Ring)
    
    ### Assign data to gppDF
    for (j in 1:6) {
        gppDF$BA[gppDF$Ring== j] <- baDF$ba_ground_area[baDF$Ring==j]
    }
    
    ### Assign symbol
    gppDF$Symb[gppDF$Mod.Trt=="e"&gppDF$Trt=="eCO2"] <- "solid_trian"
    gppDF$Symb[gppDF$Mod.Trt=="a"&gppDF$Trt=="eCO2"] <- "open_round"
    gppDF$Symb[gppDF$Mod.Trt=="e"&gppDF$Trt=="aCO2"] <- "open_trian"
    gppDF$Symb[gppDF$Mod.Trt=="a"&gppDF$Trt=="aCO2"] <- "solid_round"
    
    ### Plotting
    
    plotDF1 <- subset(gppDF, Yr == 2013)
    p1 <- ggplot(plotDF1, aes(x=BA, y=GPP)) + 
        geom_smooth(method="lm") + 
        ylab(expression(paste("GPP (g C ", m^-2, " ", yr^-1, ")"))) +
        geom_point(data=plotDF1, aes(BA, GPP,shape=Symb, color=as.factor(plotDF1$Ring)), size=4)+
        scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        scale_shape_manual(name="Trt", values = c("open_round"=1, "open_trian"=2,
                                                  "solid_round"=19, "solid_trian"=17),
                           labels=c("aCe", "eCa", "aCa", "eCe"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        annotate("text", x=18, y=2400, label= "2013", size=10) +
        ylim(750, 2500)

    plotDF2 <- subset(gppDF, Yr == 2014)
    p2 <- ggplot(plotDF2, aes(x=BA, y=GPP)) + 
        geom_smooth(method="lm") + ylab("GPP") +
        geom_point(data=plotDF2, aes(BA, GPP,shape=Symb, color=as.factor(plotDF2$Ring)), size=4)+
        scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        scale_shape_manual(name="Trt", values = c("open_round"=1, "open_trian"=2,
                                                  "solid_round"=19, "solid_trian"=17),
                           labels=c("aCe", "eCa", "aCa", "eCe"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        annotate("text", x=18, y=2400, label= "2014", size=10) +
        ylim(750, 2500)
    
    
    plotDF3 <- subset(gppDF, Yr == 2015)
    p3 <- ggplot(plotDF3, aes(x=BA, y=GPP)) + 
        geom_smooth(method="lm") + 
        ylab(expression(paste("GPP (g C ", m^-2, " ", yr^-1, ")"))) +
        xlab(expression(paste("Basal Area (", cm^2, ")")))+
        geom_point(data=plotDF3, aes(BA, GPP,shape=Symb, color=as.factor(plotDF3$Ring)), size=4)+
        scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        scale_shape_manual(name="Trt", values = c("open_round"=1, "open_trian"=2,
                                                  "solid_round"=19, "solid_trian"=17),
                           labels=c("aCe", "eCa", "aCa", "eCe"))+
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
        annotate("text", x=18, y=2400, label= "2015", size=10) +
        ylim(750, 2500)+
        guides(fill=guide_legend(nrow=2,byrow=TRUE), shape=guide_legend(nrow=2,byrow=TRUE))
    

    
    plotDF4 <- subset(gppDF, Yr == 2016)
    p4 <- ggplot(plotDF4, aes(x=BA, y=GPP)) + 
        geom_smooth(method="lm") + ylab("GPP") +
        xlab(expression(paste("Basal Area (", cm^2, ")")))+
        geom_point(data=plotDF4, aes(BA, GPP,shape=Symb, color=as.factor(plotDF4$Ring)), size=4)+
        scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        scale_shape_manual(name="Trt", values = c("open_round"=1, "open_trian"=2,
                                                  "solid_round"=19, "solid_trian"=17),
                           labels=c("aCe", "eCa", "aCa", "eCe"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="bottom")+
        annotate("text", x=18, y=2400, label= "2016", size=10) +
        ylim(750, 2500)+
        guides(fill=guide_legend(nrow=2,byrow=TRUE), shape=guide_legend(nrow=2,byrow=TRUE))

    pdf("R_other/GPP_vs_BA.pdf", width=12, height=8)
    require(cowplot)
    plot_grid(p1, p2, p3, p4, labels="AUTO", ncol=2, align="v", axis = "l",
              rel_heights = c(0.7, 1.0))
    dev.off()
    
    ### Plot an average
    gppDF.avg <- summaryBy(GPP+BA+Trt+Mod.Trt~Ring, data=gppDF, FUN=mean, keep.names=T)
    
    p5 <- ggplot(gppDF, aes(x=BA, y=GPP)) + 
        geom_smooth(aes(group=Symb), method="lm") + ylab("GPP") +
        xlab(expression(paste("Basal Area (", cm^2, ")")))+
        geom_point(data=gppDF, aes(BA, GPP,shape=Symb, color=as.factor(gppDF$Ring)), size=4)+
        scale_color_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        scale_shape_manual(name="Trt", values = c("open_round"=1, "open_trian"=2,
                                                  "solid_round"=19, "solid_trian"=17),
                           labels=c("aCe", "eCa", "aCa", "eCe"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="bottom")+
        ylim(750, 2500)+
        guides(fill=guide_legend(nrow=2,byrow=TRUE), shape=guide_legend(nrow=2,byrow=TRUE))
 
    plot(p5)
       
 }