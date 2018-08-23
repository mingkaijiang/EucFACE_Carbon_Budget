make_gpp_over_soil_p_conc <- function(pDF=soil_p_concentration) {

    ### Read soil P concentration data, calculate all-year average for each ring
    pDF$Date <- as.Date(pDF$Date)
    paDF <- summaryBy(PercP~Ring, data=pDF, FUN=mean, keep.names=T, na.rm=T)
    paDF.sd <- summaryBy(PercP~Ring, data=pDF, FUN=sd, keep.names=T, na.rm=T)
    paDF$sd <- paDF.sd$PercP
    
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
        gppDF$soil_P[gppDF$Ring== j] <- paDF$PercP[paDF$Ring==j]
    }
    
    ### Plotting
    
    plotDF1 <- subset(gppDF, Yr == 2013)
    p1 <- ggplot(plotDF1, aes(x=soil_P, y=GPP)) + 
        ylab(expression(paste("GPP (g C ", m^-2, " ", yr^-1, ")"))) +
        geom_point(data=plotDF1, aes(soil_P, GPP,shape=Mod.Trt, color=as.factor(plotDF1$Ring)), size=4)+
        scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        scale_shape_manual(name="Treatment", values = c("a"=19, "e"=17),
                           labels=c("aCa", "eCa"))+
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
        annotate("text", x=0.0085, y=2400, label= "2013", size=10) +
        ylim(750, 2500) + xlim(0.005, 0.009)
    
    plotDF2 <- subset(gppDF, Yr == 2014)
    p2 <- ggplot(plotDF2, aes(x=soil_P, y=GPP)) + 
        geom_point(data=plotDF2, aes(soil_P, GPP,shape=Mod.Trt, color=as.factor(plotDF2$Ring)), size=4)+
        scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        scale_shape_manual(name="Treatment", values = c("a"=19, "e"=17),
                           labels=c("aCa", "eCa"))+
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
        annotate("text", x=0.0085, y=2400, label= "2014", size=10) +
        ylim(750, 2500) + xlim(0.005, 0.009)
    
    
    plotDF3 <- subset(gppDF, Yr == 2015)
    p3 <- ggplot(plotDF3, aes(x=soil_P, y=GPP)) + 
        ylab(expression(paste("GPP (g C ", m^-2, " ", yr^-1, ")"))) +
        xlab("Soil P concentration (%)")+
        geom_point(data=plotDF3, aes(soil_P, GPP,shape=Mod.Trt, color=as.factor(plotDF3$Ring)), size=4)+
        scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        scale_shape_manual(name="Treatment", values = c("a"=19, "e"=17),
                           labels=c("aCa", "eCa"))+
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
        annotate("text", x=0.0085, y=2400, label= "2015", size=10) +
        ylim(750, 2500) + xlim(0.005, 0.009)
    

    
    plotDF4 <- subset(gppDF, Yr == 2016)
    p4 <- ggplot(plotDF4, aes(x=soil_P, y=GPP)) + 
        xlab("Soil P concentration (%)")+
        geom_point(data=plotDF4, aes(soil_P, GPP,shape=Mod.Trt, color=as.factor(plotDF4$Ring)), size=4)+
        scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                    "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                            labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
        scale_shape_manual(name="Treatment", values = c("a"=19, "e"=17),
                           labels=c("aCa", "eCa"))+
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
        annotate("text", x=0.0085, y=2400, label= "2016", size=10) +
        ylim(750, 2500) + xlim(0.005, 0.009)
    
    pdf("R_other/GPP_vs_soil_P_concentration.pdf", width=12, height=8)
    require(cowplot)
    plot_grid(p1, p2, p3, p4, labels="AUTO", ncol=2, align="v", axis = "l",
              rel_heights = c(0.7, 1.0))
    dev.off()
    
 }