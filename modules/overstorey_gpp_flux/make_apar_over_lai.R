make_apar_over_lai <- function(laiDF) {

    ### summary LAI per ring per ring
    laiDF$Yr <- year(laiDF$Date)
    lai.ann <- summaryBy(lai_variable~Yr+Ring, data=laiDF, FUN=mean, keep.names=T, na.rm=T)
    
    ### Read APAR data
    aparDF <- read.csv("temp_files/ca_response.csv")
    aparDF <- aparDF[,c("year.e", "Ring.e", "APAR.e")]
    colnames(aparDF) <- c("Yr", "Ring", "APAR")

    aparDF$Trt[aparDF$Ring%in%c("R1", "R4", "R5")] <- "eCO2"
    aparDF$Trt[aparDF$Ring%in%c("R2", "R3", "R6")] <- "aCO2"
    
    aparDF$Ring <- gsub("R", "", aparDF$Ring)
    aparDF$Ring <- as.numeric(aparDF$Ring)
    
    ### Assign data to aparDF
    for (j in 1:6) {
        aparDF$lai_variable[aparDF$Ring== j] <- lai.ann$lai_variable[lai.ann$Ring==j]
    }
    
    ### Plotting
    
    plotDF1 <- subset(aparDF, Yr == 2013)
    p1 <- ggplot(plotDF1, aes(x=lai_variable, y=APAR)) + 
        geom_smooth(method="lm") + ylab("APAR") + xlab("LAI")+
        geom_point(data=plotDF1, aes(lai_variable, APAR,color=as.factor(plotDF1$Ring)), size=4)+
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
        ylim(500, 2000)+xlim(1,2.1)
    
    plotDF2 <- subset(aparDF, Yr == 2014)
    p2 <- ggplot(plotDF2, aes(x=lai_variable, y=APAR)) + 
        geom_smooth(method="lm") + ylab("APAR") +xlab("LAI")+
        geom_point(data=plotDF2, aes(lai_variable, APAR, color=as.factor(plotDF2$Ring)), size=4)+
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
        ylim(500, 2000)+xlim(1,2.1)
    
    
    plotDF3 <- subset(aparDF, Yr == 2015)
    p3 <- ggplot(plotDF3, aes(x=lai_variable, y=APAR)) + 
        geom_smooth(method="lm") + ylab("APAR") +xlab("LAI")+
        geom_point(data=plotDF3, aes(lai_variable, APAR,color=as.factor(plotDF3$Ring)), size=4)+
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
        ylim(500, 2000)+xlim(1,2.1)
    

    
    plotDF4 <- subset(aparDF, Yr == 2016)
    p4 <- ggplot(plotDF4, aes(x=lai_variable, y=APAR)) + 
        geom_smooth(method="lm") + ylab("APAR") +xlab("LAI")+
        geom_point(data=plotDF4, aes(lai_variable, APAR, color=as.factor(plotDF4$Ring)), size=4)+
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
        ylim(500, 2000)+xlim(1,2.1)
    
    pdf("R_other/APAR_vs_LAI.pdf", width=12, height=8)
    require(cowplot)
    plot_grid(p1, p2, p3, p4, labels="AUTO", ncol=2, align="v", axis = "l",
              rel_heights = c(0.7, 1.0))
    dev.off()
    
}