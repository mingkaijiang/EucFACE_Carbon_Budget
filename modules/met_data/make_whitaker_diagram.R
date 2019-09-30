make_whitaker_diagram <- function() {
    ### read in global biome and climate data at CRU grids
    myDF <- read.csv("data/biome_temp_prec_full_1991_2012.csv")
    
    myDF <- subset(myDF, BIOME <= 14 & BIOME > 0)
    myDF <- myDF[,c("BIOME", "temp_annual_mean", "prec_annual_sum")]

    ## eucFACE
    eucDF <- data.frame(15, 17.5, 800)
    colnames(eucDF) <- c("BIOME", "temp_annual_mean", "prec_annual_sum")
    
    plotDF <- rbind(myDF, eucDF)

    ### biome list
    biome <- c("Tropical moist broadleaf",            ##1
               "Tropical dry broadleaf",            ##2
               "Tropical conifer",                ##3
               "Temperate broadleaf & mixed",      ##4
               "Temperate conifer",                ##5
               "Boreal forests",              ##6
               "Tropical grass",                  ##7
               "Temperate grass",                  ##8
               "Flooded grass",               ##9
               "Montane grass",               ##10
               "Tundra",                      ##11
               "Mediterranean forest",                ##12
               "Desert",                      ##13
               "Mangroves",                   ##14
               "EucFACE")                   
    
    library(RColorBrewer)
    col.list <- c(viridis(14), "red")

    
    
    p1 <- ggplot() +
        geom_point(plotDF, mapping=aes(temp_annual_mean, prec_annual_sum, 
                                       col=factor(BIOME),
                                       shape=factor(BIOME), 
                                       size = factor(BIOME)))+
        xlab(expression("MAT (" * degree * "C)")) + 
        ylab("MAP (mm)") +
        scale_color_manual(name="", 
                           values=col.list,
                           labels=biome) +
        scale_shape_manual(values=c(rep(19, 14), 17),
                           labels=biome) +
        scale_size_manual(values=c(rep(1, 14), 6),
                          labels=biome)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=20),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(size = "none",
               shape = "none",
               color = guide_legend(ncol=5, override.aes = 
                                        list(size = 9, shape=c(rep(19, 14),17), 
                                             colour=col.list)))
    
    
    pdf("output/ED_Figure_7.pdf", width=16, height=10)
    plot(p1)
    
    dev.off()
    
    
    
    ### alternative
    
    
    #for (i in 1:14) {
    #    # subsetting dataframe
    #    DF <- data.frame(myDF[myDF$BIOME == i, "temp_annual_mean"],
    #                     myDF[myDF$BIOME == i, "prec_annual_sum"])
    #    
    #    l <- nrow(DF)
    #    
    #    if(l <= 1000) {
    #        H <- Hpi(x=DF)      # optimal bandwidth estimation
    #        est<- kde(x=DF, H=H, compute.cont=TRUE)     # kernel density estimation
    #    } else {
    #        DF.sub <- DF[sample(nrow(DF), 1000),]
    #        # kernel density estimation
    #        H <- Hpi(x=DF.sub)      # optimal bandwidth estimation
    #        est<- kde(x=DF.sub, H=H, compute.cont=TRUE)     # kernel density estimation
    #    }
    #    
    #    # set contour probabilities for drawing contour levels
    #    cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
    #    
    #    plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, 
    #         ylab="Precipitation", xlab="Temperature", main=biome[i],
    #         ylim=c(0,8000), xlim=c(-30,40),las=1,
    #         cex.axis=2.0, cex.main = 3.5, cex.lab = 3) 
    #    plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
    #    plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    #    plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    #    
    #}
    
}