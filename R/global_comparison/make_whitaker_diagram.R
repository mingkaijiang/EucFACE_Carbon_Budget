make_whitaker_diagram <- function() {
    ### read in global biome and climate data at CRU grids
    myDF <- read.csv("data/biome_temp_prec_full_1991_2012.csv")
    
    
    ### extract European forest temperature and precipitation range
    euroDF <- subset(myDF, lon >= -20 & lon <= 50 & lat >= 30 & lat <= 70 & BIOME %in%c(1,2,3,4,5,6,12))
    e.v1 <- mean(euroDF$temp_annual_mean, na.rm=T)
    e.v2 <- mean(euroDF$prec_annual_sum, na.rm=T)
    e.v3 <- sd(euroDF$temp_annual_mean, na.rm=T)
    e.v4 <- sd(euroDF$prec_annual_sum, na.rm=T)
    
    euroDF1 <- data.frame(19, e.v1, e.v2, 
                          e.v1+e.v3, e.v1-e.v3, 
                          e.v2+e.v4, e.v2-e.v4)
    colnames(euroDF1) <- c("BIOME", "temp_annual_mean", "prec_annual_sum", "temp_pos", "temp_neg", 
                           "prec_pos", "prec_neg")
    
    
    ### tropical old-growth forest
    trpDF <- read.csv("data/Clark_2001.csv")
    
    t.v1 <- mean(trpDF$MAT, na.rm=T)
    t.v2 <- mean(trpDF$MAP, na.rm=T)
    t.v3 <- sd(trpDF$MAT, na.rm=T)
    t.v4 <- sd(trpDF$MAP, na.rm=T)
    
    tropDF <- data.frame(20, t.v1, t.v2, 
                          t.v1+t.v3, t.v1-t.v3, 
                          t.v2+t.v4, t.v2-t.v4)
    colnames(tropDF) <- c("BIOME", "temp_annual_mean", "prec_annual_sum", "temp_pos", "temp_neg", 
                           "prec_pos", "prec_neg")
    
    
    ### create a subset DF
    myDF <- subset(myDF, BIOME <= 14 & BIOME > 0)
    myDF <- myDF[,c("BIOME", "temp_annual_mean", "prec_annual_sum")]
    myDF$temp_pos <- NA
    myDF$temp_neg <- NA
    myDF$prec_pos <- NA
    myDF$prec_neg <- NA
    
    
    ## eucFACE
    eucDF <- data.frame(15, 17.5, 800, NA, NA, NA, NA)
    colnames(eucDF) <- c("BIOME", "temp_annual_mean", "prec_annual_sum", "temp_pos", "temp_neg",
                         "prec_pos", "prec_neg")
    
    ## DukeFACE
    dkDF <- data.frame(16, 15.5, 1145, NA, NA, NA, NA)
    colnames(dkDF) <- c("BIOME", "temp_annual_mean", "prec_annual_sum", "temp_pos", "temp_neg",
                        "prec_pos", "prec_neg")
    
    ## ORNL
    orDF <- data.frame(17, 13.9, 1371, NA, NA, NA, NA)
    colnames(orDF) <- c("BIOME", "temp_annual_mean", "prec_annual_sum", "temp_pos", "temp_neg",
                        "prec_pos", "prec_neg")
    
    ## WebFACE
    wbDF <- data.frame(18, 14.7, 900, NA, NA, NA, NA)
    colnames(wbDF) <- c("BIOME", "temp_annual_mean", "prec_annual_sum", "temp_pos", "temp_neg",
                        "prec_pos", "prec_neg")
    
    ### create plotDF
    plotDF <- rbind(myDF, eucDF, dkDF, orDF, wbDF, euroDF1, tropDF)

    ### biome list
    #biome <- c("Tropical moist broadleaf",            ##1
    #           "Tropical dry broadleaf",            ##2
    #           "Tropical conifer",                ##3
    #           "Temperate broadleaf & mixed",      ##4
    #           "Temperate conifer",                ##5
    #           "Boreal forests",              ##6
    #           "Tropical grass",                  ##7
    #           "Temperate grass",                  ##8
    #           "Flooded grass",               ##9
    #           "Montane grass",               ##10
    #           "Tundra",                      ##11
    #           "Mediterranean forest",                ##12
    #           "Desert",                      ##13
    #           "Mangroves",                   ##14
    #           "EucFACE")                   
    
    ### this is the full biome grids, we don't need it
    #col.list <- c(viridis(14), "red")

    #p1 <- ggplot() +
    #    geom_point(plotDF, mapping=aes(temp_annual_mean, prec_annual_sum, 
    #                                   col=factor(BIOME),
    #                                   shape=factor(BIOME), 
    #                                   size = factor(BIOME)))+
    #    xlab(expression("MAT (" * degree * "C)")) + 
    #    ylab("MAP (mm)") +
    #    scale_color_manual(name="", 
    #                       values=col.list,
    #                       labels=biome) +
    #    scale_shape_manual(values=c(rep(19, 14), 17),
    #                       labels=biome) +
    #    scale_size_manual(values=c(rep(1, 14), 6),
    #                      labels=biome)+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=16), 
    #          axis.text.x = element_text(size=20),
    #          axis.text.y=element_text(size=20),
    #          axis.title.y=element_text(size=20),
    #          legend.text=element_text(size=18),
    #          legend.title=element_text(size=16),
    #          panel.grid.major=element_blank(),
    #          legend.position="bottom",
    #          legend.text.align=0)+
    #    guides(size = "none",
    #           shape = "none",
    #           color = guide_legend(ncol=5, override.aes = 
    #                                    list(size = 9, shape=c(rep(19, 14),17), 
    #                                         colour=col.list)))
    #
    
    ### this is just the forest plot
    plotDF2 <- subset(plotDF, BIOME%in%c(1,2,3,4,5,6,12,
                                         15,16,17,18,
                                         19,20))
    
    forests <- c("Tropical moist broadleaf",            ##1
                 "Tropical dry broadleaf",            ##2
                 "Tropical conifer",                ##3
                 "Temperate broadleaf & mixed",      ##4
                 "Temperate conifer",                ##5
                 "Boreal forests",              ##6
                 "Mediterranean forests",                ##12
                 "EucFACE",
                 "DukeFACE",
                 "ORNLFACE",
                 "WebFACE",
                 "European forests",
                 "Tropical old-growth")      
    
    ### create plotting settings
    col.list <- c(viridis(7), "red", brewer.pal(5,"Blues"))
    
    p1 <- ggplot() +
        geom_point(plotDF2, mapping=aes(temp_annual_mean, prec_annual_sum, 
                                       col=factor(BIOME),
                                       shape=factor(BIOME), 
                                       size = factor(BIOME)))+
        geom_errorbar(data=plotDF2, 
                      mapping=aes(x=temp_annual_mean, ymin=prec_neg, ymax=prec_pos), 
                      width=2, size=1, color="black") + 
        geom_errorbarh(data=plotDF2, 
                      mapping=aes(y=prec_annual_sum, xmax=temp_pos, xmin=temp_neg), 
                      height=200, size=1, color="black") + 
        xlab(expression("MAT (" * degree * "C)")) + 
        ylab("MAP (mm)") +
        scale_color_manual(name="", 
                           values=col.list,
                           labels=forests) +
        scale_shape_manual(values=c(rep(19, 7), 17, rep(15, 5)),
                           labels=forests) +
        scale_size_manual(values=c(rep(1, 7), rep(4, 6)),
                          labels=forests)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=6),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(size = "none",
               shape = "none",
               color = guide_legend(ncol=4, override.aes = 
                                        list(size = 4, shape=c(rep(19, 7), 17, rep(15, 5)), 
                                             colour=col.list)))
    
    #plot(p1)
    
    ### inlet plot, comparing EucFACE NPP with European forests
    nppDF <- tables_by_ring_predicted$npp
    nppDF2 <- nppDF[nppDF$term%in%c("Leaf NPP", "Stem NPP",
                                    "Fine Root NPP", "Coarse Root NPP",
                                    "Bole Root NPP", "Other NPP",
                                    "Understorey NPP", "Leaf consumption",
                                    "Mycorrhizal production"),
                    c("Ring_1","Ring_2","Ring_3","Ring_4","Ring_5","Ring_6")]
    
    nppDF3 <- nppDF[nppDF$term%in%c("Leaf NPP", "Stem NPP",
                                    "Fine Root NPP", "Coarse Root NPP",
                                    "Bole Root NPP", "Other NPP",
                                    "Understorey NPP", "Leaf consumption"),
                    c("Ring_1","Ring_2","Ring_3","Ring_4","Ring_5","Ring_6")]
    
    csumDF1 <- colSums(nppDF2)
    csumDF2 <- colSums(nppDF3)
    
    npp.1 <- mean(csumDF1)
    npp.2 <- mean(csumDF2)
    npp.3 <- sd(csumDF1)
    npp.4 <- sd(csumDF2)
    
    ### prepare a plotDF
    plotDF3 <- data.frame(rbind(npp.1, npp.2), rbind(npp.3, npp.4), NA)
    colnames(plotDF3) <- c("mean", "sd", "Data")
    plotDF3$Data <- c("Euc1","Euc2")

    ### add european values
    euro.npp <- data.frame(rbind(447, 544), c(112, 90), c("Euro1", "Euro2"))
    colnames(euro.npp) <- c("mean", "sd", "Data")
    
    ### add tropical old-growth values
    trop.npp <- data.frame(rbind(mean(trpDF$Total_NPP_low), mean(trpDF$Total_NPP_high)),
                           rbind(sd(trpDF$Total_NPP_low), sd(trpDF$Total_NPP_high)),
                           rbind("Trop1", "Trop2"))
    colnames(trop.npp) <- c("mean", "sd", "Data")
    trop.npp$mean <- trop.npp$mean * 100
    trop.npp$sd <- trop.npp$sd * 100
    
    ### combine dataframe
    plotDF3 <- rbind(plotDF3, euro.npp, trop.npp)
    
    p2 <- ggplot() +
        geom_point(plotDF3, mapping=aes(Data, mean))+
        geom_errorbar(data=plotDF3, 
                      mapping=aes(x=Data, ymin=mean-sd, ymax=mean+sd), 
                      width=0.2, size=1, color="black") + 
        ylab(expression("NPP (g" * m^-2 * " " * yr^-1 *" )")) +
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=8), 
              axis.text.x = element_text(size=8),
              axis.text.y=element_text(size=8),
              axis.title.y=element_text(size=8),
              legend.text=element_text(size=8),
              legend.title=element_text(size=8),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_y_continuous(limits=c(0, 2000),
                           breaks = c(0,500,1500))
    
    #plot(p2)
    
    plot.with.inset <-
        ggdraw() +
        draw_plot(p1) +
        draw_plot(p2, x = 0.12, y = .68, width = .4, height = .3)
    

    ggsave(filename = "output/ED_Figure_2.pdf", 
           plot = plot.with.inset,
           width = 17, 
           height = 12,
           units = "cm",
           dpi = 300)
    
    
    
    ### alternative
    #require(ks)

    ## subsetting dataframe
    #DF <- data.frame(myDF[myDF$BIOME %in%c(1,2,3,4,5,6,12), "temp_annual_mean"],
    #                 myDF[myDF$BIOME %in%c(1,2,3,4,5,6,12), "prec_annual_sum"])
    #
    #l <- nrow(DF)
    #
    #if(l <= 5000) {
    #    H <- Hpi(x=DF)      # optimal bandwidth estimation
    #    est<- kde(x=DF, H=H, compute.cont=TRUE)     # kernel density estimation
    #} else {
    #    DF.sub <- DF[sample(nrow(DF), 5000),]
    #    # kernel density estimation
    #    H <- Hpi(x=DF.sub)      # optimal bandwidth estimation
    #    est<- kde(x=DF.sub, H=H, compute.cont=TRUE)     # kernel density estimation
    #}
    #
    ## set contour probabilities for drawing contour levels
    #cl<-contourLevels(est, prob=c(0.5, 0.25, 0.1), approx=TRUE)
    #
    #### plot
    #pdf("output/ED_Figure_7_alternative.pdf", width=16, height=10)
    #
    #plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, 
    #     ylab="Precipitation", xlab="Temperature", 
    #     ylim=c(0,4000), xlim=c(-30,40),las=1,
    #     cex.axis=2.0, cex.main = 3.5, cex.lab = 3) 
    #
    #plot(est,abs.cont=cl[1],  labels=c(0.5), labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
    #plot(est,abs.cont=cl[2], labels=c(0.75),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    #plot(est,abs.cont=cl[3], labels=c(0.9),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    #points(eucDF$temp_annual_mean, eucDF$prec_annual_sum, add=TRUE, pch=17, size=6)    
    #
    #dev.off()
 
    
}
