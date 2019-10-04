make_whitaker_diagram_2 <- function() {
    
    ################################### Prepare MAT and MAP, globally forests
    ### read in global biome and climate data at CRU grids
    myDF <- read.csv("data/biome_temp_prec_full_1991_2012.csv")
    
    ### create a subset DF
    myDF <- subset(myDF, BIOME <= 14 & BIOME > 0)
    glob.climDF <- myDF[,c("BIOME", "temp_annual_mean", "prec_annual_sum")]
    colnames(glob.climDF) <- c("Biome", "MAT", "MAP")
    
    ################################### Prepare MAT and MAP, FACE sites
    ### prepare storage DF
    faceDF <- data.frame(c("EucFACE", "DukeFACE", "ORNLFACE", "AspenFACE", "PopFACE",
                           "WebFACE", "FlakalidenWTC"), 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(faceDF) <- c("Site", "Biome", "Lat", "Lon", "MAT", "MAP", "Age", "soilN", "soilP", "NPP", 
                          "MAT_sd", "MAP_sd", "soilN_sd", "soilP_sd", "NPP_sd")
    faceDF$Biome <- c(15:21)
    
    faceDF$Lat[faceDF$Site=="EucFACE"] <- -33.36
    faceDF$Lon[faceDF$Site=="EucFACE"] <- 150.44
    
    faceDF$Lat[faceDF$Site=="DukeFACE"] <- 35.97
    faceDF$Lon[faceDF$Site=="DukeFACE"] <- -79.08
    
    faceDF$Lat[faceDF$Site=="ORNLFACE"] <- 35.9
    faceDF$Lon[faceDF$Site=="ORNLFACE"] <- -84.33
    
    faceDF$Lat[faceDF$Site=="AspenFACE"] <- 45.68
    faceDF$Lon[faceDF$Site=="AspenFACE"] <- -89.63
    
    faceDF$Lat[faceDF$Site=="PopFACE"] <- 42.22
    faceDF$Lon[faceDF$Site=="PopFACE"] <- 11.48
    
    faceDF$Lat[faceDF$Site=="WebFACE"] <- 47.33
    faceDF$Lon[faceDF$Site=="WebFACE"] <- 7.36
    
    faceDF$Lat[faceDF$Site=="FlakalidenWTC"] <- 64.07
    faceDF$Lon[faceDF$Site=="FlakalidenWTC"] <- 19.27
    
    
    
    ### prepare input files - EucFACE
    ### Read input - climate
    DF1 <- read.csv("R_other/met_air_flux_data_daily.csv")
    DF2 <- read.csv("R_other/rainfall_data_daily.csv")
    names(DF1)[1] <- names(DF2)[1] <- "Date"
    myDF <- merge(DF1, DF2, by.x="Date", all=T)
    colnames(myDF) <- c("Date", "Tair", "RH", "PAR", "Pressure", "Wind", "Rain")
    myDF$year <- year(myDF$Date)
    myDF <- subset(myDF, year > 2012 & year < 2019)
    euc.t <- summaryBy(Tair ~ year, FUN=mean, data=myDF, keep.names=T)
    euc.p <- summaryBy(Rain ~ year, FUN=sum, data=myDF, keep.names=T)
    
    ### Read in put - npp
    DF1 <- tables_by_ring_predicted$npp[,c("term", "Ring_2", "Ring_3", "Ring_6")]
    euc.npp <- colSums(DF1[DF1$term%in%c("Leaf NPP", "Stem NPP", "Fine Root NPP",
                                         "Coarse Root NPP", "Bole Root NPP",
                                         "Other NPP", "Understorey NPP",
                                         "Leaf consumption"), 2:4]) 
    
    ### prepare input files - DukeFACE NPP
    DF1 <- read.csv("data/ess_dive_f15e8d846622729_20190628T143153130/DUKE_a.csv")
    dkDF <- summaryBy(fc_npp~treatment_fertilisation, data=DF1, FUN=c(mean,sd), keep.names=T)
    
    ### prepare input files - ORNLFACE NPP
    DF1 <- read.csv("data/ess_dive_f15e8d846622729_20190628T143153130/ORNL_a.csv")
    orDF <- summaryBy(fc_npp~co2, data=DF1, FUN=c(mean,sd), keep.names=T)
    
    
    ### prepare input files - ORNLFACE NPP
    DF1 <- read.csv("data/ess_dive_f15e8d846622729_20190628T143153130/RHIN_a.csv")
    asDF <- summaryBy(fc_npp~co2, data=DF1, FUN=c(mean,sd), keep.names=T)
    
    ### prepare input files - PopFACE NPP
    pop.npp.m <- (1132 + 1437 + 1130)/3
    pop.npp.sd <- sqrt((57^2 + 31^2 + 35^2)/3)
    
    
    
    ### assign values - eucFACE
    faceDF$MAT[faceDF$Site=="EucFACE"] <- mean(euc.t$Tair)
    faceDF$MAP[faceDF$Site=="EucFACE"] <- mean(euc.p$Rain)
    faceDF$Age[faceDF$Site=="EucFACE"] <- 90
    faceDF$soilN[faceDF$Site=="EucFACE"] <- NA
    faceDF$soilP[faceDF$Site=="EucFACE"] <- NA
    faceDF$NPP[faceDF$Site=="EucFACE"] <- mean(euc.npp)
    
    faceDF$MAT_sd[faceDF$Site=="EucFACE"] <- sd(euc.t$Tair)
    faceDF$MAP_sd[faceDF$Site=="EucFACE"] <- sd(euc.p$Rain)
    faceDF$soilN_sd[faceDF$Site=="EucFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="EucFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="EucFACE"] <- sd(euc.npp)
    
    ### assign values - DukeFACE
    faceDF$MAT[faceDF$Site=="DukeFACE"] <- 14.8
    faceDF$MAP[faceDF$Site=="DukeFACE"] <- 1081
    faceDF$Age[faceDF$Site=="DukeFACE"] <- 13
    faceDF$soilN[faceDF$Site=="DukeFACE"] <- NA
    faceDF$soilP[faceDF$Site=="DukeFACE"] <- NA
    faceDF$NPP[faceDF$Site=="DukeFACE"] <- dkDF$fc_npp.mean
    
    faceDF$MAT_sd[faceDF$Site=="DukeFACE"] <- 0.6
    faceDF$MAP_sd[faceDF$Site=="DukeFACE"] <-168
    faceDF$soilN_sd[faceDF$Site=="DukeFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="DukeFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="DukeFACE"] <- dkDF$fc_npp.sd
    
    ### assign values - ORNLFACE
    faceDF$MAT[faceDF$Site=="ORNLFACE"] <- 14.8
    faceDF$MAP[faceDF$Site=="ORNLFACE"] <- 1221
    faceDF$Age[faceDF$Site=="ORNLFACE"] <- 10
    faceDF$soilN[faceDF$Site=="ORNLFACE"] <- NA
    faceDF$soilP[faceDF$Site=="ORNLFACE"] <- NA
    faceDF$NPP[faceDF$Site=="ORNLFACE"] <- orDF$fc_npp.mean[orDF$co2=="AMB"]
    
    faceDF$MAT_sd[faceDF$Site=="ORNLFACE"] <- 0.9
    faceDF$MAP_sd[faceDF$Site=="ORNLFACE"] <- 218
    faceDF$soilN_sd[faceDF$Site=="ORNLFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="ORNLFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="ORNLFACE"] <- orDF$fc_npp.sd[orDF$co2=="AMB"]
    
    ### assign values - AspenFACE
    faceDF$MAT[faceDF$Site=="AspenFACE"] <- 6.0
    faceDF$MAP[faceDF$Site=="AspenFACE"] <- 662
    faceDF$Age[faceDF$Site=="AspenFACE"] <- 1.0
    faceDF$soilN[faceDF$Site=="AspenFACE"] <- NA
    faceDF$soilP[faceDF$Site=="AspenFACE"] <- NA
    faceDF$NPP[faceDF$Site=="AspenFACE"] <- asDF$fc_npp.mean[asDF$co2=="AMB"]
    
    faceDF$MAT_sd[faceDF$Site=="AspenFACE"] <- 0.8
    faceDF$MAP_sd[faceDF$Site=="AspenFACE"] <- 122
    faceDF$soilN_sd[faceDF$Site=="AspenFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="AspenFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="AspenFACE"] <- asDF$fc_npp.sd[asDF$co2=="AMB"]
    
    ### assign values - PopFACE
    faceDF$MAT[faceDF$Site=="PopFACE"] <- 14
    faceDF$MAP[faceDF$Site=="PopFACE"] <- 800
    faceDF$Age[faceDF$Site=="PopFACE"] <- 3
    faceDF$soilN[faceDF$Site=="PopFACE"] <- 0.0012 # unit is percent
    faceDF$soilP[faceDF$Site=="PopFACE"] <- NA
    faceDF$NPP[faceDF$Site=="PopFACE"] <- pop.npp.m
    
    faceDF$MAT_sd[faceDF$Site=="PopFACE"] <- NA
    faceDF$MAP_sd[faceDF$Site=="PopFACE"] <- NA
    faceDF$soilN_sd[faceDF$Site=="PopFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="PopFACE"] <- 0.0001 * sqrt(3)
    faceDF$NPP_sd[faceDF$Site=="PopFACE"] <- pop.npp.sd
    
    
    ### assign values - WebFACE
    faceDF$MAT[faceDF$Site=="WebFACE"] <- 11
    faceDF$MAP[faceDF$Site=="WebFACE"] <- 900
    faceDF$Age[faceDF$Site=="WebFACE"] <- 110
    faceDF$soilN[faceDF$Site=="WebFACE"] <- NA
    faceDF$soilP[faceDF$Site=="WebFACE"] <- NA
    faceDF$NPP[faceDF$Site=="WebFACE"] <- NA
    
    faceDF$MAT_sd[faceDF$Site=="WebFACE"] <- NA
    faceDF$MAP_sd[faceDF$Site=="WebFACE"] <- NA
    faceDF$soilN_sd[faceDF$Site=="WebFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="WebFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="WebFACE"] <- NA
    
    
    ### assign values - FlakalidenWTC
    faceDF$MAT[faceDF$Site=="FlakalidenWTC"] <-  2.3
    faceDF$MAP[faceDF$Site=="FlakalidenWTC"] <-  600
    faceDF$Age[faceDF$Site=="FlakalidenWTC"] <- 41
    faceDF$soilN[faceDF$Site=="FlakalidenWTC"] <- NA
    faceDF$soilP[faceDF$Site=="FlakalidenWTC"] <- NA
    faceDF$NPP[faceDF$Site=="FlakalidenWTC"] <- 291
    
    faceDF$MAT_sd[faceDF$Site=="FlakalidenWTC"] <-  NA
    faceDF$MAP_sd[faceDF$Site=="FlakalidenWTC"] <-  NA
    faceDF$soilN_sd[faceDF$Site=="FlakalidenWTC"] <- NA
    faceDF$soilP_sd[faceDF$Site=="FlakalidenWTC"] <- NA
    faceDF$NPP_sd[faceDF$Site=="FlakalidenWTC"] <- NA

    ### prepare data frames to plot
    glob.climDF$MAT_pos <- NA
    glob.climDF$MAT_neg <- NA
    glob.climDF$MAP_pos <- NA
    glob.climDF$MAP_neg <- NA
    
    subDF1 <- faceDF[,c("Biome", "MAT", "MAP")]
    subDF1$MAT_pos <- faceDF$MAT + faceDF$MAT_sd
    subDF1$MAT_neg <- faceDF$MAT - faceDF$MAT_sd
    subDF1$MAP_pos <- faceDF$MAP + faceDF$MAP_sd
    subDF1$MAP_neg <- faceDF$MAP - faceDF$MAP_sd
    
    plotDF1 <- rbind(glob.climDF, subDF1)
    
    ### this is just the forest plot
    plotDF1 <- subset(plotDF1, Biome%in%c(1,2,3,4,5,6,12, 
                                         15:21))
    
    forests <- c("Tropical moist broadleaf",            
                 "Tropical dry broadleaf",            
                 "Tropical conifer",               
                 "Temperate broadleaf & mixed",      
                 "Temperate conifer",                
                 "Boreal forests",              
                 "Mediterranean forests",                
                 "EucFACE",
                 "DukeFACE",
                 "ORNLFACE",
                 "AspenFACE",
                 "PopFACE",
                 "WebFACE",
                 "FlakalidenWTC")      
    
    
    ### create plotting settings
    col.list <- c(viridis(7), "red", brewer.pal(6,"Set2"))
    
    p1 <- ggplot() +
        geom_point(plotDF1, mapping=aes(MAT, MAP, 
                                       fill=factor(Biome),
                                       color=factor(Biome),
                                       shape=factor(Biome), 
                                       size = factor(Biome)))+
        #geom_errorbar(data=plotDF1, 
        #              mapping=aes(x=MAT, ymin=MAP_neg, ymax=MAP_pos), 
        #              width=2, size=1, color="black") + 
        #geom_errorbarh(data=plotDF1, 
        #              mapping=aes(y=MAP, xmax=MAT_pos, xmin=MAT_neg), 
        #              height=200, size=1, color="black") + 
        xlab(expression("MAT (" * degree * "C)")) + 
        ylab("MAP (mm)") +
        scale_fill_manual(name="", 
                           values=col.list,
                           labels=forests) +
        scale_color_manual(name="", 
                          values=col.list,
                          labels=forests) +
        scale_shape_manual(values=c(rep(21, 7), 24, rep(22, 6)),
                           labels=forests) +
        scale_size_manual(values=c(rep(1, 7), rep(4, 7)),
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
               color = "none",
               fill = guide_legend(ncol=4, override.aes = 
                                        list(size = 4, shape=c(rep(21, 7), 24, rep(22, 6)), 
                                             colour=col.list)))
    
    #plot(p1)
    
    ### prepare NPP vs. age
    col.list <- c("red", brewer.pal(6,"Set2"))
        
    fit <- lm(NPP ~ Age, data=faceDF)
    
    p2 <- ggplot() +
        geom_point(data=faceDF, aes(Age, NPP, fill=as.factor(Biome),
                                       shape=as.factor(Biome)), size=2)+
        geom_abline(intercept = coefficients(fit)[[1]], slope = coefficients(fit)[[2]],
                    lty=2)+
        annotate(geom="text", x=60, y=1400, 
                 label=paste0("y = ", round(coefficients(fit)[[2]], 2), "x + ", 
                              round(coefficients(fit)[[1]],2)),
                  color="black")+
        ylab(expression("NPP (g" * m^-2 * " " * yr^-1 *" )")) +
        xlab("Age (yr)")+
        theme_linedraw() +
        scale_fill_manual(name="Sites", 
                           values=col.list) +
        scale_shape_manual(values=c(24, rep(22, 6)),
                           labels="") +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=8), 
              axis.text.x = element_text(size=8),
              axis.text.y=element_text(size=8),
              axis.title.y=element_text(size=8),
              legend.text=element_text(size=8),
              legend.title=element_text(size=8),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(0, 1500),
                           breaks = c(0,500,1000,1500))
    
    plot(p2)
    
    plot.with.inset <-
        ggdraw() +
        draw_plot(p1) +
        draw_plot(p2, x = 0.12, y = .68, width = .4, height = .3)
    

    ggsave(filename = "output/ED_Figure_7.pdf", 
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
