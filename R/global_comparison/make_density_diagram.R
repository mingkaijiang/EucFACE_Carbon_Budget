make_density_diagram <- function() {
    
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
                           "WebFACE", "AmazonFACE", "BiForFACE", "SwedFACE",
                           "FlakalidenWTC"), 
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(faceDF) <- c("Site", "Biome", "Lat", "Lon", "MAT", "MAP", "Age", "soilN", "soilP", "NPP", 
                          "MAT_sd", "MAP_sd", "soilN_sd", "soilP_sd", "NPP_sd")
    faceDF$Biome <- c(15:24)
    
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
    
    faceDF$Lat[faceDF$Site=="BiForFACE"] <- 52.801
    faceDF$Lon[faceDF$Site=="BiForFACE"] <- -2.301
    
    faceDF$Lat[faceDF$Site=="AmazonFACE"] <- -2.596
    faceDF$Lon[faceDF$Site=="AmazonFACE"] <- -60.208
    
    faceDF$Lat[faceDF$Site=="SwedFACE"] <- 57.167
    faceDF$Lon[faceDF$Site=="SwedFACE"] <- 14.783
    
    faceDF$Lat[faceDF$Site=="FlakalidenWTC"] <- 64.07
    faceDF$Lon[faceDF$Site=="FlakalidenWTC"] <- 19.27
    
    
    ### cuedata
    #deluciaDF <- read.csv("data/DeLucia2007.csv")
#
    #collatiDF <- read.csv("data/Collati2019.csv")
    #
    #MichaletzDF <- read.csv("data/Michaletz2014.csv")
    #
    #sub1 <- data.frame(deluciaDF$NPP, deluciaDF$Age)
    #colnames(sub1)<- c("NPP", "Age")
    #sub2 <- data.frame(collatiDF$NPP, collatiDF$Age)
    #colnames(sub2)<- c("NPP", "Age")
    #
    #sub3 <- MichaletzDF[MichaletzDF$Source!="Luo (1996); Ni et al. (2001)",]
    #sub3 <- data.frame(sub3$NPPTotal_gm2y, sub3$Age_yr)
    #colnames(sub3)<- c("NPP", "Age")
    #sub3$NPP <- sub3$NPP / 2
    
    #cueDF <- rbind(sub1, sub2, sub3)
    #cueDF$NPP <- as.numeric(cueDF$NPP)
    #cueDF$Age <- as.numeric(as.character(cueDF$Age))
    #
    #fit.cue <- lm(NPP ~ Age, data=cueDF)
    
    
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
    #DF1 <- tables_by_ring_predicted$npp[,c("term", "Ring_2", "Ring_3", "Ring_6")]
    #euc.npp <- colSums(DF1[DF1$term%in%c("Leaf NPP", "Stem NPP", "Fine Root NPP",
    #                                     "Coarse Root NPP", "Bole Root NPP",
    #                                     "Other NPP", "Understorey NPP",
    #                                     "Leaf consumption"), 2:4]) 
    #
    #### prepare input files - DukeFACE NPP
    #DF1 <- read.csv("data/ess_dive_f15e8d846622729_20190628T143153130/DUKE_a.csv")
    #dkDF <- summaryBy(fc_npp~treatment_fertilisation, data=DF1, FUN=c(mean,sd), keep.names=T)
    #
    #### prepare input files - ORNLFACE NPP
    #DF1 <- read.csv("data/ess_dive_f15e8d846622729_20190628T143153130/ORNL_a.csv")
    #orDF <- summaryBy(fc_npp~co2, data=DF1, FUN=c(mean,sd), keep.names=T)
    #
    #
    #### prepare input files - ORNLFACE NPP
    #DF1 <- read.csv("data/ess_dive_f15e8d846622729_20190628T143153130/RHIN_a.csv")
    #asDF <- summaryBy(fc_npp~co2, data=DF1, FUN=c(mean,sd), keep.names=T)
    #
    #### prepare input files - PopFACE NPP
    #pop.npp.m <- (1132 + 1437 + 1130)/3
    #pop.npp.sd <- sqrt((57^2 + 31^2 + 35^2)/3)
    
    
    
    ### assign values - eucFACE
    faceDF$MAT[faceDF$Site=="EucFACE"] <- mean(euc.t$Tair)
    faceDF$MAP[faceDF$Site=="EucFACE"] <- mean(euc.p$Rain)
    faceDF$Age[faceDF$Site=="EucFACE"] <- 90
    faceDF$soilN[faceDF$Site=="EucFACE"] <- NA
    faceDF$soilP[faceDF$Site=="EucFACE"] <- NA
    #faceDF$NPP[faceDF$Site=="EucFACE"] <- mean(euc.npp)
    
    faceDF$MAT_sd[faceDF$Site=="EucFACE"] <- sd(euc.t$Tair)
    faceDF$MAP_sd[faceDF$Site=="EucFACE"] <- sd(euc.p$Rain)
    faceDF$soilN_sd[faceDF$Site=="EucFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="EucFACE"] <- NA
    #faceDF$NPP_sd[faceDF$Site=="EucFACE"] <- sd(euc.npp)
    
    ### assign values - DukeFACE
    faceDF$MAT[faceDF$Site=="DukeFACE"] <- 14.8
    faceDF$MAP[faceDF$Site=="DukeFACE"] <- 1081
    faceDF$Age[faceDF$Site=="DukeFACE"] <- 13
    faceDF$soilN[faceDF$Site=="DukeFACE"] <- 0.079
    faceDF$soilP[faceDF$Site=="DukeFACE"] <- NA
    #faceDF$NPP[faceDF$Site=="DukeFACE"] <- dkDF$fc_npp.mean
    
    faceDF$MAT_sd[faceDF$Site=="DukeFACE"] <- 0.6
    faceDF$MAP_sd[faceDF$Site=="DukeFACE"] <-168
    faceDF$soilN_sd[faceDF$Site=="DukeFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="DukeFACE"] <- NA
    #faceDF$NPP_sd[faceDF$Site=="DukeFACE"] <- dkDF$fc_npp.sd
    
    ### assign values - ORNLFACE
    faceDF$MAT[faceDF$Site=="ORNLFACE"] <- 14.8
    faceDF$MAP[faceDF$Site=="ORNLFACE"] <- 1221
    faceDF$Age[faceDF$Site=="ORNLFACE"] <- 10
    faceDF$soilN[faceDF$Site=="ORNLFACE"] <- 0.112
    faceDF$soilP[faceDF$Site=="ORNLFACE"] <- NA
    #faceDF$NPP[faceDF$Site=="ORNLFACE"] <- orDF$fc_npp.mean[orDF$co2=="AMB"]
    
    faceDF$MAT_sd[faceDF$Site=="ORNLFACE"] <- 0.9
    faceDF$MAP_sd[faceDF$Site=="ORNLFACE"] <- 218
    faceDF$soilN_sd[faceDF$Site=="ORNLFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="ORNLFACE"] <- NA
    #faceDF$NPP_sd[faceDF$Site=="ORNLFACE"] <- orDF$fc_npp.sd[orDF$co2=="AMB"]
    
    ### assign values - AspenFACE
    faceDF$MAT[faceDF$Site=="AspenFACE"] <- 6.0
    faceDF$MAP[faceDF$Site=="AspenFACE"] <- 662
    faceDF$Age[faceDF$Site=="AspenFACE"] <- 1.0
    faceDF$soilN[faceDF$Site=="AspenFACE"] <- NA
    faceDF$soilP[faceDF$Site=="AspenFACE"] <- NA
    #faceDF$NPP[faceDF$Site=="AspenFACE"] <- asDF$fc_npp.mean[asDF$co2=="AMB"]
    
    faceDF$MAT_sd[faceDF$Site=="AspenFACE"] <- 0.8
    faceDF$MAP_sd[faceDF$Site=="AspenFACE"] <- 122
    faceDF$soilN_sd[faceDF$Site=="AspenFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="AspenFACE"] <- NA
    #faceDF$NPP_sd[faceDF$Site=="AspenFACE"] <- asDF$fc_npp.sd[asDF$co2=="AMB"]
    
    ### assign values - PopFACE
    faceDF$MAT[faceDF$Site=="PopFACE"] <- 14
    faceDF$MAP[faceDF$Site=="PopFACE"] <- 800
    faceDF$Age[faceDF$Site=="PopFACE"] <- 3
    faceDF$soilN[faceDF$Site=="PopFACE"] <- 0.11 
    faceDF$soilP[faceDF$Site=="PopFACE"] <- NA
    #faceDF$NPP[faceDF$Site=="PopFACE"] <- pop.npp.m
    
    faceDF$MAT_sd[faceDF$Site=="PopFACE"] <- NA
    faceDF$MAP_sd[faceDF$Site=="PopFACE"] <- NA
    faceDF$soilN_sd[faceDF$Site=="PopFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="PopFACE"] <- 0.0001 * sqrt(3)
    #faceDF$NPP_sd[faceDF$Site=="PopFACE"] <- pop.npp.sd
    
    
    ### assign values - WebFACE
    faceDF$MAT[faceDF$Site=="WebFACE"] <- 11
    faceDF$MAP[faceDF$Site=="WebFACE"] <- 900
    faceDF$Age[faceDF$Site=="WebFACE"] <- 110
    faceDF$soilN[faceDF$Site=="WebFACE"] <- 0.28
    faceDF$soilP[faceDF$Site=="WebFACE"] <- NA
    faceDF$NPP[faceDF$Site=="WebFACE"] <- NA
    
    faceDF$MAT_sd[faceDF$Site=="WebFACE"] <- NA
    faceDF$MAP_sd[faceDF$Site=="WebFACE"] <- NA
    faceDF$soilN_sd[faceDF$Site=="WebFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="WebFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="WebFACE"] <- NA
    
    ### assign values - BiForFACE
    faceDF$MAT[faceDF$Site=="BiForFACE"] <- 9
    faceDF$MAP[faceDF$Site=="BiForFACE"] <- 690
    faceDF$Age[faceDF$Site=="BiForFACE"] <- 150
    faceDF$soilN[faceDF$Site=="BiForFACE"] <- NA
    faceDF$soilP[faceDF$Site=="BiForFACE"] <- NA
    faceDF$NPP[faceDF$Site=="BiForFACE"] <- NA
    
    faceDF$MAT_sd[faceDF$Site=="BiForFACE"] <- NA
    faceDF$MAP_sd[faceDF$Site=="BiForFACE"] <- NA
    faceDF$soilN_sd[faceDF$Site=="BiForFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="BiForFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="BiForFACE"] <- NA
    
    
    ### assign values - AmazonFACE
    faceDF$MAT[faceDF$Site=="AmazonFACE"] <- 26.7
    faceDF$MAP[faceDF$Site=="AmazonFACE"] <- 2400
    faceDF$Age[faceDF$Site=="AmazonFACE"] <- 200
    faceDF$soilN[faceDF$Site=="AmazonFACE"] <- NA
    faceDF$soilP[faceDF$Site=="AmazonFACE"] <- NA
    faceDF$NPP[faceDF$Site=="AmazonFACE"] <- NA
    
    faceDF$MAT_sd[faceDF$Site=="AmazonFACE"] <- NA
    faceDF$MAP_sd[faceDF$Site=="AmazonFACE"] <- NA
    faceDF$soilN_sd[faceDF$Site=="AmazonFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="AmazonFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="AmazonFACE"] <- NA
    
    
    ### assign values - SwedFACE
    faceDF$MAT[faceDF$Site=="SwedFACE"] <- 5.5
    faceDF$MAP[faceDF$Site=="SwedFACE"] <- 688
    faceDF$Age[faceDF$Site=="SwedFACE"] <- 34
    faceDF$soilN[faceDF$Site=="SwedFACE"] <- NA
    faceDF$soilP[faceDF$Site=="SwedFACE"] <- NA
    faceDF$NPP[faceDF$Site=="SwedFACE"] <- NA
    
    faceDF$MAT_sd[faceDF$Site=="SwedFACE"] <- NA
    faceDF$MAP_sd[faceDF$Site=="SwedFACE"] <- NA
    faceDF$soilN_sd[faceDF$Site=="SwedFACE"] <- NA
    faceDF$soilP_sd[faceDF$Site=="SwedFACE"] <- NA
    faceDF$NPP_sd[faceDF$Site=="SwedFACE"] <- NA
    
    
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
    
    ### plot global soil N and P spatial maps, with face points projected onto it
    #source("R/global_comparison/read_global_soil_data.R")
    #faceDF <- read_global_soil_data(faceDF=faceDF)
    
    ### calcualte soil N as g N m-2 for top 50 cm soil, based on bulk density
    #faceDF$soilN <- faceDF$TN1 / 100 * faceDF$BD1 * 1000000 * 0.5
    
    ### get soil labile P (g m-2) from Yang et al. 2014
    #faceDF <- read_Yang_soil_P_data(faceDF)
    
    ### ignore AmazonFACE and SwedFACE
    #faceDF$soilN[faceDF$Site%in%c("AmazonFACE", "SwedFACE")] <- NA
    #faceDF$labP[faceDF$Site%in%c("AmazonFACE", "SwedFACE")] <- NA
    
    ### make NP lines
    #ab_1 <- data.frame(faceDF$soilN, faceDF$soilN / 20)
    #colnames(ab_1) <- c("ab_n", "ab_p")
    
    #ab_2 <- data.frame(faceDF$soilN, faceDF$soilN / 100)
    #colnames(ab_2) <- c("ab_n", "ab_p")
    

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
                                         15:24))
    
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
                 "AmazonFACE",
                 "BiForFACE",
                 "SwedFACE",
                 "FlakalidenWTC")      
    
    
    ### create plotting settings
    col.list <- c(viridis(7), "red", brewer.pal(9,"Paired"))
    
    
    ### density plot
    require(ks)
 
    site.list <- c(1,2,3,4,5,6,12)
        
    pdf("output/density_plot_MAT_MAP.pdf")
    
    # subsetting dataframe
    DF <- data.frame(plotDF1[plotDF1$Biome %in% site.list, "MAT"],
                     plotDF1[plotDF1$Biome %in% site.list, "MAP"])
    
    l <- nrow(DF)
    
    if(l <= 10000) {
        H <- Hpi(x=DF)      # optimal bandwidth estimation
        est<- kde(x=DF, H=H, compute.cont=TRUE)     # kernel density estimation
    } else {
        DF.sub <- DF[sample(nrow(DF), 10000),]
        # kernel density estimation
        H <- Hpi(x=DF.sub)      # optimal bandwidth estimation
        est<- kde(x=DF.sub, H=H, compute.cont=TRUE)     # kernel density estimation
    }
    
    # set contour probabilities for drawing contour levels
    cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
    
    plot(est, cont=seq(1,100,by=1), display="filled.contour", 
         ylab="Precipitation", xlab="Temperature", 
         ylim=c(0,8000), xlim=c(-30,40),las=1, 
         cex.axis=2.0, cex.main = 3.5, cex.lab = 3, add=F) 
    plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
    plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    
    col.list2 <- c("red", brewer.pal(9,"Paired"))
    
    ### add FACE points
    points(faceDF$MAT, faceDF$MAP, col=col.list2, pch=c(17, rep(15, 9)))
    
    dev.off()
    
    ### alternative
    plotDF2 <- subset(plotDF1, Biome%in%c(1,2,3,4,5,6,12))
 
    col.list2 <- c("red", brewer.pal(9,"Paired"))
    
    forests2 <- c("EucFACE",
                 "DukeFACE",
                 "ORNLFACE",
                 "AspenFACE",
                 "PopFACE",
                 "WebFACE",
                 "AmazonFACE",
                 "BiForFACE",
                 "SwedFACE",
                 "FlakalidenWTC") 
        
    p1 <- ggplot() +
        geom_hex(plotDF2, mapping=aes(x=MAT, y=MAP), bins = 50) +
        scale_fill_continuous(type = "viridis") +
        geom_point(faceDF, mapping=aes(x=MAT, y=MAP, 
                                        color=factor(Biome),
                                        shape=factor(Biome), 
                                        size = factor(Biome)),
                   inherit.aes = FALSE)+
        xlab(expression("MAT (" * degree * "C)")) + 
        ylab("MAP (mm)") +
        scale_color_manual(name="", 
                           values=col.list2,
                           labels=forests2) +
        scale_shape_manual(values=c(17, rep(15, 9)),
                           labels=forests2) +
        scale_size_manual(values=c(rep(4, 10)),
                          labels=forests2)+
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
               #fill = viridis,
               color = guide_legend(ncol=5, override.aes = 
                                       list(size = 4, shape=c(17, rep(15, 9)), 
                                            colour=col.list2)))
    
    
    pdf("output/density_plot_MAT_MAP_2.pdf")
    
    plot(p1)

    dev.off()
    
        
}
