make_coarse_root_pool_2 <- function() {
    
    ### Read in Juan's coarseroot biomass estimates
    myDF <- read.csv("data/CRBFACE.csv")
    colnames(myDF) <- c("Ring", "Date", "Depth", "n", "CRB", "sd", "se", "ci")
    
    ### assign 30 cm data to data frame
    subDF <- subset(myDF, Depth==30)
    
    myDF <- subset(myDF, Depth == 0)
    myDF$CRB_30 <- subDF$CRB
    myDF$CRB_tot <- myDF$CRB + myDF$CRB_30
    
    ### get the date right
    frb1 <- read.csv("temp_files/EucFACERootsRingDateDepth.csv")
    frb1$Date <- as.Date(frb1$Dateform, format="%d-%m-%Y")
    dates <- unique(frb1$Date)
    dates <- dates[-7]
    myDF$Date2 <- rep(dates, each=6)
    frb2 <- frb1[frb1$Dateform!="26-02-2016",]
    
    ### assign fineroot to the DF
    myDF$froot_0_10 <- frb2$FRB_0.10cm
    myDF$froot_10_30 <- frb2$FRB_10.30cm
    myDF$froot_tot <- myDF$froot_0_10 + myDF$froot_10_30
    
    test <- median(myDF$froot_10_30) / median(myDF$CRB_30) 
    test <- mean(myDF$froot_10_30) / mean(myDF$CRB_30) 
    
    ### calculate fineroot to total root fraction in top 10 cm
    f.value <- froot_median / croot_median
    f.value.10 <- mean(myDF$froot_0_10 / myDF$CRB)
    f.value.30 <- mean(myDF$froot_10_30 / myDF$CRB_30)
    

    ### recalculate coarseroot bioamss based on fineroot biomass
    frb1$croot_0_10 <- frb1$FRB_0.10cm / f.value.10
    frb1$croot_10_30 <- frb1$FRB_10.30cm / f.value.30

    frb1$coarseroot_pool_0_10cm <- frb1$croot_0_10 * frb1$C0_0.10cm / 100
    frb1$coarseroot_pool_10_30cm <- frb1$croot_10_30 * frb1$C0_10.30cm / 100
    frb1$coarse_root_pool <- frb1$coarseroot_pool_0_10cm + frb1$coarseroot_pool_10_30cm
    
    
    outDF <- frb1[,c("Date", "Ring", "coarse_root_pool", "coarseroot_pool_0_10cm", "coarseroot_pool_10_30cm")]
    
    
    test <- summaryBy(coarse_root_pool~Ring, FUN=mean, data=outDF, keep.names=T)
    test$trt[test$Ring%in%c(2,3,6)] <- "amb"
    test$trt[test$Ring%in%c(1,4,5)] <- "ele"
    summaryBy(coarse_root_pool~trt, FUN=mean, data=test, keep.names=T)

    ### return
    return(outDF)

    
}