make_coarse_root_pool <- function(froot) {
    
    ### Read in Juan's coarseroot biomass estimates
    myDF <- read.csv("data/CRBFACE.csv")
    colnames(myDF) <- c("Ring", "Date", "Depth", "n", "CRB", "sd", "se", "ci")
    
    ### subset only top 10 cm, because data below it is very variable (also including more woody roots)
    myDF <- subset(myDF, Depth == 0)
    
    
    ### if se is within 80% of mean, replace data points with treatment means
    myDF$CRB2 <- ifelse(myDF$CRB * 0.8 < myDF$se, NA, myDF$CRB)
    myDF$CRB2[myDF$Ring==5 & myDF$Date=="14-Feb"] <- mean(myDF$CRB2[myDF$Date=="14-Feb"&myDF$Ring%in%c(1,4,5)], na.rm=T)
    myDF$CRB2[myDF$Ring==1 & myDF$Date=="14-Jun"] <- myDF$CRB[myDF$Date=="14-Jun"&myDF$Ring==1]
    myDF$CRB2[myDF$Ring==2 & myDF$Date=="14-Jun"] <- mean(myDF$CRB2[myDF$Date=="14-Jun"&myDF$Ring%in%c(2,3,6)], na.rm=T)
    myDF$CRB2[myDF$Ring==2 & myDF$Date=="14-Dec"] <- mean(myDF$CRB2[myDF$Date=="14-Dec"&myDF$Ring%in%c(2,3,6)], na.rm=T)
    
    ### get the date right
    frb1 <- read.csv("temp_files/EucFACERootsRingDateDepth.csv")
    frb1$Date <- as.Date(frb1$Dateform, format="%d-%m-%Y")
    dates <- unique(frb1$Date)
    dates <- dates[-7]
    myDF$Date2 <- rep(dates, each=6)
    
    # assign C concentrations
    myDF$c_frac <- c_fraction_croot
    
    myDF$coarseroot_pool_0_10cm <- myDF$CRB2 * myDF$c_frac
    
    ### assign fineroot to the DF
    froot <- froot[froot$Date%in%dates,]
    myDF$froot_0_10 <- froot$fineroot_0_10_cm
    myDF$froot_10_30 <- froot$fineroot_10_30_cm
    
    ### calculate fineroot to total root fraction in top 10 cm
    myDF$f_c_frac <- myDF$froot_0_10 / myDF$coarseroot_pool_0_10cm
    
    ### estimate coarseroot pool in 10 - 30 cm
    myDF$coarseroot_pool_10_30cm <- myDF$froot_10_30 / myDF$f_c_frac
    
    ### clean
    outDF <- myDF[,c("Date2", "Ring", "coarseroot_pool_0_10cm", "coarseroot_pool_10_30cm")]
    outDF$coarse_root_pool <- outDF$coarseroot_pool_0_10cm + outDF$coarseroot_pool_10_30cm
    
    colnames(outDF) <- c("Date", "Ring", "coarseroot_pool_0_10cm", "coarseroot_pool_10_30cm", "coarse_root_pool")
    outDF <- outDF[,c("Date", "Ring", "coarse_root_pool", "coarseroot_pool_0_10cm", "coarseroot_pool_10_30cm")]
    
    ### return
    return(outDF)

    
}