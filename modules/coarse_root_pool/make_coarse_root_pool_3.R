make_coarse_root_pool_3 <- function() {
    
    ### use Johanna's data to estimate the relative contribution
    ### of fineroot (< 2mm) coarseroot (2-3 mm) to total root
    myDF <- read.csv("data/EucFACE_P0091_roots_SEP2017.csv")
    myDF$depth <- as.character(myDF$depth)
    myDF <- myDF[myDF$depth%in%c("0-10 cm", "10-30 cm"),]
    
    ### check percentage
    #myDF$f_pct_1 <- myDF$root.smal.2.mm.mg.g/(myDF$root.smal.2.mm.mg.g+myDF$root.2t3.mm.mg.g)
    #myDF$c_pct_1 <- myDF$root.2t3.mm.mg.g/(myDF$root.smal.2.mm.mg.g+myDF$root.2t3.mm.mg.g)
    #
    #myDF$f_pct_2 <- myDF$root.smal.2.mm.mg.g/(myDF$tot.root.mg.g)
    #myDF$c_pct_2 <- myDF$root.2t3.mm.mg.g/(myDF$tot.root.mg.g)
    #myDF$b_pct_2 <- myDF$root.large.3.mm.mg.g/(myDF$tot.root.mg.g)
    
    ### calculate fractional coefficient
    myDF$f_c_1 <- myDF$root.smal.2.mm.mg.g / myDF$root.2t3.mm.mg.g
    myDF$l_biomass <- myDF$root.2t3.mm.mg.g + myDF$root.large.3.mm.mg.g
    
    with(myDF[myDF$depth=="0-10 cm",], plot(f_c_1~l_biomass))
    with(myDF[myDF$depth=="10-30 cm",], plot(f_c_1~l_biomass))
    
    #fDF <- summaryBy(f_c_1~CO2+depth, data=myDF, FUN=mean, keep.names=T, na.rm=T)
    fDF <- summaryBy(f_c_1~depth, data=myDF, FUN=mean, keep.names=T, na.rm=T)
    f.value <- mean(myDF$f_c_1, na.rm=T)
    
    ### get the fineroot biomass data
    frbDF <- read.csv("temp_files/EucFACERootsRingDateDepth.csv")
    frbDF$Date <- as.Date(frbDF$Dateform, format="%d-%m-%Y")

    ### assign fineroot to the DF
    #frbDF$croot_0_10[frbDF$CO2=="Elevated"] <- frbDF$FRB_0.10cm[frbDF$CO2=="Elevated"] / fDF$f_c_1[fDF$CO2=="Elevated"&fDF$depth=="0-10 cm"]
    #frbDF$croot_0_10[frbDF$CO2=="Ambient"] <- frbDF$FRB_0.10cm[frbDF$CO2=="Ambient"] / fDF$f_c_1[fDF$CO2=="Ambient"&fDF$depth=="0-10 cm"]
    #
    #frbDF$croot_10_30[frbDF$CO2=="Elevated"] <- frbDF$FRB_10.30cm[frbDF$CO2=="Elevated"] / fDF$f_c_1[fDF$CO2=="Elevated"&fDF$depth=="10-30 cm"]
    #frbDF$croot_10_30[frbDF$CO2=="Ambient"] <- frbDF$FRB_10.30cm[frbDF$CO2=="Ambient"] / fDF$f_c_1[fDF$CO2=="Ambient"&fDF$depth=="10-30 cm"]
    
    #frbDF$croot_0_10 <- frbDF$FRB_0.10cm / fDF$f_c_1[fDF$depth=="0-10 cm"]
    #frbDF$croot_10_30 <- frbDF$FRB_10.30cm / fDF$f_c_1[fDF$depth=="10-30 cm"]

    frbDF$croot_0_10 <- frbDF$FRB_0.10cm / f.value
    frbDF$croot_10_30 <- frbDF$FRB_10.30cm / f.value
    
    ### calculate c cotent, based on fineroot content
    frbDF$coarseroot_pool_0_10cm <- frbDF$croot_0_10 * frbDF$C0_0.10cm / 100
    frbDF$coarseroot_pool_10_30cm <- frbDF$croot_10_30 * frbDF$C0_10.30cm / 100
    
    frbDF$coarse_root_pool <- frbDF$coarseroot_pool_0_10cm + frbDF$coarseroot_pool_10_30cm
    
    ### clean
    outDF <- frbDF[,c("Date", "Ring", "coarse_root_pool", "coarseroot_pool_0_10cm", "coarseroot_pool_10_30cm")]

    
    #test <- summaryBy(coarse_root_pool~Ring, FUN=mean, data=outDF, keep.names=T)
    #test$trt[test$Ring%in%c(2,3,6)] <- "amb"
    #test$trt[test$Ring%in%c(1,4,5)] <- "ele"
    #summaryBy(coarse_root_pool~trt, FUN=mean, data=test, keep.names=T)

    ### return
    return(outDF)

    
}
