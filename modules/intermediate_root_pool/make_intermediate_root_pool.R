make_intermediate_root_pool <- function(bkDF) {
    
    ### estimate ring-specific bulk density
    bk <- summaryBy(bulk_density_kg_m3~ring, FUN=mean, data=bkDF, keep.names=T)
    
    ### use Johanna's data to estimate the relative contribution
    ### of fineroot (< 2mm) coarseroot (2-3 mm) to total root
    myDF <- read.csv("data/EucFACE_P0091_roots_SEP2017.csv")
    myDF$depth <- as.character(myDF$depth)
    myDF <- myDF[myDF$depth%in%c("0-10 cm", "10-30 cm"),]

    ### calculate fractional coefficient
    myDF$f_c_1 <- myDF$root.smal.2.mm.mg.g / myDF$root.2t3.mm.mg.g
    
    ### assign soil bulk density
    for (i in 1:6) {
        myDF$bk[myDF$ring==i] <- bk$bulk_density_kg_m3[bk$ring==i]
    }

    ### calculate sum biomass and convernt unit from g g-1 to mg m-2
    myDF$f_biomass[myDF$depth=="0-10 cm"] <- (myDF$root.smal.2.mm.mg.g[myDF$depth=="0-10 cm"]) * 
        myDF$bk[myDF$depth=="0-10 cm"] * 1000 * 0.1 / 1000
    
    myDF$f_biomass[myDF$depth=="10-30 cm"] <- (myDF$root.smal.2.mm.mg.g[myDF$depth=="10-30 cm"]) * 
        myDF$bk[myDF$depth=="10-30 cm"] * 1000 * 0.2 / 1000
    
    ### fit linear relationship for each depth
    fit1 <- lm(f_c_1~f_biomass, data=myDF[myDF$depth=="0-10 cm",])
    fit2 <- lm(f_c_1~f_biomass, data=myDF[myDF$depth=="10-30 cm",])

    ### get the fineroot biomass data
    frbDF <- read.csv("temp_files/EucFACERootsRingDateDepth.csv")
    frbDF$Date <- as.Date(frbDF$Dateform, format="%d-%m-%Y")
    
    frbDF$fc1 <- frbDF$FRB_0.10cm * coefficients(fit1)[[2]] + coefficients(fit1)[[1]]
    frbDF$fc2 <- frbDF$FRB_10.30cm * coefficients(fit2)[[2]] + coefficients(fit2)[[1]]

    ### get coarseroot biomass
    frbDF$IRB_0.10cm <- frbDF$FRB_0.10cm / frbDF$fc1
    frbDF$IRB_10.30cm <- frbDF$FRB_10.30cm / frbDF$fc2
    
    ### calculate c cotent, based on fineroot content
    frbDF$intermediate_root_pool_0_10cm <- frbDF$IRB_0.10cm * frbDF$C0_0.10cm / 100
    frbDF$intermediate_root_pool_10_30cm <- frbDF$IRB_10.30cm * frbDF$C0_10.30cm / 100
    
    frbDF$intermediate_root_pool <- frbDF$intermediate_root_pool_0_10cm + frbDF$intermediate_root_pool_10_30cm
    
    ### clean
    outDF <- frbDF[,c("Date", "Ring", "intermediate_root_pool", "intermediate_root_pool_0_10cm", "intermediate_root_pool_10_30cm")]

    

    
    ### return
    return(outDF)

    
}
