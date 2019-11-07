make_coarse_root_pool <- function(c_frac, fr_pool, ir_pool) {
    ### Method 1 of making total root biomass pool
    ### Based on Snowdon et al., 2000. National Carbon accounting system:
    ### synthesis of allometrics, review of root biomass and design of future woody biomass sampling strategies.
    ### Australian Greenhouse Office. Technical Report No. 17.
    ### Relationship: ln(root biomass) = 0.787 * ln(stand basal area) + 1.218
    ### Root biomass in t/ha, basal area in m2/ha.
    
    #- download the data from HIEv
    download_diameter_data()
    
    #- read in 2012-15 data sets
    f13 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2012-13_RAW-V1.csv"))
    f14 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2013-14_RAW_V1.csv"))
    f15 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2015_RAW_V1.csv"))
    f16 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2016_RAW_V1.csv"))

    # this file is not on HIEv yet!
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    
    ########################
    # Read in additional files that I used when doing the data analysis
    classif <- read.csv("download/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv",stringsAsFactors = FALSE)
    classif$Active.FALSE.means.dead.[classif$Tree == 608] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 125] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 206] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 210] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 212] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 510] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 518] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 520] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 524] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 527] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 531] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 605] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 615] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 616] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 617] <- FALSE  # This tree dead
    #classif$Active.FALSE.means.dead.[classif$Tree == 101] <- FALSE  # This tree dead in 2018
    #classif$Active.FALSE.means.dead.[classif$Tree == 219] <- FALSE  # This tree dead in 2018
    #classif$Active.FALSE.means.dead.[classif$Tree == 220] <- FALSE  # This tree dead in 2018
    #classif$Active.FALSE.means.dead.[classif$Tree == 621] <- FALSE  # This tree dead in 2018
    
    # Merge the files
    all <- merge(classif,f12,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f13,by=c("Tree","Ring","CO2.trt")) 
    all <- merge(all,f14,by=c("Tree","Ring","CO2.trt"))  
    all <- merge(all,f15,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f16,by=c("Tree","Ring","CO2.trt"))
    
    # remove dead trees
    all$Active.FALSE.means.dead.[is.na(all$Active.FALSE.means.dead.)] <- "TRUE"
    all <- subset(all, Active.FALSE.means.dead.== TRUE)
    #all <- all[complete.cases(all),]
    
    # remove "CORR" columns and dead column
    uncorr <- all[,-grep("CORR",names(all))]
    uncorr <- uncorr[,-grep("Coor",names(uncorr))]
    
    uncorr <- uncorr[,names(uncorr) != "Active.FALSE.means.dead."]
    
    # make a long-form version of dataframe
    long <- reshape(uncorr,idvar="Tree",varying=list(7:58),direction="long")
    dates <- names(uncorr)[7:58]
    long$Date <- c(rep(Sys.Date(),length(long$time)))  
    for (i in (1:length(long$time))) {
        long$Date[i] <- as.Date(dates[long$time[i]],format="X%d.%m.%Y")
    }
    long <- renameCol(long,c("X17.02.2011"),c("diam"))
    
    long$diam <- as.numeric(long$diam)
    
    # Calculate Basal area, m2
    long$ba <- 0.00007854 * (long$diam)^2 
    
    dates <- c(as.Date("2012-12-20"),as.Date("2013-12-20"),
               as.Date("2014-12-23"),as.Date("2015-12-14"),
               as.Date("2016-12-21"))
    data <- long[long$Date %in% dates,]
    
    data$ba2 <- data$ba 
    data$biomass <- exp(0.787 * log(data$ba2) + 1.218) 
    
    ### Estimate sapwood and heartwood C fraction
    sap.c <- make_sapwood_c_n_fraction()
    data$c_frac[data$Ring %in% c(2, 3, 6)] <- sap.c$aCO2[sap.c$variable=="C"]
    data$c_frac[data$Ring %in% c(1, 4, 5)] <- sap.c$eCO2[sap.c$variable=="C"]
    
    # convert from g matter m-2 to g C m-2
    data$total_root_c_pool <- data$biomass * data$c_frac * water_frac * 1000000
    
    
    # sum across rings and dates
    data.m <- summaryBy(ba~Date+Ring,data=data,FUN=sum,keep.names=T,na.rm=T)
    
    # convert into m2/ha
    data.m$ba2 <- data.m$ba / (ring_area / 10000)
    
    # Calculate root biomass, return in t/ha, then convert to g/m2
    data.m$biomass <- exp(0.787 * log(data.m$ba2) + 1.218) * 100
    
    # calculate fineroot biomass
    fr_pool$c_frac[fr_pool$Ring==1] <- 0.426
    fr_pool$c_frac[fr_pool$Ring==2] <- 0.413
    fr_pool$c_frac[fr_pool$Ring==3] <- 0.399
    fr_pool$c_frac[fr_pool$Ring==4] <- 0.415
    fr_pool$c_frac[fr_pool$Ring==5] <- 0.42
    fr_pool$c_frac[fr_pool$Ring==6] <- 0.401
    
    fr_pool$biomass <- fr_pool$fineroot_pool/fr_pool$c_frac
    
    fr.ring <- summaryBy(biomass~Ring, data=fr_pool, FUN=mean, keep.names=T, na.rm=T)
    
    
    # calculate coarseroot biomass
    ir_pool$c_frac[ir_pool$Ring==1] <- 0.426
    ir_pool$c_frac[ir_pool$Ring==2] <- 0.413
    ir_pool$c_frac[ir_pool$Ring==3] <- 0.399
    ir_pool$c_frac[ir_pool$Ring==4] <- 0.415
    ir_pool$c_frac[ir_pool$Ring==5] <- 0.42
    ir_pool$c_frac[ir_pool$Ring==6] <- 0.401
    
    ir_pool$biomass <- ir_pool$intermediate_root_pool/ir_pool$c_frac
    
    ir.ring <- summaryBy(biomass~Ring, data=ir_pool, FUN=mean, keep.names=T, na.rm=T)
    
    # convert from g matter m-2 to g C m-2
    data.m$total_root_biomass <- data.m$biomass * water_frac
    
    # subtract fineroot biomass out, assuming one froot value per ring
    for (i in 1:6) {
        data.m$coarseroot_biomass[data.m$Ring == i] <- data.m$total_root_biomass[data.m$Ring == i] - fr.ring$biomass[fr.ring$Ring == i] - ir.ring$biomass[ir.ring$Ring == i]
            
    }
    
    data.m$c_frac[data.m$Ring %in% c(2, 3, 6)] <- sap.c$aCO2[sap.c$variable=="C"]
    data.m$c_frac[data.m$Ring %in% c(1, 4, 5)] <- sap.c$eCO2[sap.c$variable=="C"]
    
    data.m$coarseroot_c_pool <- data.m$coarseroot_biomass * data.m$c_frac

    # output
    out <- data.m[,c("Date","Ring","coarseroot_c_pool")]
    
    colnames(out) <- c("Date", "Ring", "coarse_root_pool")
    
    # Only use data period 2012-2016
    out <- out[out$Date<="2016-12-31",]

    ### Decision on what to return
    return(out)

    
}