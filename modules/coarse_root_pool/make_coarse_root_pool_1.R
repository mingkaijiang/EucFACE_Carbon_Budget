make_coarse_root_pool_1 <- function(c_frac) {
    ### Method 1 of making coarse root biomass pool
    ### Based on Forrester et al. 2006, developed by Misra et al., 1998 for
    ### Eucalyptus nitens. This is an allometric relationship between
    ### coarse root C (roots > 3 mm in diameter) and aboveground stem volume. 
    ### This relatinship was not affected by age, fertiliser or irrigation traetments. 
    ### ln(RB) = 0.75 ln(V)
    ### where RB is coarse root biomass (g), and V is volume (cm3), as
    ### V = D^2 * H, where D is stem diameter (cm) at height of 15 cm, and H is tree height (cm).
    ### D at 15 cm can be derived by:
    ### D = 0.555 + 1.577Dx, where Dx is diameter at 130 cm. 
    
    #- download the data from HIEv
    download_diameter_data()
    
    #- read in 2012-15 data sets
    f13 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2012-13_RAW-V1.csv"))
    f14 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2013-14_RAW_V1.csv"))
    f15 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2015_RAW_V1.csv"))
    # this file is not on HIEv yet!
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    
    ########################
    # Read in additional files that I used when doing the data analysis
    classif <- read.csv("download/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv",stringsAsFactors = FALSE)
    classif$Active.FALSE.means.dead.[classif$Tree == 608] <- FALSE  # This tree dead too
    
    # Merge the files
    all <- merge(classif,f12,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f13,by=c("Tree","Ring","CO2.trt")) 
    all <- merge(all,f14,by=c("Tree","Ring","CO2.trt"))  
    all <- merge(all,f15,by=c("Tree","Ring","CO2.trt"))
    
    # remove dead trees
    all$Active.FALSE.means.dead.[is.na(all$Active.FALSE.means.dead.)] <- "TRUE"
    all <- subset(all, Active.FALSE.means.dead.== TRUE)
    all <- all[complete.cases(all),]
    
    # remove "CORR" columns and dead column
    uncorr <- all[,-grep("CORR",names(all))]
    uncorr <- uncorr[,-grep("Coor",names(uncorr))]
    
    uncorr <- uncorr[,names(uncorr) != "Active.FALSE.means.dead."]
    
    # make a long-form version of dataframe
    long <- reshape(uncorr,idvar="Tree",varying=list(7:51),direction="long")
    dates <- names(uncorr)[7:51]
    long$Date <- c(rep(Sys.Date(),length(long$time)))  
    for (i in (1:length(long$time))) {
        long$Date[i] <- as.Date(dates[long$time[i]],format="X%d.%m.%Y")
    }
    long <- renameCol(long,c("X17.02.2011"),c("diam"))
    
    long$diam <- as.numeric(long$diam)
    
    # add biomass to long-form dataframe
    long$biom <- allom_agb(long$diam)  # in kg DM
    
    # convert diam fro breast height to 15 cm
    long$d15 <- 0.555 + 1.1577 * long$diam 
    
    # calculate volume
    long$volume <- (long$d15^2) * (long$Height * 100)

    # calculate coarseroot biomass pool, unit of g
    long$coarse_biomass <- exp(0.75 * log(long$volume))
    
    dates <- c(as.Date("2012-12-20"),as.Date("2013-12-20"),
               as.Date("2014-12-23"),as.Date("2015-12-14"))
    data <- long[long$Date %in% dates,]
    
    # sum across rings and dates
    data.m <- summaryBy(coarse_biomass~Date+Ring,data=data,FUN=sum,keep.names=T,na.rm=T)
    
    # divide by ring area to get biomass per m2
    data.m$coarse_root_biomass <- data.m$coarse_biomass / ring_area
    
    # convert from g DM m-2 to g C m-2
    data.m$coarse_root_pool <- data.m$coarse_root_biomass * c_frac
    
    # output
    cr_pool <- data.m[,c("Date","Ring","coarse_root_pool")]
    
    return(cr_pool)
    
}