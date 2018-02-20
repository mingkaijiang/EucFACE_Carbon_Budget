
# Make the standing dead wood C pool
make_standing_dead_c_pool <- function(ring_area, c_frac) {
    
    ### download the data from HIEv
    download_diameter_data()
    
    ### read in 2012-15 data sets
    f13 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2012-13_RAW-V1.csv"))
    f14 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2013-14_RAW_V1.csv"))
    f15 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2015_RAW_V1.csv"))
    ### this file is not on HIEv yet!
    f12 <- read.csv("~/Documents/Research/Projects/EucFACE_C_Balance/R_repo/data/EucFACE_dendrometers2011-12_RAW.csv")
    
    ########################
    ### Read in additional files
    classif <- read.csv("download/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv",stringsAsFactors = FALSE)
    classif$Active.FALSE.means.dead.[classif$Tree == 608] <- FALSE  # This tree dead too
    
    ########################
    
    ### Merge the files
    all <- merge(classif,f12,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f13,by=c("Tree","Ring","CO2.trt")) 
    all <- merge(all,f14,by=c("Tree","Ring","CO2.trt"))  
    all <- merge(all,f15,by=c("Tree","Ring","CO2.trt"))
    
    ### remove dead trees
    all$Active.FALSE.means.dead.[is.na(all$Active.FALSE.means.dead.)] <- "TRUE"
    all <- subset(all, Active.FALSE.means.dead.!= TRUE)
    all <- all[complete.cases(all),]
    
    ### remove "CORR" columns and dead column
    uncorr <- all[,-grep("CORR",names(all))]
    uncorr <- uncorr[,-grep("Coor",names(uncorr))]
    
    uncorr <- uncorr[,names(uncorr) != "Active.FALSE.means.dead."]
    
    ### make a long-form version of dataframe
    long <- reshape(uncorr,idvar="Tree",varying=list(7:51),direction="long")
    dates <- names(uncorr)[7:51]
    long$Date <- c(rep(Sys.Date(),length(long$time)))  #wasn't sure how else to make this column date type
    for (i in (1:length(long$time))) {
        long$Date[i] <- as.Date(dates[long$time[i]],format="X%d.%m.%Y")
    }
    long <- renameCol(long,c("X17.02.2011"),c("diam"))
    
    long$diam <- as.numeric(long$diam)
    
    ### add biomass to long-form dataframe
    long$biom <- allom_agb(long$diam)  # in kg DM
    
    ### calculate heartwood height,
    ### based on Morais and Pereira. 2007. Annals of forest science
    long$heart_height <- 0.957 * as.numeric(long$Height) - 2.298
    
    ### calculate heartwood diameter,
    ### based on Morais and Pereira, 2007, Annals of Forest Science
    long$heart_diam <- -1.411 + 0.809 * long$diam
    
    ### calculate biomass of heartwood and sapwood
    long$heart_biom <- allom_agb(long$heart_diam)
    long$sap_biom <- long$biom - long$heart_biom
    
    ### The bark removal affects the diameters mid-year. 
    ### Hence, just calculate biomass once per year 
    ### Specify dates here - may update this to March in future
    dates <- c(as.Date("2012-12-20"),as.Date("2013-12-20"),
               as.Date("2014-12-23"),as.Date("2015-12-14"))
    data <- long[long$Date %in% dates,]
    
    ### sum across rings and dates
    data.tot <- summaryBy(biom~Date+Ring,data=data,FUN=sum,keep.names=T,na.rm=T)
    data.heart <- summaryBy(heart_biom~Date+Ring,data=data,FUN=sum,keep.names=T,na.rm=T)
    data.sap <- summaryBy(sap_biom~Date+Ring,data=data,FUN=sum,keep.names=T,na.rm=T)
    
    ### calculate sapwood and heartwood biomass
    out.dat <- cbind(data.tot, data.heart$heart_biom, data.sap$sap_biom)
    colnames(out.dat) <- c("Date", "Ring", "Tot_biom", "Heart_biom", "Sap_biom")
    
    ## divide by ring area to get biomass per m2
    out.dat$wood_pool <- out.dat$Tot_biom / ring_area
    out.dat$sap_pool <- out.dat$Sap_biom /ring_area
    out.dat$heart_pool <- out.dat$Heart_biom /ring_area
    
    ### convert from kg DM m-2 to g C m-2
    out.dat$wood_pool <- out.dat$wood_pool * c_frac * 1000
    out.dat$sap_pool <- out.dat$sap_pool * c_frac * 1000
    out.dat$heart_pool <- out.dat$heart_pool * c_frac * 1000
    
    ### format dataframe to return
    wood_pool <- out.dat[,c("Date", "Ring", "wood_pool", "sap_pool", "heart_pool")]
    
    return(wood_pool)
}