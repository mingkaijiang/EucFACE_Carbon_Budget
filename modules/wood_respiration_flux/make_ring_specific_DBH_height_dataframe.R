make_ring_specific_DBH_height_dataframe <- function(ring_area) {
    #### download the data from HIEv
    download_diameter_data()
    
    #### read in 2012-15 data sets
    f13 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2012-13_RAW-V1.csv"))
    f14 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2013-14_RAW_V1.csv"))
    f15 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2015_RAW_V1.csv"))
    f16 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2016_RAW_V1.csv"))
    # this file is not on HIEv yet!
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    
    #### Read in additional files that I used when doing the data analysis
    classif <- read.csv("download/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv",stringsAsFactors = FALSE)

    
    #### Merge the files
    all <- merge(classif,f12,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f13,by=c("Tree","Ring","CO2.trt")) 
    all <- merge(all,f14,by=c("Tree","Ring","CO2.trt"))  
    all <- merge(all,f15,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f16,by=c("Tree","Ring","CO2.trt"))
    
    #### remove dead trees
    #all$Active.FALSE.means.dead.[is.na(all$Active.FALSE.means.dead.)] <- "TRUE"
    #all <- subset(all, Active.FALSE.means.dead.== TRUE)

    #### remove "CORR" columns and dead column
    uncorr <- all[,-grep("CORR",names(all))]
    uncorr <- uncorr[,-grep("Coor",names(uncorr))]
    uncorr <- uncorr[,names(uncorr) != "Active.FALSE.means.dead."]
    
    #### make a long-form version of dataframe
    long <- reshape(uncorr,idvar="Tree",varying=list(7:58),direction="long")
    dates <- names(uncorr)[7:58]
    long$Date <- c(rep(Sys.Date(),length(long$time)))  #wasn't sure how else to make this column date type
    for (i in (1:length(long$time))) {
        long$Date[i] <- as.Date(dates[long$time[i]],format="X%d.%m.%Y")
    }
    long <- renameCol(long,c("X17.02.2011"),c("diam"))
    
    long$diam <- as.numeric(long$diam)
    
    #### add biomass to long-form dataframe
    long$biom <- allom_agb(long$diam)  # in kg DM
    
    #### The bark removal affects the diameters mid-year. 
    #### Hence, just calculate biomass once per year 
    #### Specify dates here - may update this to March in future
    dates <- c(as.Date("2015-12-14"))
    data <- long[long$Date %in% dates,]
    data <- as.data.frame(data)
    
    #write.csv(data, "output/dbh_height.csv", row.names=F)
    
    #### read in Kashif's data
    myDF <- read.csv("data/lidar_data_eucface_HIEv.csv")
    #myDF2 <- read.csv(file.path(getToPath(), "FACE_RA_P0037_STEMVOL-LIDAR_20150526_L2.csv")) 
    
    ### look at plot level summary
    outDF <- summaryBy(Height+diam~Ring, data=data, FUN=sum, keep.names=T, na.rm=T)
    outDF2 <- summaryBy(dbh_m+height_m+stemarea_m2+total.woodarea_m2~Ring, data=myDF, FUN=sum, keep.names=T, na.rm=T)
    outDF2$dbh_cm <- outDF2$dbh_m * 100
    
    outDF$Height2 <- outDF2$height_m
    outDF$diam2 <- outDF2$dbh_m * 100
    #outDF$sfc2 <- outDF2$stemarea_m2
    outDF$sfc2 <- outDF2$total.woodarea_m2
    
    colnames(outDF) <- c("Ring", "David_height", "David_diam", "Kashif_height", "Kashif_diam", "Kashif_stem_surface_area")
    
    
    ### obtain relationship based on Kashif's data between DBH and stem sfc area
    #with(outDF2, plot(stemarea_m2~dbh_cm))
    #lm <- lm(stemarea_m2~dbh_cm, data=outDF2)
    lm <- lm(total.woodarea_m2~dbh_cm, data=outDF2)
    
    outDF$wood_surface_area <- outDF$David_diam * coef(lm)[[2]] + coef(lm)[[1]]
    
    #with(outDF, plot(wood_surface_area~Kashif_stem_surface_area))
    
    outDF2 <- summaryBy(wood_surface_area ~ Ring, FUN=sum, data=outDF, keep.names=TRUE) %>%
        mutate(wood_surface_area = wood_surface_area / ring_area,
               Date = "2015-05-26",
               Ring = as.numeric(Ring)) %>%
        dplyr::select(Date, Ring, wood_surface_area)
    

    return(outDF2)

}

