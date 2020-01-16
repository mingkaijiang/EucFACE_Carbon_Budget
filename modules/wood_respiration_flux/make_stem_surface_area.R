
make_stem_surface_area <- function(ring_area){
  
  
  #### read in 2012-15 data sets
  f13 <- read.csv("data/EucFACE_data/FACE_P0025_RA_TREEMEAS_2012-13_RAW-V1.csv")
  f14 <- read.csv("data/EucFACE_data/FACE_P0025_RA_TREEMEAS_2013-14_RAW_V1.csv")
  f15 <- read.csv("data/EucFACE_data/FACE_P0025_RA_TREEMEAS_2015_RAW_V1.csv")
  f16 <- read.csv("data/EucFACE_data/FACE_P0025_RA_TREEMEAS_2016_RAW_V1.csv")
  f12 <- read.csv("data/EucFACE_data/EucFACE_dendrometers2011-12_RAW.csv")
  
  #### Read in additional files that I used when doing the data analysis
  classif <- read.csv("data/EucFACE_data/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv",stringsAsFactors = FALSE)
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
  
  
  #### Merge the files
  all <- merge(classif,f12,by=c("Tree","Ring","CO2.trt"))
  all <- merge(all,f13,by=c("Tree","Ring","CO2.trt")) 
  all <- merge(all,f14,by=c("Tree","Ring","CO2.trt"))  
  all <- merge(all,f15,by=c("Tree","Ring","CO2.trt"))
  all <- merge(all,f16,by=c("Tree","Ring","CO2.trt"))
  
  #### remove dead trees
  all$Active.FALSE.means.dead.[is.na(all$Active.FALSE.means.dead.)] <- "TRUE"
  all <- subset(all, Active.FALSE.means.dead.== TRUE)
  
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
  
  
  #### The bark removal affects the diameters mid-year. 
  #### Hence, just calculate biomass once per year 
  #### Specify dates here - may update this to March in future
  dates <- c(as.Date("2015-12-14"))
  data <- long[long$Date %in% dates,]
  data <- as.data.frame(data)
  

  #### read in lidar data
  myDF <- read.csv("data/EucFACE_data/lidar_data_eucface_HIEv.csv")

  ### look at plot level summary
  outDF <- summaryBy(diam~Ring, data=data, FUN=sum, keep.names=T, na.rm=T)
  outDF2 <- summaryBy(dbh_m+stemarea_m2+total.woodarea_m2~Ring, data=myDF, FUN=sum, keep.names=T, na.rm=T)
  outDF2$dbh_cm <- outDF2$dbh_m * 100
  
  outDF$diam2 <- outDF2$dbh_m * 100
  outDF$sfc2 <- outDF2$total.woodarea_m2
  
  colnames(outDF) <- c("Ring", "census_diam", "lidar_diam", "lidar_stem_surface_area")
  
  ### get stem surface area ~ diameter relationship based on Lidar
  lm <- lm(total.woodarea_m2~dbh_cm, data=outDF2)
  
  outDF$wood_surface_area <- outDF$census_diam * coef(lm)[[2]] + coef(lm)[[1]]
    
  ### normalized to per ground area
  outDF2 <- summaryBy(wood_surface_area ~ Ring, FUN=sum, data=outDF, keep.names=TRUE) %>%
    mutate(wood_surface_area = wood_surface_area / ring_area,
           Date = "2015-05-26",
           Ring = as.numeric(Ring)) %>%
    dplyr::select(Date, Ring, wood_surface_area)

  return(outDF2)
}