compare_basal_area <- function(ring_area){
    
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
    classDF <- read.csv("download/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv",stringsAsFactors = FALSE)

    #### Merge the files
    all <- merge(classDF,f12,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f13,by=c("Tree","Ring","CO2.trt")) 
    all <- merge(all,f14,by=c("Tree","Ring","CO2.trt"))  
    all <- merge(all,f15,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f16,by=c("Tree","Ring","CO2.trt"))
    
    #### remove "CORR" columns and dead column
    uncorr <- all[,-grep("CORR",names(all))]
    uncorr <- uncorr[,-grep("Coor",names(uncorr))]

    #### make a long-form version of dataframe
    long <- reshape(uncorr,idvar="Tree",varying=list(7:58),direction="long")
    dates <- names(uncorr)[7:58]
    long$Date <- c(rep(Sys.Date(),length(long$time)))  
    for (i in (1:length(long$time))) {
        long$Date[i] <- as.Date(dates[long$time[i]],format="X%d.%m.%Y")
    }
    
    ### There are two diameter information here
    long <- renameCol(long,c("X17.02.2011"),c("diam"))
    long$diam <- as.numeric(long$diam)
    
    #with(long, plot(diam~Diameter))
    #abline(a=0,b=1)

    ### Calculate basal area based on diameter
    long$BA1 <- (long$Diameter / 2)^2 * pi
    long$BA2 <- (long$diam / 2)^2 * pi
    
    ### Summary by ring and treatment
    rDF <- summaryBy(BA1+BA2~Date+Ring, data=long, FUN=sum, keep.names=T, na.rm=T)
    rDF$BA1 <- rDF$BA1/ring_area
    rDF$BA2 <- rDF$BA2/ring_area
    
    ggplot(rDF, aes(Date, BA2, color=as.factor(Ring)))+
        geom_line()
    
    ### Assign treatment
    rDF$Trt[rDF$Ring%in%c(2,3,6)] <- "aCO2"
    rDF$Trt[rDF$Ring%in%c(1,4,5)] <- "eCO2"
    tDF <- summaryBy(BA1+BA2~Date+Trt, data=rDF, FUN=mean, keep.names=T, na.rm=T)
    tDF$BA1 <- tDF$BA1
    tDF$BA2 <- tDF$BA2
    
    ### Plot BA2
    ggplot(tDF, aes(Date, BA2, color=as.factor(Trt))) +
        geom_line()
    
    
    ###    #### Read in additional files that I used when doing the data analysis
    classif <- read.csv("download/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv",stringsAsFactors = FALSE)
    classif$Active.FALSE.means.dead.[classif$Tree == 608] <- FALSE  # This tree dead too
    
    #### Merge the files
    all <- merge(classif,f12,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f13,by=c("Tree","Ring","CO2.trt")) 
    all <- merge(all,f14,by=c("Tree","Ring","CO2.trt"))  
    all <- merge(all,f15,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f16,by=c("Tree","Ring","CO2.trt"))
    
    #### remove dead trees
    all$Active.FALSE.means.dead.[is.na(all$Active.FALSE.means.dead.)] <- "TRUE"
    all <- subset(all, Active.FALSE.means.dead.== TRUE)
    all <- all[complete.cases(all),]
    
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
    
    ### Calculate basal area based on diameter
    long$BA1 <- (long$Diameter / 2)^2 * pi
    long$BA2 <- (long$diam / 2)^2 * pi
    
    ### Summary by ring and treatment
    rDF <- summaryBy(BA1+BA2~Date+Ring, data=long, FUN=sum, keep.names=T, na.rm=T)
    rDF$BA1 <- rDF$BA1/ring_area
    rDF$BA2 <- rDF$BA2/ring_area
    
    ggplot(rDF, aes(Date, BA2, color=as.factor(Ring)))+
        geom_line()
    
    ### Assign treatment
    rDF$Trt[rDF$Ring%in%c(2,3,6)] <- "aCO2"
    rDF$Trt[rDF$Ring%in%c(1,4,5)] <- "eCO2"
    tDF <- summaryBy(BA1+BA2~Date+Trt, data=rDF, FUN=mean, keep.names=T, na.rm=T)
    tDF$BA1 <- tDF$BA1
    tDF$BA2 <- tDF$BA2
    
    ### Plot BA2
    ggplot(tDF, aes(Date, BA2, color=as.factor(Trt))) +
        geom_line()
    

    
}