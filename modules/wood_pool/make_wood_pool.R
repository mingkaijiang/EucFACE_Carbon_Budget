
# Function to calculate biomass (kg) corresponding to diameter (cm)
# Using relationship taken from Paul et al. (2013) Forest Ecol. Manag. 
allom_agb <- function(diamcm) {
    exp(-2.15 + 2.34*log(diamcm))
}

#- Make the live wood C pool
make_wood_pool <- function(ring_area, c_fraction, return_tree_level=FALSE,
                           return.decision="data",
                           trt.effect="abs",
                           stat.model="interaction"){
    
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
    classif$Active.FALSE.means.dead.[classif$Tree == 608] <- FALSE  # This tree dead too

    ########################
    
    # Merge the files
    all <- merge(classif,f12,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f13,by=c("Tree","Ring","CO2.trt")) 
    all <- merge(all,f14,by=c("Tree","Ring","CO2.trt"))  
    all <- merge(all,f15,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f16,by=c("Tree","Ring","CO2.trt"))
    
    # remove dead trees
    all$Active.FALSE.means.dead.[is.na(all$Active.FALSE.means.dead.)] <- "TRUE"
    all <- subset(all, Active.FALSE.means.dead.== TRUE)
    all <- all[complete.cases(all),]
    
    # remove "CORR" columns and dead column
    uncorr <- all[,-grep("CORR",names(all))]
    uncorr <- uncorr[,-grep("Coor",names(uncorr))]
    
    uncorr <- uncorr[,names(uncorr) != "Active.FALSE.means.dead."]
    
    # make a long-form version of dataframe
    long <- reshape(uncorr,idvar="Tree",varying=list(7:58),direction="long")
    dates <- names(uncorr)[7:58]
    long$Date <- c(rep(Sys.Date(),length(long$time)))  #wasn't sure how else to make this column date type
    for (i in (1:length(long$time))) {
        long$Date[i] <- as.Date(dates[long$time[i]],format="X%d.%m.%Y")
    }
    long <- renameCol(long,c("X17.02.2011"),c("diam"))
    
    long$diam <- as.numeric(long$diam)
    
    # add biomass to long-form dataframe
    long$biom <- allom_agb(long$diam)  # in kg DM
    
    # The bark removal affects the diameters mid-year. 
    # Hence, just calculate biomass once per year 
    # Specify dates here - may update this to March in future
    dates <- c(as.Date("2012-12-20"),as.Date("2013-12-20"),
               as.Date("2014-12-23"),as.Date("2015-12-14"),
               as.Date("2016-12-21"))
    data <- long[long$Date %in% dates,]
    
    data$biom_g <- data$biom * 1000

    #- sum across rings and dates
    data.m <- summaryBy(biom~Date+Ring,data=data,FUN=sum,keep.names=T,na.rm=T)
    
    # divide by ring area to get biomass per m2
    data.m$wood_pool <- data.m$biom / ring_area
    
    #- convert from kg DM m-2 to g C m-2
    data.m$wood_pool <- data.m$wood_pool * c_fraction * 1000
    
    #- format dataframe to return
    wood_pool <- data.m[,c("Date","Ring","wood_pool")]
    
    # Only use data period 2012-2016
    wood_pool <- wood_pool[wood_pool$Date<="2016-12-31",]
    
    ### Compute statistical analyses
    if (return.decision == "stats") {
        if (trt.effect == "abs") {
            if (stat.model == "dynamic") {
                source("R/stats/treatment_effect_abs_statistics_dynamic.R")
            } else if (stat.model == "no_interaction") {
                source("R/stats/treatment_effect_abs_statistics_no_interaction.R")
            } else if (stat.model == "interaction") {
                source("R/stats/treatment_effect_abs_statistics_interaction.R")
            } else {
                source("R/stats/treatment_effect_abs_statistics_no_random_effect.R")
            }
            
            s.stats <- treatment_effect_abs_statistics(inDF=data, 
                                                       var.cond="pool", var.col=11,
                                                       date.as.factor=F)
        } else if (trt.effect == "ratio") {
            if (stat.model == "dynamic") {
                source("R/stats/treatment_effect_ratio_statistics_dynamic.R")
            } else if (stat.model == "no_interaction") {
                source("R/stats/treatment_effect_ratio_statistics_no_interaction.R")
            } else if (stat.model == "interaction") {
                source("R/stats/treatment_effect_ratio_statistics_interaction.R")
            } else {
                source("R/stats/treatment_effect_ratio_statistics_no_random_effect.R")
            }
            
            s.stats <- treatment_effect_ratio_statistics(inDF=data, 
                                                         var.cond="pool", var.col=11,
                                                         date.as.factor=F)
        }
    }

    
    ### Decision on what to return
    if (return.decision == "data") {
        return(wood_pool)
    } else if (return.decision == "stats") {
        return(s.stats)
    }
}