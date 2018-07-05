make_understorey_aboveground_c_pool <- function(c_frac,
                                                return.decision="data",
                                                trt.effect="abs",
                                                stat.model="interaction") {
    
    ### currently only Varsha's harvest data on HIEv
    download_understorey_aboveground_biomass_data()
    
    ### read in the data 
    inDF1 <- read.csv(file.path(getToPath(), 
                               "FACE_P0061_RA_PATHARE_UNDERSTORY_ABOVEGROUND_BIOMASS_L2_20150201_20160730.csv"))
    
    ### read in Matthias's harvest data 
    inDF2 <- read.csv("temp_files/EucFACE_GrassStrip_Harvest_20170523.csv")
    
    ### process inDFs
    inDF1$Date <- paste0(as.character(inDF1$month), "-1")
    inDF1$Date <- as.Date(inDF1$Date, "%y-%b-%d")
    
    inDF1$live_g <- inDF1$grasses_live_g + inDF1$forbs_g
    
    tempDF1 <- data.frame(inDF1$ring, inDF1$Date, 
                          inDF1$live_g, inDF1$dead_g, inDF1$total_g)
    colnames(tempDF1) <- c("Ring", "Date", "Live_g", "Dead_g", "Total_g")
    
    
    
    tempDF2 <- data.frame(inDF2$Ring, "2017-05-01",
                          inDF2$LiveBiomassDW, inDF2$DeadBiomassDW,
                          inDF2$LiveBiomassDW + inDF2$DeadBiomassDW)
    colnames(tempDF2) <- c("Ring", "Date", "Live_g", "Dead_g", "Total_g")
    
    # combine data
    myDF <- rbind(tempDF1, tempDF2)
    myDF$total_g_c_m2 <- myDF$Total_g / strip_area * c_frac
    
    #- average across rings and dates
    liveDF <- summaryBy(Live_g~Date+Ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    deadDF <- summaryBy(Dead_g~Date+Ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    totDF <- summaryBy(Total_g~Date+Ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    # convert from g per 0.1 m-2 to g/m2, and make an assumption for C fraction
    outDF <- cbind(liveDF, deadDF$Dead_g, totDF$Total_g)
    names(outDF) <- c("Date", "Ring", "Live_g", "Dead_g", "Total_g")
    outDF$Live_g_C_m2 <- outDF$Live_g / strip_area * c_frac
    outDF$Dead_g_C_m2 <- outDF$Dead_g / strip_area * c_frac
    outDF$Total_g_C_m2 <- outDF$Live_g_C_m2 + outDF$Dead_g_C_m2
    out <- outDF[,c("Date", "Ring", "Live_g_C_m2", "Dead_g_C_m2", "Total_g_C_m2")]
    
    ### Compute statistical analyses
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
        
        s.stats <- treatment_effect_abs_statistics(inDF=myDF, 
                                                   var.cond="pool", var.col=6,
                                                   date.as.factor=T)
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
        
        s.stats <- treatment_effect_ratio_statistics(inDF=myDF, 
                                                     var.cond="pool", var.col=6,
                                                     date.as.factor=T)
    }
    
    ### Decision on what to return
    if (return.decision == "data") {
        return(out)
    } else if (return.decision == "stats") {
        return(s.stats)
    }
}


