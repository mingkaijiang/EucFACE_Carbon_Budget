
make_sla_variable <- function(return.decision="data",
                              trt.effect="abs",
                              stat.model="interaction") {
    
    #### three decisions to make:
    #### 1. return.decision: data - return the dataframe, stats - return the stats
    #### 2. If return stats, then need to choose what effect to show (trt.effect) i.e. ratio or abs
    #### 3. Then need to decide which model (stat.model): interaction, no_interaction, dynamic, and no_random_effect
    
    ### Generate ring-specific SLA data per date
    download_lma_data()
    
    inDF1 <- read.csv(file.path(getToPath(), "FACE_P0020_RA_LMA_L2_20130213-20131115.csv"), stringsAsFactors=F)
    inDF2 <- read.csv(file.path(getToPath(), "FACE_P0020_RA_LMA_20140130-20141016_L2.csv"), stringsAsFactors=F)
    inDF3 <- read.csv(file.path(getToPath(), "FACE_P0020_RA_LMA_20150129-20150416_L2.csv"), stringsAsFactors=F)
    inDF4 <- read.csv(file.path(getToPath(), "FACE_P0020_RA_LMA_20160201-20161018_L2.csv"), stringsAsFactors=F)
    
    lma_raw <- rbind(inDF1, inDF2, inDF3, inDF4)
    lma_raw$Date <- as.Date(lma_raw$Date, format="%d/%m/%Y")
    lma <- droplevels(subset(lma_raw, TREE != "outs R6"))  # outside ring trees
    lma <- mutate(lma, 
                  Ring = as.numeric(substr(TREE,1,1)),
                  LMA = as.numeric(LMA),
                  SLA = 10000 / LMA)  # cm2 g-1
    lma <- subset(lma, !is.na(Ring), select =c(Date,Ring, SLA))  # missing ring is for tree 'outs R6' - ignore
 
    lma_a <- summaryBy(SLA ~ Ring + Date, FUN=mean, na.rm=TRUE, data=lma, keep.names=TRUE)
    
    lma.out <- dplyr::rename(lma_a, sla_variable = SLA)
    
    
    ### Compute statistical analyses
    if(return.decision=="stats") {
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
            
            s.sla <- treatment_effect_abs_statistics(inDF=lma, 
                                                     var.cond="pool", var.col=3,
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
            
            s.sla <- treatment_effect_ratio_statistics(inDF=lma, 
                                                       var.cond="pool", var.col=3,
                                                       date.as.factor=T)
        }
    }


    ### Decision on what to return
    if (return.decision == "data") {
        return(lma.out)
    } else if (return.decision == "stats") {
        return(s.sla)
    }
    

}
