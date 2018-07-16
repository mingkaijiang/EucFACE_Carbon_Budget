#- Make the fine root C pool
make_fineroot_pool <- function(c_frac,
                               return.decision="data",
                               trt.effect="abs",
                               stat.model="interaction"){
  
  #- download the data
  download_fineroot_data()
  
  #- read in the csv
  frb1 <- read.csv(file.path(getToPath(), 
                             "FACE_P0083_RA_FR-BIOMASS_L1_20140201-20150915.csv"))
  frb1$Date <- as.Date(frb1$Date)
  names(frb1)[2] <- "Ring"
  names(frb1)[8] <- "frb_tot"
  
  frb1$fineroot_pool <- frb1$frb_tot*c_frac
  
  #- average across rings and dates
  frb.m <- summaryBy(frb_tot~Date+Ring,data=frb1,FUN=mean,keep.names=T)
  
  #- convert to g C m-2. Use fine-root specific c_fraction from Juan.
  frb.m$fineroot_pool <- frb.m$frb_tot*c_frac
  
  #- format dataframe to return
  frb.out <- frb.m[,c("Date","Ring","fineroot_pool")]
  
  # Only use data period 2012-2016
  frb.out <- frb.out[frb.out$Date<="2016-12-31",]
  
  
  ### Compute statistical analyses
  if (return.decision=="stats") {
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
          
          s.stats <- treatment_effect_abs_statistics(inDF=frb1, 
                                                     var.cond="pool", var.col=8,
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
          
          s.stats <- treatment_effect_ratio_statistics(inDF=frb1, 
                                                       var.cond="pool", var.col=8,
                                                       date.as.factor=T)
      }
  }

  
  ### Decision on what to return
  if (return.decision == "data") {
      return(frb.out)
  } else if (return.decision == "stats") {
      return(s.stats)
  }
}