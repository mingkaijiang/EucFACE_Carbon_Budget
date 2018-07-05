make_lai_variable <- function(return.decision="data",
                              trt.effect="abs",
                              stat.model="interaction"){

  res <- download_lai_variable()
  
  res <- subset(res, select=c(Date, Ring, LAI))
  names(res)[3] <- "lai_variable"
  
  #- return a number for ring
  res$Ring <- as.numeric(res$Ring)
  
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
          
          s.stats <- treatment_effect_abs_statistics(inDF=res, 
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
          
          s.stats <- treatment_effect_ratio_statistics(inDF=res, 
                                                       var.cond="pool", var.col=3,
                                                       date.as.factor=T)
      }
  }

  
  ### Decision on what to return
  if (return.decision == "data") {
      return(res)
  } else if (return.decision == "stats") {
      return(s.stats)
  }
}

