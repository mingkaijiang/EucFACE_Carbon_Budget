generate_stats_abs_change_in_pools <- function(stat.model) {
    #### Call run.R program
    #source("run.R")
    
    #### Decision on what type of model to run
    #stat.model = "interaction"

    #### Call the stats function
    #### this compares eC/aC in terms of absolute difference.
    #### We can call dynamic model, meaning the model determines whether to consider
    #### interaction effect or not, or
    #### "no-interaction" just considers a simple model without look at interaction look
    #### "interaction" is a model with interaction considered.
    #### default model is a model without random effect, assuming no interaction. 
    if (stat.model == "dynamic") {
        source("R/stats/change_in_pool/treatment_effect_abs_statistics_dynamic.R")
    } else if (stat.model == "no_interaction") {
        source("R/stats/change_in_pool/treatment_effect_abs_statistics_no_interaction.R")
    } else if (stat.model == "interaction") {
        source("R/stats/change_in_pool/treatment_effect_abs_statistics_interaction.R")
    } else {
        source("R/stats/change_in_pool/treatment_effect_abs_statistics_no_random_effect.R")
    }
    
    ### Source change_in_pool function to automatically calculate changes in pool
    source("R/stats/change_in_pool/make_change_in_pool.R")
    
    
    ### Compute changes in pool variables
    delta_soil_c <- make_change_in_pool(mypool=soil_c_pool, var.col=3)
    delta_leaf_c <- make_change_in_pool(mypool=leaf_c_pool, var.col=3)
    delta_wood_c <- make_change_in_pool(mypool=wood_c_pool, var.col=3)
    delta_croot_c <- make_change_in_pool(mypool=coarse_root_c_pool_1, var.col=3)
    delta_froot_c <- make_change_in_pool(mypool=fineroot_c_pool, var.col=3)
    delta_ua_c <- make_change_in_pool(mypool=understorey_aboveground_c_pool, var.col=5)
    delta_ua_c2 <- make_change_in_pool(mypool=understorey_aboveground_c_pool_2, var.col=3)
    delta_ua_live_c <- make_change_in_pool(mypool=understorey_aboveground_c_pool, var.col=3)
    delta_ua_dead_c <- make_change_in_pool(mypool=understorey_aboveground_c_pool, var.col=4)
    delta_mic_c <- make_change_in_pool(mypool=microbial_c_pool, var.col=3)
    delta_myc_c <- make_change_in_pool(mypool=mycorrhizal_c_pool, var.col=3)
    delta_lit_c <- make_change_in_pool(mypool=leaflitter_pool, var.col=6)
    
    
    #### Work on each variable per time
    #### If date as factor, you should be looking at cumulative biomass (column 7)
    #### If date is not a factor, then use cumulative ndays to predict, hence column 8
    ### Soil C
    s.soilc <- treatment_effect_abs_statistics(inDF=delta_soil_c, 
                                               var.col=7, date.as.factor=T)
    
    ### Leaf C
    s.leafc <- treatment_effect_abs_statistics(inDF=delta_leaf_c, 
                                               var.col=7, date.as.factor=T)
    
    ### Wood C pool
    s.woodc <- treatment_effect_abs_statistics(inDF=delta_wood_c, 
                                               var.col=7, date.as.factor=T) 
    
    ### Fineroot C pool
    s.frc <- treatment_effect_abs_statistics(inDF=delta_froot_c, 
                                             var.col=7, date.as.factor=T)
    
    ### Coarseroot C pool
    s.crc <- treatment_effect_abs_statistics(inDF=delta_croot_c, 
                                             var.col=7, date.as.factor=T)
    
    ### Understorey aboveground C pool
    s.uac <- treatment_effect_abs_statistics(inDF=delta_ua_c, 
                                             var.col=7, date.as.factor=T)
    
    s.uac.live <- treatment_effect_abs_statistics(inDF=delta_ua_live_c, 
                                                  var.col=7, date.as.factor=T)
    
    s.uac.dead <- treatment_effect_abs_statistics(inDF=delta_ua_dead_c, 
                                                  var.col=7, date.as.factor=T)
    
    s.uac2 <- treatment_effect_abs_statistics(inDF=delta_ua_c2, 
                                              var.col=7, date.as.factor=T)
    
    ### Microbial C pool
    s.micc <- treatment_effect_abs_statistics(inDF=delta_mic_c, 
                                              var.col=7, date.as.factor=T)
    
    ### Mycorrhizal C pool
    s.mycc <- treatment_effect_abs_statistics(inDF=delta_myc_c, 
                                              var.col=7, date.as.factor=T)
    
    ### Standing dead C pool
    
    ### leaf litter C pool
    s.litc <- treatment_effect_abs_statistics(inDF=delta_lit_c, 
                                              var.col=7, date.as.factor=T)
    
    
    #### Create a output table to store all stats
    var.list <- c("delta_soil_c","delta_leaf_c","delta_wood_c","delta_fineroot_c",
                  "delta_coarseroot_c","delta_understorey_c","delta_understorey_c_2",
                  "delta_understorey_c_live","delta_understorey_c_dead",
                  "delta_microbial_c","delta_mycorrhizal_c", "delta_litter_c")
    out <- data.frame(var.list, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA,NA,NA,NA,NA,NA)
    colnames(out) <- c("Variable", "interactive_state",
                       "Trt_F", "Date_F", "Trt_Date_F", 
                       "Trt_Df", "Date_Df","Trt_Date_Df",
                       "Trt_Df.res", "Date_Df.res","Trt_Date_Df.res",
                       "Trt_Pr", "Date_Pr", "Trt_Date_Pr",
                       "effect_size", "conf_low", "conf_high")
    
    #### Create a function to ease life
    assign_stats <- function(s.var) {
        temp <- c()
        
        if(s.var$int.state == "non-interactive") {
            #### Assign values to out
            temp <- c(s.var$int.state,
                      s.var$anova$F[1],s.var$anova$F[2],NA,
                      s.var$anova$Df[1],s.var$anova$Df[2],NA,
                      s.var$anova$Df.res[1],s.var$anova$Df.res[2],NA,
                      s.var$anova$`Pr(>F)`[1],s.var$anova$`Pr(>F)`[2],NA,
                      s.var$eff,s.var$conf[1],s.var$conf[2])
            
        } else {#if (s.var$int.state == "interactive") {
            #### Assign values to out
            temp <- c(s.var$int.state,
                      s.var$anova$F[1],s.var$anova$F[2],s.var$anova$F[3],
                      s.var$anova$Df[1],s.var$anova$Df[2],s.var$anova$Df[3],
                      s.var$anova$Df.res[1],s.var$anova$Df.res[2],s.var$anova$Df.res[3],
                      s.var$anova$`Pr(>F)`[1],s.var$anova$`Pr(>F)`[2],s.var$anova$`Pr(>F)`[3],
                      s.var$eff,s.var$conf[1],s.var$conf[2])
        }
        return(temp)
    }
    
    #### Assign value to out
    out[out$Variable=="delta_soil_c",2:17] <- assign_stats(s.var=s.soilc)
    out[out$Variable=="delta_leaf_c",2:17] <- assign_stats(s.var=s.leafc)
    out[out$Variable=="delta_wood_c",2:17] <- assign_stats(s.var=s.woodc)
    out[out$Variable=="delta_fineroot_c",2:17] <- assign_stats(s.var=s.frc)
    out[out$Variable=="delta_coarseroot_c",2:17] <- assign_stats(s.var=s.crc)
    out[out$Variable=="delta_understorey_c",2:17] <- assign_stats(s.var=s.uac)
    out[out$Variable=="delta_understorey_c_2",2:17] <- assign_stats(s.var=s.uac2)
    out[out$Variable=="delta_understorey_c_live",2:17] <- assign_stats(s.var=s.uac.live)
    out[out$Variable=="delta_understorey_c_dead",2:17] <- assign_stats(s.var=s.uac.dead)
    out[out$Variable=="delta_microbial_c",2:17] <- assign_stats(s.var=s.micc)
    out[out$Variable=="delta_mycorrhizal_c",2:17] <- assign_stats(s.var=s.mycc)
    out[out$Variable=="delta_litter_c",2:17] <- assign_stats(s.var=s.litc)
    
    
    if (stat.model == "dynamic") {
        write.csv(out, "R_other/treatment_statistics_abs_change_in_pool_dynamic.csv", row.names=F)
    } else if (stat.model == "no_interaction") {
        write.csv(out, "R_other/treatment_statistics_abs_change_in_pool_no_interaction.csv", row.names=F)
    } else if (stat.model == "interaction") {
        write.csv(out, "R_other/treatment_statistics_abs_change_in_pool_interaction.csv", row.names=F)
    } else {
        write.csv(out, "R_other/treatment_statistics_abs_change_in_pool_no_random_effect.csv", row.names=F)
    }
    
}