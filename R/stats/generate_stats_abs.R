generate_stats_abs <- function(stat.model) {
    #### Call run.R program
    #source("run.R")
    
    #### Decision on what type of model to run
    #stat.model = "no_interaction"

    #### Call the stats function
    #### this compares eC/  in terms of absolute difference.
    #### We can call dynamic model, meaning the model determines whether to consider
    #### interaction effect or not, or
    #### "no-interaction" just considers a simple model without look at interaction look
    #### "interaction" is a model with interaction considered.
    #### default model is a model without random effect, assuming no interaction. 
    if (stat.model == "dynamic") {
        source("R/stats/treatment_effect_abs_statistics_dynamic.R")
    } else if (stat.model == "no_interaction") {
        source("R/stats/treatment_effect_abs_statistics_no_interaction.R")
    } else if (stat.model == "interaction") {
        source("R/stats/treatment_effect_abs_statistics_interaction.R")
    } else {
        source("R/stats/treatment_effect_abs_statistics_no_random_effect.R")
    }

    
    #### Work on each variable per time
    ### LAI
    s.lai <- treatment_effect_abs_statistics(inDF=lai_variable, 
                                         var.cond="pool", var.col=3,
                                         date.as.factor=T)
    
    ### SLA 
    s.sla <- treatment_effect_abs_statistics(inDF=sla_variable, 
                                         var.cond="pool", var.col=3,
                                         date.as.factor=T)
    
    ### Soil C
    s.soilc <- treatment_effect_abs_statistics(inDF=soil_c_pool, 
                                           var.cond="pool", var.col=3,
                                           date.as.factor=T)
    
    ### Leaf C
    s.leafc <- treatment_effect_abs_statistics(inDF=leaf_c_pool, 
                                           var.cond="pool", var.col=3,
                                           date.as.factor=T)
    
    ### Wood C pool
    s.woodc <- treatment_effect_abs_statistics(inDF=wood_c_pool, 
                                              var.cond="pool", var.col=3,
                                              date.as.factor=T) 
    
    ### Fineroot C pool
    s.frc <- treatment_effect_abs_statistics(inDF=fineroot_c_pool, 
                                         var.cond="pool", var.col=3,
                                         date.as.factor=T)
    
    ### Coarseroot C pool
    s.crc <- treatment_effect_abs_statistics(inDF=coarse_root_c_pool_1, 
                                            var.cond="pool", var.col=3,
                                            date.as.factor=T)
    
    ### Understorey aboveground C pool
    s.uac <- treatment_effect_abs_statistics(inDF=understorey_aboveground_c_pool, 
                                         var.cond="pool", var.col=5,
                                         date.as.factor=T)
    
    s.uac.live <- treatment_effect_abs_statistics(inDF=understorey_aboveground_c_pool, 
                                         var.cond="pool", var.col=3,
                                         date.as.factor=T)
    
    s.uac.dead <- treatment_effect_abs_statistics(inDF=understorey_aboveground_c_pool, 
                                         var.cond="pool", var.col=4,
                                         date.as.factor=T)
    
    s.uac2 <- treatment_effect_abs_statistics(inDF=understorey_aboveground_c_pool_2, 
                                         var.cond="pool", var.col=3,
                                         date.as.factor=T)
    
    ### Microbial C pool
    s.micc <- treatment_effect_abs_statistics(inDF=microbial_c_pool, 
                                          var.cond="pool", var.col=3,
                                          date.as.factor=T)
    
    ### Mycorrhizal C pool
    s.mycc <- treatment_effect_abs_statistics(inDF=mycorrhizal_c_pool, 
                                             var.cond="pool", var.col=3,
                                             date.as.factor=T)
    
    ### Standing dead C pool
    
    ### Leaf litter C pool
    s.litc <- treatment_effect_abs_statistics(inDF=leaflitter_pool, 
                                              var.cond="pool", var.col=6,
                                              date.as.factor=T)
    
    ### Insect pool
    s.insc <- treatment_effect_abs_statistics(inDF=insect_pool, 
                                              var.cond="pool", var.col=3,
                                              date.as.factor=T)
    
    ### Overstorey GPP
    
    
    ### Understorey GPP
    
    ### Overstorey Leaf respiration
    
    ### Root respiration
    s.rroot <- treatment_effect_abs_statistics(inDF=root_respiration_flux, 
                                              var.cond="flux", var.col=5,
                                              date.as.factor=T)
    
    ### Understorey respiration
    s.rund <- treatment_effect_abs_statistics(inDF=understorey_respiration_flux, 
                                          var.cond="flux", var.col=5,
                                          date.as.factor=T)
    
    ### Frass production
    s.fras <- treatment_effect_abs_statistics(inDF=frass_production_flux, 
                                             var.cond="flux", var.col=5,
                                             date.as.factor=T)
    
    ### herbivory leaf consumption flux
    s.hb.cons <- treatment_effect_abs_statistics(inDF=herbivory_leaf_consumption_flux, 
                                             var.cond="flux", var.col=5,
                                             date.as.factor=T)
    
    ### Herbivory respiration
    s.rhb <- treatment_effect_abs_statistics(inDF=herbivory_respiration_flux, 
                                         var.cond="flux", var.col=5,
                                         date.as.factor=T)
    
    ### Lerp production
    s.lerp.prod <- treatment_effect_abs_statistics(inDF=lerp_production_flux, 
                                               var.cond="flux", var.col=5,
                                               date.as.factor=T)
    
    ### soil respiration
    s.rsoil <- treatment_effect_abs_statistics(inDF=soil_respiration_flux, 
                                              var.cond="flux", var.col=5,
                                              date.as.factor=T)
    
    ### DOC leaching
    s.doc <- treatment_effect_abs_statistics(inDF=doc_leaching_flux, 
                                         var.cond="flux", var.col=5,
                                         date.as.factor=T)
    
    ### CH4 uptake - un-gap filled data
    s.ch4 <- treatment_effect_abs_statistics(inDF=methane_c_flux, 
                                         var.cond="flux", var.col=3,
                                         date.as.factor=T)
    
    ### Leaflitter flux
    s.lit.leaf <- treatment_effect_abs_statistics(inDF=leaflitter_flux, 
                                                  var.cond="flux", var.col=6,
                                                  date.as.factor=T)  
    
    ### twig litter flux
    s.lit.twig <- treatment_effect_abs_statistics(inDF=leaflitter_flux, 
                                             var.cond="flux", var.col=3,
                                             date.as.factor=T)
    
    ### bark litter flux
    s.lit.bark <- treatment_effect_abs_statistics(inDF=leaflitter_flux, 
                                              var.cond="flux", var.col=4,
                                              date.as.factor=T) 
    
    ### Seed litter flux
    s.lit.seed <- treatment_effect_abs_statistics(inDF=leaflitter_flux, 
                                              var.cond="flux", var.col=5,
                                              date.as.factor=T)
    
    ### Wood production flux
    s.wood.prod <- treatment_effect_abs_statistics(inDF=wood_production_flux, 
                                               var.cond="flux", var.col=5,
                                               date.as.factor=T) 
    
    ### Fineroot production flux
    s.froot.prod <- treatment_effect_abs_statistics(inDF=fineroot_production_flux, 
                                                var.cond="flux", var.col=5,
                                                date.as.factor=T) 
    
    ### Coarseroot production
    s.croot.prod <- treatment_effect_abs_statistics(inDF=coarse_root_production_flux_1, 
                                                var.cond="flux", var.col=5,
                                                date.as.factor=T)
    
    ### Understorey aboveground production
    s.und.prod <- treatment_effect_abs_statistics(inDF=understorey_aboveground_production_flux, 
                                              var.cond="flux", var.col=5,
                                              date.as.factor=T)
    
    ### Rh respiration
    s.rh <- treatment_effect_abs_statistics(inDF=heterotrophic_respiration_flux, 
                                        var.cond="flux", var.col=5,
                                        date.as.factor=T) 
    
    #### Create a output table to store all stats
    var.list <- c("lai", "sla","soil_c","leaf_c","wood_c","fineroot_c",
                  "coarseroot_c","understorey_c","understorey_c_2","understorey_c_live","understorey_c_dead",
                  "microbial_c","mycorrhizal_c","litter_c","insect_c",
                  "root_respiration","understorey_respiration",
                  "frass_prod","herb_consump","herb_respiration","lerp_prod",
                  "soil_respiration","doc","ch4","leaf_prod","twig_prod",
                  "bark_prod","seed_prod","wood_prod","fineroot_prod",
                  "coarseroot_prod","understorey_prod","hetero_respiration",
                  "over_gpp","over_leaf_respiration","wood_respiration",
                  "understorey_gpp")
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
    out[out$Variable=="lai",2:17] <- assign_stats(s.var=s.lai)
    out[out$Variable=="sla",2:17] <- assign_stats(s.var=s.sla)
    out[out$Variable=="soil_c",2:17] <- assign_stats(s.var=s.soilc)
    out[out$Variable=="leaf_c",2:17] <- assign_stats(s.var=s.leafc)
    out[out$Variable=="wood_c",2:17] <- assign_stats(s.var=s.woodc)
    out[out$Variable=="fineroot_c",2:17] <- assign_stats(s.var=s.frc)
    out[out$Variable=="coarseroot_c",2:17] <- assign_stats(s.var=s.crc)
    out[out$Variable=="understorey_c",2:17] <- assign_stats(s.var=s.uac)
    out[out$Variable=="understorey_c_2",2:17] <- assign_stats(s.var=s.uac2)
    out[out$Variable=="understorey_c_live",2:17] <- assign_stats(s.var=s.uac.live)
    out[out$Variable=="understorey_c_dead",2:17] <- assign_stats(s.var=s.uac.dead)
    out[out$Variable=="microbial_c",2:17] <- assign_stats(s.var=s.micc)
    out[out$Variable=="mycorrhizal_c",2:17] <- assign_stats(s.var=s.mycc)
    out[out$Variable=="litter_c",2:17] <- assign_stats(s.var=s.litc)
    out[out$Variable=="insect_c",2:17] <- assign_stats(s.var=s.insc)
    out[out$Variable=="root_respiration",2:17] <- assign_stats(s.var=s.rroot)
    out[out$Variable=="understorey_respiration",2:17] <- assign_stats(s.var=s.rund)
    out[out$Variable=="frass_prod",2:17] <- assign_stats(s.var=s.fras)
    out[out$Variable=="herb_consump",2:17] <- assign_stats(s.var=s.hb.cons)
    out[out$Variable=="herb_respiration",2:17] <- assign_stats(s.var=s.rhb)
    out[out$Variable=="lerp_prod",2:17] <- assign_stats(s.var=s.lerp.prod)
    out[out$Variable=="soil_respiration",2:17] <- assign_stats(s.var=s.rsoil)
    out[out$Variable=="doc",2:17] <- assign_stats(s.var=s.doc)
    out[out$Variable=="ch4",2:17] <- assign_stats(s.var=s.ch4)
    out[out$Variable=="leaf_prod",2:17] <- assign_stats(s.var=s.lit.leaf)
    out[out$Variable=="twig_prod",2:17] <- assign_stats(s.var=s.lit.twig)
    out[out$Variable=="bark_prod",2:17] <- assign_stats(s.var=s.lit.bark)
    out[out$Variable=="seed_prod",2:17] <- assign_stats(s.var=s.lit.seed)
    out[out$Variable=="wood_prod",2:17] <- assign_stats(s.var=s.wood.prod)
    out[out$Variable=="fineroot_prod",2:17] <- assign_stats(s.var=s.froot.prod)
    out[out$Variable=="coarseroot_prod",2:17] <- assign_stats(s.var=s.croot.prod)
    out[out$Variable=="understorey_prod",2:17] <- assign_stats(s.var=s.und.prod)
    out[out$Variable=="hetero_respiration",2:17] <- assign_stats(s.var=s.rh)

    
    if (stat.model == "dynamic") {
        write.csv(out, "R_other/treatment_statistics_abs_dynamic.csv", row.names=F)
    } else if (stat.model == "no_interaction") {
        write.csv(out, "R_other/treatment_statistics_abs_no_interaction.csv", row.names=F)
    } else if (stat.model == "interaction") {
        write.csv(out, "R_other/treatment_statistics_abs_interaction.csv", row.names=F)
    } else {
        write.csv(out, "R_other/treatment_statistics_abs_no_random_effect.csv", row.names=F)
    }
    
}