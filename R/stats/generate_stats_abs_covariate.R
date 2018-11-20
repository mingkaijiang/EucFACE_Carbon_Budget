generate_stats_abs_covariate <- function(stat.model) {
    
    
    stat.model <- "no_interaction_with_covariate"
    ### remove pre-treatment data period for individual variables (e.g. leaf, wood, soil, mic, myc)
    
    #### Work on each variable per time
    ### Soil C
    s.soilc <- make_soilc_treatment_abs_effect_statistics(inDF=soil_c_pool, 
                                                          var.cond="pool", var.col=3,
                                                          date.as.factor=T,
                                                          stat.model=stat.model,
                                                          return.outcome="model")
    
    ### Leaf C
    s.leafc <- make_leafc_treatment_abs_effect_statistics(inDF=leaf_c_pool, 
                                                          var.cond="pool", var.col=3,
                                                          date.as.factor=T,
                                                          stat.model=stat.model,
                                                          return.outcome="model")
    
    ### Wood C pool
    s.woodc <- make_woodc_treatment_abs_effect_statistics(inDF=wood_c_pool, 
                                                          var.cond="pool", var.col=3,
                                                          date.as.factor=T,
                                                          stat.model=stat.model,
                                                          return.outcome="model") 

    ### Fineroot C pool
    s.frc <- make_frootc_treatment_abs_effect_statistics(inDF=fineroot_c_pool, 
                                                         var.cond="pool", var.col=3,
                                                         date.as.factor=T,
                                                         stat.model=stat.model,
                                                         return.outcome="model")

    ### Coarseroot C pool
    s.crc <- make_crootc_treatment_abs_effect_statistics(inDF=coarse_root_c_pool_1, 
                                            var.cond="pool", var.col=3,
                                            date.as.factor=T,
                                            stat.model=stat.model,
                                            return.outcome="model")

    ### Understorey aboveground C pool
    s.uac <- make_uac_treatment_abs_effect_statistics(inDF=understorey_aboveground_c_pool, 
                                                      var.cond="pool", var.col=5,
                                                      date.as.factor=T,
                                                      stat.model=stat.model,
                                                      return.outcome="model")
 
    s.uac2 <- make_uac_treatment_abs_effect_statistics(inDF=understorey_aboveground_c_pool_2, 
                                                        var.cond="pool", var.col=3,
                                                        date.as.factor=T,
                                                       stat.model=stat.model,
                                                       return.outcome="model")

    ### Microbial C pool
    s.micc <- make_micc_treatment_abs_effect_statistics(inDF=microbial_c_pool, 
                                                        var.cond="pool", var.col=3,
                                                        date.as.factor=T,
                                                        stat.model=stat.model,
                                                        return.outcome="model")

    ### Mycorrhizal C pool
    s.mycc <- make_mycc_treatment_abs_effect_statistics(inDF=mycorrhizal_c_pool, 
                                                        var.cond="pool", var.col=3,
                                                        date.as.factor=T,
                                                        stat.model=stat.model,
                                                        return.outcome="model")

    ### Leaf litter C pool
    s.litc <- make_litc_treatment_abs_effect_statistics(inDF=leaflitter_pool, 
                                                        var.cond="pool", var.col=6,
                                                        date.as.factor=T,
                                                        stat.model=stat.model,
                                                        return.outcome="model")

    ### Insect pool
    s.insc <- make_insc_treatment_abs_effect_statistics(inDF=insect_pool, 
                                                            var.cond="pool", var.col=3,
                                                            date.as.factor=T,
                                                        stat.model=stat.model,
                                                        return.outcome="model")
 
    ### Overstorey GPP
    s.o.gpp <- make_overstorey_gpp_treatment_abs_effect_statistics(inDF=overstorey_gpp_flux, 
                                                                   var.cond="ann.flux", var.col=3,
                                                                   date.as.factor=T,
                                                                   stat.model=stat.model,
                                                                   return.outcome="model")
    
    ### Understorey GPP
    s.u.gpp <- make_understorey_gpp_treatment_abs_effect_statistics(inDF=understorey_gpp_flux, 
                                                                    var.cond="ann.flux", var.col=3,
                                                                    date.as.factor=T,
                                                                    stat.model=stat.model,
                                                                    return.outcome="model")

    ### Overstorey Leaf respiration
    s.rleaf <- make_overstorey_ra_leaf_treatment_abs_effect_statistics(inDF=overstorey_leaf_respiration_flux, 
                                                                      var.cond="ann.flux", var.col=3,
                                                                      date.as.factor=T,
                                                                      stat.model=stat.model,
                                                                      return.outcome="model")

    ### Wood respiration
    s.rwood <- make_ra_wood_treatment_abs_effect_statistics(inDF=wood_respiration_flux, 
                                               var.cond="flux", var.col=5,
                                               date.as.factor=T,
                                               stat.model=stat.model,
                                               return.outcome="model")
    
    ### Root respiration
    s.rroot <- make_ra_root_treatment_abs_effect_statistics(inDF=root_respiration_flux, 
                                                            var.cond="flux", var.col=5,
                                                            date.as.factor=T,
                                                            stat.model=stat.model,
                                                            return.outcome="model")
    ### Understorey respiration
    s.rund <- make_ra_und_treatment_abs_effect_statistics(inDF=understorey_respiration_flux, 
                                                          var.cond="flux", var.col=5,
                                                          date.as.factor=T,
                                                          stat.model=stat.model,
                                                          return.outcome="model")
    
    ### Frass production
    s.fras <- make_frass_treatment_abs_effect_statistics(inDF=frass_production_flux, 
                                                         var.cond="flux", var.col=5,
                                                         date.as.factor=T,
                                                         stat.model=stat.model,
                                                         return.outcome="model")
  
    ### herbivory leaf consumption flux
    s.hb.cons <- make_hb_cons_treatment_abs_effect_statistics(inDF=herbivory_leaf_consumption_flux, 
                                                              var.cond="flux", var.col=5,
                                                              date.as.factor=T,
                                                              stat.model=stat.model,
                                                              return.outcome="model")
 
    ### Herbivory respiration
    s.rhb <- make_r_hb_treatment_abs_effect_statistics(inDF=herbivory_respiration_flux, 
                                                       var.cond="flux", var.col=5,
                                                       date.as.factor=T,
                                                       stat.model=stat.model,
                                                       return.outcome="model")
  
    ### Lerp production
    s.lerp.prod <- make_lp_treatment_abs_effect_statistics(inDF=lerp_production_flux, 
                                                           var.cond="flux", var.col=5,
                                                           date.as.factor=T,
                                                           stat.model=stat.model,
                                                           return.outcome="model")
   
    ### soil respiration
    s.rsoil <- make_rsoil_treatment_abs_effect_statistics(inDF=soil_respiration_flux, 
                                                          var.cond="flux", var.col=5,
                                                          date.as.factor=T,
                                                          stat.model=stat.model,
                                                          return.outcome="model")
 
    
    #s.myc.prod <- make_myc_production_treatment_abs_effect_statistics(inDF=mycorrhizal_c_production_flux, 
    #                                                      var.cond="flux", var.col=5,
    #                                                      date.as.factor=T,
    #                                                      stat.model=stat.model,
    #                                                      return.outcome="model")
    
    ### DOC leaching
    s.doc <- make_doc_treatment_abs_effect_statistics(inDF=doc_leaching_flux, 
                                                      var.cond="flux", var.col=5,
                                                      date.as.factor=T,
                                                      stat.model=stat.model,
                                                      return.outcome="model")
   
    ### CH4 uptake - un-gap filled data
    s.ch4 <- make_ch4_treatment_abs_effect_statistics(inDF=methane_c_flux, 
                                                      var.cond="flux", var.col=3,
                                                      date.as.factor=T,
                                                      stat.model=stat.model,
                                                      return.outcome="model")
    
    s.voc <- make_voc_treatment_abs_effect_statistics(inDF=voc_emission_flux, 
                                                                   var.cond="ann.flux", var.col=3,
                                                                   date.as.factor=T,
                                                                   stat.model=stat.model,
                                                                   return.outcome="model")
  
    ### Leaflitter flux
    s.lit.leaf <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                   var.cond="flux", var.col=6,
                                                                   date.as.factor=T,
                                                                   stat.model=stat.model,
                                                                   return.outcome="model")  

    ### twig litter flux
    s.lit.twig <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                   var.cond="flux", var.col=3,
                                                                   date.as.factor=T,
                                                                   stat.model=stat.model,
                                                                   return.outcome="model")
  
    ### bark litter flux
    s.lit.bark <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                   var.cond="flux", var.col=4,
                                                                   date.as.factor=T,
                                                                   stat.model=stat.model,
                                                                   return.outcome="model") 
   
    ### Seed litter flux
    s.lit.seed <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                   var.cond="flux", var.col=5,
                                                                   date.as.factor=T,
                                                                   stat.model=stat.model,
                                                                   return.outcome="model")
    
    ### Wood production flux
    s.wood.prod <- make_wood_prod_treatment_abs_effect_statistics(inDF=wood_production_flux, 
                                               var.cond="flux", var.col=5,
                                               date.as.factor=T,
                                               stat.model=stat.model,
                                               return.outcome="model") 
  
    ### Fineroot production flux
    s.froot.prod <- make_froot_prod_treatment_abs_effect_statistics(inDF=fineroot_production_flux, 
                                                                    var.cond="flux", var.col=5,
                                                                    date.as.factor=T,
                                                                    stat.model=stat.model,
                                                                    return.outcome="model") 

    ### Coarseroot production
    s.croot.prod <- make_croot_prod_treatment_abs_effect_statistics(inDF=coarse_root_production_flux_1, 
                                                                    var.cond="flux", var.col=5,
                                                                    date.as.factor=T,
                                                                    stat.model=stat.model,
                                                                    return.outcome="model")
   
    ### Understorey aboveground production
    s.und.prod <- make_und_prod_treatment_abs_effect_statistics(inDF=understorey_aboveground_production_flux, 
                                                                var.cond="flux", var.col=5,
                                                                date.as.factor=T,
                                                                stat.model=stat.model,
                                                                return.outcome="model")
    
    ### Understory litter flux
    s.und.lit <- make_und_prod_treatment_abs_effect_statistics(inDF=understorey_aboveground_production_flux, 
                                                                var.cond="flux", var.col=6,
                                                                date.as.factor=T,
                                                               stat.model=stat.model,
                                                               return.outcome="model")

    ### Rh respiration
    s.rh <- make_rh_treatment_abs_effect_statistics(inDF=heterotrophic_respiration_flux, 
                                                    var.cond="flux", var.col=5,
                                                    date.as.factor=T,
                                                    stat.model=stat.model,
                                                    return.outcome="model")

    ### Delta Soil C
    s.delta.soilc <- make_delta_soilc_treatment_abs_effect_statistics(inDF=soil_c_pool, 
                                                          var.cond="pool", var.col=3,
                                                          date.as.factor=T,
                                                          stat.model=stat.model,
                                                          return.outcome="model")
    
    ### Delta Leaf C
    s.delta.leafc <- make_delta_leafc_treatment_abs_effect_statistics(inDF=leaf_c_pool, 
                                                          var.cond="pool", var.col=3,
                                                          date.as.factor=T,
                                                          stat.model=stat.model,
                                                          return.outcome="model")
    
    ### Delta Wood C pool
    s.delta.woodc <- make_delta_woodc_treatment_abs_effect_statistics(inDF=wood_c_pool, 
                                                          var.cond="pool", var.col=3,
                                                          date.as.factor=T,
                                                          stat.model=stat.model,
                                                          return.outcome="model") 
    
    ### Delta Fineroot C pool
    s.delta.frc <- make_delta_frootc_treatment_abs_effect_statistics(inDF=fineroot_c_pool, 
                                                         var.cond="pool", var.col=3,
                                                         date.as.factor=T,
                                                         stat.model=stat.model,
                                                         return.outcome="model")
    
    ### Delta Coarseroot C pool
    s.delta.crc <- make_delta_crootc_treatment_abs_effect_statistics(inDF=coarse_root_c_pool_1, 
                                                         var.cond="pool", var.col=3,
                                                         date.as.factor=T,
                                                         stat.model=stat.model,
                                                         return.outcome="model")
    
    ### Delta Understorey aboveground C pool
    s.delta.uac <- make_delta_uac_treatment_abs_effect_statistics(inDF=understorey_aboveground_c_pool, 
                                                      var.cond="pool", var.col=5,
                                                      date.as.factor=T,
                                                      stat.model=stat.model,
                                                      return.outcome="model")
    
    s.delta.uac2 <- make_delta_uac_treatment_abs_effect_statistics(inDF=understorey_aboveground_c_pool_2, 
                                                       var.cond="pool", var.col=3,
                                                       date.as.factor=T,
                                                       stat.model=stat.model,
                                                       return.outcome="model")
    
    ### Delta Microbial C pool
    s.delta.micc <- make_delta_micc_treatment_abs_effect_statistics(inDF=microbial_c_pool, 
                                                        var.cond="pool", var.col=3,
                                                        date.as.factor=T,
                                                        stat.model=stat.model,
                                                        return.outcome="model")
    
    ### Delta Mycorrhizal C pool
    s.delta.mycc <- make_delta_mycc_treatment_abs_effect_statistics(inDF=mycorrhizal_c_pool, 
                                                        var.cond="pool", var.col=3,
                                                        date.as.factor=T,
                                                        stat.model=stat.model,
                                                        return.outcome="model")
    
    
    ### Delta Leaf litter C pool
    s.delta.litc <- make_delta_litc_treatment_abs_effect_statistics(inDF=leaflitter_pool, 
                                                        var.cond="pool", var.col=6,
                                                        date.as.factor=T,
                                                        stat.model=stat.model,
                                                        return.outcome="model")
    
    ### Delta Insect pool
    s.delta.insc <- make_delta_insc_treatment_abs_effect_statistics(inDF=insect_pool, 
                                                        var.cond="pool", var.col=3,
                                                        date.as.factor=T,
                                                        stat.model=stat.model,
                                                        return.outcome="model")
    
    #### Create a output table to store all stats
    var.list <- c("soil_c","leaf_c","wood_c","fineroot_c",
                  "coarseroot_c","understorey_c","understorey_c_2",
                  "microbial_c","mycorrhizal_c","litter_c","insect_c",
                  "root_respiration","understorey_respiration",
                  "frass_prod","herb_consump","herb_respiration","lerp_prod",
                  "soil_respiration","doc","ch4","voc","leaf_prod","twig_prod",
                  "bark_prod","seed_prod","wood_prod","fineroot_prod",
                  "coarseroot_prod","understorey_prod","hetero_respiration","understorey_lit",
                  "over_gpp","over_leaf_respiration","wood_respiration",#"mycorrhizal_prod",
                  "understorey_gpp","delta_soil_c","delta_leaf_c","delta_wood_c","delta_fineroot_c",
                  "delta_coarseroot_c","delta_understorey_c","delta_understorey_c_2",
                  "delta_microbial_c","delta_mycorrhizal_c","delta_litter_c","delta_insect_c")
    out <- data.frame(var.list, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                      NA,NA,NA,NA,NA,NA)
    colnames(out) <- c("Variable", "interactive_state",
                       "Trt_F", "Date_F", "Cov_F", 
                       "Trt_Df", "Date_Df","Cov_Df",
                       "Trt_Df.res", "Date_Df.res","Cov_Df.res",
                       "Trt_Pr", "Date_Pr", "Cov_Pr",
                       "effect_size", "conf_low", "conf_high")
    
    #### Create a function to ease life
    assign_stats <- function(s.var) {
        temp <- c()
        
        #### Assign values to out
        temp <- c(s.var$int.state,
                  s.var$anova$F[1],s.var$anova$F[2],s.var$anova$F[3],
                  s.var$anova$Df[1],s.var$anova$Df[2],s.var$anova$Df[3],
                  s.var$anova$Df.res[1],s.var$anova$Df.res[2],s.var$anova$Df.res[3],
                  s.var$anova$`Pr(>F)`[1],s.var$anova$`Pr(>F)`[2],s.var$anova$`Pr(>F)`[3],
                  s.var$eff,s.var$conf[1],s.var$conf[2])
        
        
        return(temp)
    }
    
    #### Assign value to out
    out[out$Variable=="soil_c",2:17] <- assign_stats(s.var=s.soilc)
    out[out$Variable=="leaf_c",2:17] <- assign_stats(s.var=s.leafc)
    out[out$Variable=="wood_c",2:17] <- assign_stats(s.var=s.woodc)
    out[out$Variable=="fineroot_c",2:17] <- assign_stats(s.var=s.frc)
    out[out$Variable=="coarseroot_c",2:17] <- assign_stats(s.var=s.crc)
    out[out$Variable=="understorey_c",2:17] <- assign_stats(s.var=s.uac)
    out[out$Variable=="understorey_c_2",2:17] <- assign_stats(s.var=s.uac2)
    out[out$Variable=="microbial_c",2:17] <- assign_stats(s.var=s.micc)
    out[out$Variable=="mycorrhizal_c",2:17] <- assign_stats(s.var=s.mycc)
    out[out$Variable=="litter_c",2:17] <- assign_stats(s.var=s.litc)
    out[out$Variable=="insect_c",2:17] <- assign_stats(s.var=s.insc)
    out[out$Variable=="wood_respiration",2:17] <- assign_stats(s.var=s.rwood)
    out[out$Variable=="root_respiration",2:17] <- assign_stats(s.var=s.rroot)
    out[out$Variable=="understorey_respiration",2:17] <- assign_stats(s.var=s.rund)
    out[out$Variable=="frass_prod",2:17] <- assign_stats(s.var=s.fras)
    out[out$Variable=="herb_consump",2:17] <- assign_stats(s.var=s.hb.cons)
    out[out$Variable=="herb_respiration",2:17] <- assign_stats(s.var=s.rhb)
    out[out$Variable=="lerp_prod",2:17] <- assign_stats(s.var=s.lerp.prod)
    out[out$Variable=="soil_respiration",2:17] <- assign_stats(s.var=s.rsoil)
    out[out$Variable=="doc",2:17] <- assign_stats(s.var=s.doc)
    out[out$Variable=="ch4",2:17] <- assign_stats(s.var=s.ch4)
    out[out$Variable=="voc",2:17] <- assign_stats(s.var=s.voc)
    out[out$Variable=="leaf_prod",2:17] <- assign_stats(s.var=s.lit.leaf)
    out[out$Variable=="twig_prod",2:17] <- assign_stats(s.var=s.lit.twig)
    out[out$Variable=="bark_prod",2:17] <- assign_stats(s.var=s.lit.bark)
    out[out$Variable=="seed_prod",2:17] <- assign_stats(s.var=s.lit.seed)
    out[out$Variable=="wood_prod",2:17] <- assign_stats(s.var=s.wood.prod)
    out[out$Variable=="fineroot_prod",2:17] <- assign_stats(s.var=s.froot.prod)
    out[out$Variable=="coarseroot_prod",2:17] <- assign_stats(s.var=s.croot.prod)
    out[out$Variable=="understorey_prod",2:17] <- assign_stats(s.var=s.und.prod)
    out[out$Variable=="hetero_respiration",2:17] <- assign_stats(s.var=s.rh)
    out[out$Variable=="understorey_lit",2:17] <- assign_stats(s.var=s.und.lit)
    
    out[out$Variable=="over_gpp",2:17] <- assign_stats(s.var=s.o.gpp)
    out[out$Variable=="over_leaf_respiration",2:17] <- assign_stats(s.var=s.rleaf)
    #out[out$Variable=="mycorrhizal_prod",2:17] <- c(s.myc.prod$int.state,
    #                                                NA,NA,NA,
    #                                                NA,NA,NA,
    #                                                NA,NA,NA,
    #                                                NA,NA,NA,
    #                                                s.myc.prod$eff,s.myc.prod$conf[1],s.myc.prod$conf[2])
    out[out$Variable=="understorey_gpp",2:17] <- assign_stats(s.var=s.u.gpp)
    out[out$Variable=="delta_soil_c",2:17] <- assign_stats(s.var=s.delta.soilc)
    out[out$Variable=="delta_leaf_c",2:17] <- assign_stats(s.var=s.delta.leafc)
    out[out$Variable=="delta_wood_c",2:17] <- assign_stats(s.var=s.delta.woodc)
    out[out$Variable=="delta_fineroot_c",2:17] <- assign_stats(s.var=s.delta.frc)
    out[out$Variable=="delta_coarseroot_c",2:17] <- assign_stats(s.var=s.delta.crc)
    out[out$Variable=="delta_understorey_c",2:17] <- assign_stats(s.var=s.delta.uac)
    out[out$Variable=="delta_understorey_c_2",2:17] <- assign_stats(s.var=s.delta.uac2)
    out[out$Variable=="delta_microbial_c",2:17] <- assign_stats(s.var=s.delta.micc)
    out[out$Variable=="delta_mycorrhizal_c",2:17] <- assign_stats(s.var=s.delta.mycc)
    out[out$Variable=="delta_litter_c",2:17] <- assign_stats(s.var=s.delta.litc)
    out[out$Variable=="delta_insect_c",2:17] <- assign_stats(s.var=s.delta.insc)

    stat.model <- "no_interaction_with_covariate"
    
    if (stat.model == "no_interaction_with_covariate") {
        write.csv(out, "R_other/treatment_statistics_abs_no_interaction_with_covariate.csv", row.names=F)
    } else if (stat.model == "interaction_with_covariate") {
        write.csv(out, "R_other/treatment_statistics_abs_interaction_with_covariate.csv", row.names=F)
    } else if (stat.model == "no_interaction_with_covariate_and_covariate") {
        write.csv(out, "R_other/treatment_statistics_abs_no_interaction_with_covariate_and_covariate.csv", row.names=F)
    } else {
        write.csv(out, "R_other/treatment_statistics_abs_paired_t_test.csv", row.names=F)
    }
    
}