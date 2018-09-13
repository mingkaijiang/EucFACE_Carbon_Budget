generate_stats_abs_covariate <- function(stat.model) {
    
    ### remove pre-treatment data period for individual variables (e.g. leaf, wood, soil, mic, myc)
    
    #### Work on each variable per time
    ### Soil C
    s.soilc <- make_soilc_treatment_abs_effect_statistics(inDF=soil_c_pool, 
                                                          var.cond="pool", var.col=3,
                                                          date.as.factor=T,
                                                          stat.model="no_interaction_with_covariate")
    
    ### Leaf C
    s.leafc <- make_leafc_treatment_abs_effect_statistics(inDF=leaf_c_pool, 
                                                          var.cond="pool", var.col=3,
                                                          date.as.factor=T,
                                                          stat.model="no_interaction_with_covariate")
    
    ### Wood C pool
    s.woodc <- make_woodc_treatment_abs_effect_statistics(inDF=wood_c_pool, 
                                                          var.cond="pool", var.col=3,
                                                          date.as.factor=T,
                                                          stat.model="no_interaction_with_covariate") 
    
    ### Fineroot C pool
    s.frc <- make_frootc_treatment_abs_effect_statistics(inDF=fineroot_c_pool, 
                                                         var.cond="pool", var.col=3,
                                                         date.as.factor=T,
                                                         stat.model="no_interaction_with_covariate")
    
    ### Coarseroot C pool
    s.crc <- make_crootc_treatment_abs_effect_statistics(inDF=coarse_root_c_pool_1, 
                                            var.cond="pool", var.col=3,
                                            date.as.factor=T,
                                            stat.model="no_interaction_with_covariate")
    
    ### Understorey aboveground C pool
    s.uac <- make_uac_treatment_abs_effect_statistics(inDF=understorey_aboveground_c_pool, 
                                                      var.cond="pool", var.col=5,
                                                      date.as.factor=T,
                                                      stat.model="no_interaction_with_covariate")
    
    s.uac2 <- make_uac_treatment_abs_effect_statistics(inDF=understorey_aboveground_c_pool_2, 
                                                        var.cond="pool", var.col=3,
                                                        date.as.factor=T,
                                                        stat.model="no_interaction_with_covariate")
    
    ### Microbial C pool
    s.micc <- make_micc_treatment_abs_effect_statistics(inDF=microbial_c_pool, 
                                                        var.cond="pool", var.col=3,
                                                        date.as.factor=T,
                                                        stat.model="no_interaction_with_covariate")
    
    ### Mycorrhizal C pool
    s.mycc <- make_mycc_treatment_abs_effect_statistics(inDF=mycorrhizal_c_pool, 
                                                        var.cond="pool", var.col=3,
                                                        date.as.factor=T,
                                                        stat.model="no_interaction_with_covariate")
    
    
    ### Leaf litter C pool
    s.litc <- make_litc_treatment_abs_effect_statistics(inDF=leaflitter_pool, 
                                                        var.cond="pool", var.col=6,
                                                        date.as.factor=T,
                                                        stat.model="no_interaction_with_covariate")
    
    ### Insect pool
    s.insc <- make_insc_treatment_abs_effect_statistics(inDF=insect_pool, 
                                                            var.cond="pool", var.col=3,
                                                            date.as.factor=T,
                                                            stat.model="no_interaction_with_covariate")
    
    ### Overstorey GPP
    s.o.gpp <- make_overstorey_gpp_treatment_abs_effect_statistics(inDF=overstorey_gpp_flux, 
                                                                   var.cond="ann.flux", var.col=3,
                                                                   date.as.factor=T,
                                                                   stat.model="no_interaction_with_covariate")
    
    ### Understorey GPP
    s.u.gpp <- make_understorey_gpp_treatment_abs_effect_statistics(inDF=understorey_gpp_flux, 
                                                                    var.cond="ann.flux", var.col=3,
                                                                    date.as.factor=T,
                                                                    stat.model="no_interaction_with_covariate")
    
    ### Overstorey Leaf respiration
    s.rleaf <- make_overstorey_ra_leaf_treatment_abs_effect_statistics(inDF=overstorey_leaf_respiration_flux, 
                                                                      var.cond="ann.flux", var.col=3,
                                                                      date.as.factor=T,
                                                                      stat.model="no_interaction_with_covariate")
    
    ### Wood respiration
    s.rwood <- make_ra_wood_treatment_abs_effect_statistics(inDF=wood_respiration_flux, 
                                               var.cond="flux", var.col=5,
                                               date.as.factor=T,
                                               stat.model="no_interaction_with_covariate")
    
    ### Root respiration
    s.rroot <- make_ra_root_treatment_abs_effect_statistics(inDF=root_respiration_flux, 
                                                            var.cond="flux", var.col=5,
                                                            date.as.factor=T,
                                                            stat.model="no_interaction_with_covariate")
    
    ### Understorey respiration
    s.rund <- make_ra_und_treatment_abs_effect_statistics(inDF=understorey_respiration_flux, 
                                                          var.cond="flux", var.col=5,
                                                          date.as.factor=T,
                                                          stat.model="no_interaction_with_covariate")
    
    ### Frass production
    s.fras <- make_frass_treatment_abs_effect_statistics(inDF=frass_production_flux, 
                                                         var.cond="flux", var.col=5,
                                                         date.as.factor=T,
                                                         stat.model="no_interaction_with_covariate")
    
    ### herbivory leaf consumption flux
    s.hb.cons <- make_hb_cons_treatment_abs_effect_statistics(inDF=herbivory_leaf_consumption_flux, 
                                                              var.cond="flux", var.col=5,
                                                              date.as.factor=T,
                                                              stat.model="no_interaction_with_covariate")
    
    ### Herbivory respiration
    s.rhb <- make_r_hb_treatment_abs_effect_statistics(inDF=herbivory_respiration_flux, 
                                                       var.cond="flux", var.col=5,
                                                       date.as.factor=T,
                                                       stat.model="no_interaction_with_covariate")
    
    ### Lerp production
    s.lerp.prod <- make_lp_treatment_abs_effect_statistics(inDF=lerp_production_flux, 
                                                           var.cond="flux", var.col=5,
                                                           date.as.factor=T,
                                                           stat.model="no_interaction_with_covariate")
    
    ### soil respiration
    s.rsoil <- make_rsoil_treatment_abs_effect_statistics(inDF=soil_respiration_flux, 
                                                          var.cond="flux", var.col=5,
                                                          date.as.factor=T,
                                                          stat.model="no_interaction_with_covariate")
    
    ### DOC leaching
    s.doc <- make_doc_treatment_abs_effect_statistics(inDF=doc_leaching_flux, 
                                                      var.cond="flux", var.col=5,
                                                      date.as.factor=T,
                                                      stat.model="no_interaction_with_covariate")
    
    ### CH4 uptake - un-gap filled data
    s.ch4 <- make_ch4_treatment_abs_effect_statistics(inDF=methane_c_flux, 
                                                      var.cond="flux", var.col=3,
                                                      date.as.factor=T,
                                                      stat.model="no_interaction_with_covariate")
    
    ### Leaflitter flux
    s.lit.leaf <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                   var.cond="flux", var.col=6,
                                                                   date.as.factor=T,
                                                                   stat.model="no_interaction_with_covariate")  
    
    ### twig litter flux
    s.lit.twig <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                   var.cond="flux", var.col=3,
                                                                   date.as.factor=T,
                                                                   stat.model="no_interaction_with_covariate")
    
    ### bark litter flux
    s.lit.bark <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                   var.cond="flux", var.col=4,
                                                                   date.as.factor=T,
                                                                   stat.model="no_interaction_with_covariate") 
    
    ### Seed litter flux
    s.lit.seed <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                   var.cond="flux", var.col=5,
                                                                   date.as.factor=T,
                                                                   stat.model="no_interaction_with_covariate")
    
    ### Wood production flux
    s.wood.prod <- make_wood_prod_treatment_abs_effect_statistics(inDF=wood_production_flux, 
                                               var.cond="flux", var.col=5,
                                               date.as.factor=T,
                                               stat.model="no_interaction_with_covariate") 
    
    ### Fineroot production flux
    s.froot.prod <- make_froot_prod_treatment_abs_effect_statistics(inDF=fineroot_production_flux, 
                                                                    var.cond="flux", var.col=5,
                                                                    date.as.factor=T,
                                                                    stat.model="no_interaction_with_covariate") 
    
    ### Coarseroot production
    s.croot.prod <- make_croot_prod_treatment_abs_effect_statistics(inDF=coarse_root_production_flux_1, 
                                                                    var.cond="flux", var.col=5,
                                                                    date.as.factor=T,
                                                                    stat.model="no_interaction_with_covariate")
    
    ### Understorey aboveground production
    s.und.prod <- make_und_prod_treatment_abs_effect_statistics(inDF=understorey_aboveground_production_flux, 
                                                                var.cond="flux", var.col=5,
                                                                date.as.factor=T,
                                                                stat.model="no_interaction_with_covariate")
    
    ### Rh respiration
    s.rh <- make_rh_treatment_abs_effect_statistics(inDF=heterotrophic_respiration_flux, 
                                                    var.cond="flux", var.col=5,
                                                    date.as.factor=T,
                                                    stat.model="no_interaction_with_covariate") 
    
    #### Create a output table to store all stats
    var.list <- c("soil_c","leaf_c","wood_c","fineroot_c",
                  "coarseroot_c","understorey_c","understorey_c_2",
                  "microbial_c","mycorrhizal_c","litter_c","insect_c","cwd_c",
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
    out[out$Variable=="leaf_prod",2:17] <- assign_stats(s.var=s.lit.leaf)
    out[out$Variable=="twig_prod",2:17] <- assign_stats(s.var=s.lit.twig)
    out[out$Variable=="bark_prod",2:17] <- assign_stats(s.var=s.lit.bark)
    out[out$Variable=="seed_prod",2:17] <- assign_stats(s.var=s.lit.seed)
    out[out$Variable=="wood_prod",2:17] <- assign_stats(s.var=s.wood.prod)
    out[out$Variable=="fineroot_prod",2:17] <- assign_stats(s.var=s.froot.prod)
    out[out$Variable=="coarseroot_prod",2:17] <- assign_stats(s.var=s.croot.prod)
    out[out$Variable=="understorey_prod",2:17] <- assign_stats(s.var=s.und.prod)
    out[out$Variable=="hetero_respiration",2:17] <- assign_stats(s.var=s.rh)
    out[out$Variable=="over_gpp",2:17] <- assign_stats(s.var=s.o.gpp)
    out[out$Variable=="over_leaf_respiration",2:17] <- assign_stats(s.var=s.rleaf)
    out[out$Variable=="understorey_gpp",2:17] <- assign_stats(s.var=s.u.gpp)

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