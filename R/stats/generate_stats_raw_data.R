generate_stats_raw_data <- function() {
    #### Script to generate stats for each variable, based on raw data
    
    ###### ---------------- setting up runs -------------------- ######
    #### Call run.R program
    rm(list=ls(all=TRUE))
    source("run.R")
    options(warn=-1)
    
    #### Setting input parameters
    r.decision <- "stats"
    t.effect <- "ratio"
    s.model <- "interaction"
    
    ###### ----------Compute c fluxes, variables, and pools-------------- ######
    ### LAI
    s.lai <- make_lai_variable(return.decision=r.decision,
                               trt.effect=t.effect,
                               stat.model=s.model)
    
    ### SLA
    s.sla <- make_sla_variable(return.decision=r.decision,
                               trt.effect=t.effect,
                               stat.model=s.model)
    
    ### Soil bulk density at 3 depths
    ### unit is kg m-3
    #s.soil.bk <- make_soil_bulk_density(return.decision=r.decision,
    #                                    trt.effect=t.effect,
    #                                    stat.model=s.model)
    
    ### Soil C pool, using soil bulk density data
    ### output options are shallow and all_depths data
    #s.soilc <- make_soil_carbon_pool(bk_density=soil_bulk_density_variable,
    #                                 return="all_depths",
    #                                 return.decision=r.decision,
    #                                 trt.effect=t.effect,
    #                                 stat.model=s.model)
    
    ### soil respiration flux
    s.rsoil <- make_soil_respiration_flux(return.decision=r.decision,
                                          trt.effect=t.effect,
                                          stat.model=s.model)
    
    
    ### leaf C pool
    ### read in c_fraction defined in constant
    ### We can either use mean sla value or variable SLA value to calculate leaf C
    ### sla_option: "mean", or "variable"
    s.leafc <- make_leaf_pool(lai_variable, sla_variable, c_fraction,
                              sla_option = "variable",
                              return.decision=r.decision,
                              trt.effect=t.effect,
                              stat.model=s.model)
    
    
    ### Fine root pool
    ### reads in c_fraction_fr defined in constant
    s.frc <- make_fineroot_pool(c_fraction_fr,
                                return.decision=r.decision,
                                trt.effect=t.effect,
                                stat.model=s.model)
    
    
    ### fineroot c production flux
    ### reads in c_fraction_fr defined in constant
    #s.froot.prod <- make_fineroot_production_flux(c_fraction_fr,
    #                                              return.decision=r.decision,
    #                                              trt.effect=t.effect,
    #                                              stat.model=s.model)
    
    ### frass c production flux
    #s.fras <- make_frass_production_flux(return.decision=r.decision,
    #                                     trt.effect=t.effect,
    #                                     stat.model=s.model)
    
    ### herbivore leaf c consumption flux
    ### extrapolated based on frass weight, leaf area consumed and sla data
    ### This consumed leaf mass need to be added on top of the canopy c pool. 
    #s.hb.cons <- make_herbivory_leaf_consumption_flux(sla=sla_variable, 
    #                                                  frass_flux=frass_production_flux,
    #                                                  return.decision=r.decision,
    #                                                  trt.effect=t.effect,
    #                                                  stat.model=s.model)
    
    ### lerp production flux
    ### reads in c_fraction_lp from constant
    s.lerp.prod <- make_lerp_production_flux(c_fraction_lp,
                                             return.decision=r.decision,
                                             trt.effect=t.effect,
                                             stat.model=s.model)
    
    ### DOC leaching flux
    ### Can return shallow, deep and all_depths result
    ### This data also contains total dissolved carbon, and dissolved inorganic carbon
    ### For now, return only deep option
    ### Also assumes leaching = 20 ml m-2 d-1
    s.doc <- make_doc_leaching_flux(depth="deep",
                                    return.decision=r.decision,
                                    trt.effect=t.effect,
                                    stat.model=s.model)
    
    ### Litter fluxes
    ### This dataframe includes all of twig, bark, seed, leaf.
    ### reads in c_fraction coefficient from constant
    s.lit.leaf <- make_leaflitter_flux(c_fraction,
                                       return.decision=r.decision,
                                       trt.effect=t.effect,
                                       stat.model=s.model,
                                       var.col=7)
    
    s.lit.twig <- make_leaflitter_flux(c_fraction,
                                       return.decision=r.decision,
                                       trt.effect=t.effect,
                                       stat.model=s.model,
                                       var.col=4)
    
    s.lit.bark <- make_leaflitter_flux(c_fraction,
                                       return.decision=r.decision,
                                       trt.effect=t.effect,
                                       stat.model=s.model,
                                       var.col=5)
    
    s.lit.seed <- make_leaflitter_flux(c_fraction,
                                       return.decision=r.decision,
                                       trt.effect=t.effect,
                                       stat.model=s.model,
                                       var.col=6)
    
    #s.lit.other <- make_leaflitter_flux(c_fraction,
    #                                   return.decision=r.decision,
    #                                   trt.effect=t.effect,
    #                                   stat.model=s.model,
    #                                   var.col=8)
    #
    #s.lit.insect <- make_leaflitter_flux(c_fraction,
    #                                   return.decision=r.decision,
    #                                   trt.effect=t.effect,
    #                                   stat.model=s.model,
    #                                   var.col=9)
    
    ### wood C pool
    s.woodc <- make_wood_pool(ring_area,c_fraction,
                              return.decision=r.decision,
                              trt.effect=t.effect,
                              stat.model=s.model)
    
    ### Wood C production
    #s.wood.prod <- make_wood_production_flux(wood_c_pool,
    #                                         return.decision=r.decision,
    #                                         trt.effect=t.effect,
    #                                         stat.model=s.model)
    
    ### Understorey aboveground C pool
    ### reads in c_fraction from constant
    s.uac <- make_understorey_aboveground_c_pool(c_fraction,
                                                 return.decision=r.decision,
                                                 trt.effect=t.effect,
                                                 stat.model=s.model)
    
    ### Understorey production flux
    s.und.prod <- make_understorey_aboveground_production_flux(c_fraction,
                                                               return.decision=r.decision,
                                                               trt.effect=t.effect,
                                                               stat.model=s.model)
    
    
    ### Soil microbial C pool
    ### top 10 cm only - Cat's data
    #s.micc <- make_microbial_pool(soil_bulk_density_variable,
    #                              return.decision=r.decision,
    #                              trt.effect=t.effect,
    #                              stat.model=s.model)
    
    ### Soil mycorrhizal production
    #s.mycc <- make_mycorrhizal_c_pool(soil_bulk_density_variable,
    #                                  return.decision=r.decision,
    #                                  trt.effect=t.effect,
    #                                  stat.model=s.model)
    
    ### Soil methane C flux
    ## This is a simplified version because we didn't fill the gaps
    s.ch4 <- make_methane_flux(return.decision=r.decision,
                               trt.effect=t.effect,
                               stat.model=s.model)
    
    
    ### Herbivory respiration flux
    #s.rhb <- make_herbivory_respiration_flux(leaf_consumed=herbivory_leaf_consumption_flux,
    #                                         frass_prod=frass_production_flux,
    #                                         method="diff",
    #                                         return.decision=r.decision,
    #                                         trt.effect=t.effect,
    #                                         stat.model=s.model)
    
    
    ### Coarse root C pool 
    s.crc <- make_coarse_root_pool_1(c_fraction, fr_pool=fineroot_c_pool,
                                     return.decision=r.decision,
                                     trt.effect=t.effect,
                                     stat.model=s.model) 
    
    #### Coarse root C production
    #s.croot.prod <- make_coarse_root_production_flux(coarse_root_c_pool_1,
    #                                                 return.decision=r.decision,
    #                                                 trt.effect=t.effect,
    #                                                 stat.model=s.model) 
    
    ### Root respiration flux
    #s.rroot <- make_root_respiration_flux(fineroot_c_pool, coarse_root_c_pool_1,
    #                                      return.decision=r.decision,
    #                                      trt.effect=t.effect,
    #                                      stat.model=s.model)
    
    ### Rh C flux
    #s.rh <- make_heterotrophic_respiration_flux(soil_respiration_flux, 
    #                                            root_respiration_flux,
    #                                            return.decision=r.decision,
    #                                            trt.effect=t.effect,
    #                                            stat.model=s.model)
    
    #### Overstorey GPP 
    
    ### Overstorey foliage respiration
    
    ### Understorey GPP
    
    ### Understorey respiration
    ### assumes either a fixed or a function of temperature
    #s.rund <- make_understorey_respiration_flux(c_pool=understorey_aboveground_c_pool,
    #                                            c_frac=c_fraction,
    #                                            gpp=understorey_gpp_flux,
    #                                            assumption="cue",
    #                                            return.decision=r.decision,
    #                                            trt.effect=t.effect,
    #                                            stat.model=s.model)
    
    
    #### Create a output table to store all stats
    var.list <- c("lai", "sla","soil_c","leaf_c","wood_c","fineroot_c",
                  "coarseroot_c","understorey_c","understorey_c_2","understorey_c_live","understorey_c_dead",
                  "microbial_c","mycorrhizal_c",
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
    #out[out$Variable=="understorey_c_live",2:17] <- assign_stats(s.var=s.uac.live)
    #out[out$Variable=="understorey_c_dead",2:17] <- assign_stats(s.var=s.uac.dead)
    out[out$Variable=="microbial_c",2:17] <- assign_stats(s.var=s.micc)
    out[out$Variable=="mycorrhizal_c",2:17] <- assign_stats(s.var=s.mycc)
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
    #out[out$Variable=="twig_prod",2:17] <- assign_stats(s.var=s.lit.twig)
    #out[out$Variable=="bark_prod",2:17] <- assign_stats(s.var=s.lit.bark)
    #out[out$Variable=="seed_prod",2:17] <- assign_stats(s.var=s.lit.seed)
    out[out$Variable=="wood_prod",2:17] <- assign_stats(s.var=s.wood.prod)
    out[out$Variable=="fineroot_prod",2:17] <- assign_stats(s.var=s.froot.prod)
    out[out$Variable=="coarseroot_prod",2:17] <- assign_stats(s.var=s.croot.prod)
    out[out$Variable=="understorey_prod",2:17] <- assign_stats(s.var=s.und.prod)
    out[out$Variable=="hetero_respiration",2:17] <- assign_stats(s.var=s.rh)
    
    
    if (s.model == "dynamic") {
        write.csv(out, "R_other/treatment_statistics_abs_dynamic.csv", row.names=F)
    } else if (s.model == "no_interaction") {
        write.csv(out, "R_other/treatment_statistics_abs_no_interaction.csv", row.names=F)
    } else if (s.model == "interaction") {
        write.csv(out, "R_other/treatment_statistics_abs_interaction.csv", row.names=F)
    } else {
        write.csv(out, "R_other/treatment_statistics_abs_no_random_effect.csv", row.names=F)
    }
}