make_table_by_ring <- function() {
    #### Generate ring-specific summary table
    #### ignoring time 
    
    ##############################################
    #### Method 3
    #### NPP fluxes (Method 3 of getting NEP)
    ##############################################
    ### set up dataframe
    term <- c("Leaf NPP", "Stem NPP", "Fine Root NPP", 
              "Coarse Root NPP", "Other NPP",
              "Understorey NPP",
              "Frass production", "Leaf consumption", "R hetero", 
              "Mycorrhizal production", "Flower production")
    npp <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(npp) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2", 
                       "diff", "percent_diff", "aCO2_sd", "eCO2_sd")
    Ring <- c(1:6)

    for (i in Ring) {
        
        # Leaf NPP
        npp[npp$term == "Leaf NPP", i+1] <- with(leaflitter_flux[leaflitter_flux$Ring ==i,],
                                                            sum(leaf_flux*ndays)/sum(ndays)) * conv
        
        # Stem NPP
        npp[npp$term == "Stem NPP", i+1] <- with(wood_production_flux[wood_production_flux$Ring ==i,],
                                                        sum(wood_production_flux*ndays)/sum(ndays)) * conv
        
        # Fine Root NPP
        npp[npp$term == "Fine Root NPP", i+1] <- with(fineroot_production_flux[fineroot_production_flux$Ring ==i,],
                                                          sum(fineroot_production_flux*ndays)/sum(ndays)) * conv
        
        # Coarse Root NPP
        npp[npp$term == "Coarse Root NPP", i+1] <- with(coarse_root_production_flux_1[coarse_root_production_flux_1$Ring ==i,],
                                                          sum(coarse_root_production_flux*ndays)/sum(ndays)) * conv
        
        # Other NPP (i.e. twigs, barks and seeds)
        npp[npp$term == "Other NPP", i+1] <- with(leaflitter_flux[leaflitter_flux$Ring ==i,],
                                                  sum((twig_flux+seed_flux+bark_flux)*ndays)/sum(ndays)) * conv
        
        # Frass Production
        npp[npp$term == "Frass production", i+1] <- with(frass_production_flux[frass_production_flux$Ring == i,],
                                                         sum(frass_production_flux*ndays)/sum(ndays)) * conv 
        
        # Leaf consumption
        npp[npp$term == "Leaf consumption", i+1] <- with(herbivory_leaf_consumption_flux[herbivory_leaf_consumption_flux$Ring == i,],
                                                         sum(herbivory_leaf_consumption_flux*ndays)/sum(ndays)) * conv 
        
        # Understorey NPP
        npp[npp$term == "Understorey NPP", i+1] <- with(understorey_aboveground_production_flux[understorey_aboveground_production_flux$Ring == i,],
                                                         sum(understorey_production_flux*ndays)/sum(ndays)) * conv 
        
        # R heterotrophic respiration
        npp[npp$term == "R hetero", i+1] <- with(heterotrophic_respiration_flux[heterotrophic_respiration_flux$Ring == i,],
                                                 sum(heterotrophic_respiration_flux*ndays)/sum(ndays)) * conv 

        # Mycorrhizal production
        #npp[npp$term == "Mycorrhizal production", i+1] <- with(mycorrhizal_c_production_flux[mycorrhizal_c_production_flux$Ring == i,],
        #                                         sum(mycorrhizal_production*ndays)/sum(ndays)) * conv 
        # Flower Production
        
    }

    
    ##############################################
    #### Method 1
    #### In / out fluxes (Method 1 of getting NEP)
    ##############################################
    ### define terms and dataframe
    term <- c("GPP overstorey", "GPP understorey", "CH4 efflux",
              "Ra leaf", "Ra stem", "Ra root", "Ra understorey", "VOC",
              "Rherbivore", "DOC loss", "Rsoil", "Rgrowth")
    inout <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(inout) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2",
                         "diff", "percent_diff")
    
    for (i in Ring) {
        
        # GPP overstorey - already annual
        inout[inout$term == "GPP overstorey", i+1] <- mean(overstorey_gpp_flux[overstorey_gpp_flux$Ring == i, "GPP"])
        
        # GPP understorey - already annual
        inout[inout$term == "GPP understorey", i+1] <- mean(understorey_gpp_flux[understorey_gpp_flux$Ring == i, "GPP"])
        
        # Ra leaf - already annual
        inout[inout$term == "Ra leaf", i+1] <- mean(overstorey_leaf_respiration_flux[overstorey_leaf_respiration_flux$Ring == i, "Rfoliage"])
        
        # Ra root
        inout[inout$term == "Ra root", i+1] <- with(root_respiration_flux[root_respiration_flux$Ring == i,],
                                                    sum(root_respiration_flux*ndays)/sum(ndays)) * conv 
        
        # Ra stem
        inout[inout$term == "Ra stem", i+1] <- with(wood_respiration_flux[wood_respiration_flux$Ring == i,],
                                                    sum(wood_respiration*ndays)/sum(ndays)) * conv 

        # Rgrowth
        inout[inout$term == "Rgrowth", i+1] <- ccost * (npp[npp$term == "Leaf NPP", i+1] + 
                                                        npp[npp$term == "Stem NPP", i+1] + 
                                                        npp[npp$term == "Other NPP", i+1] +
                                                        npp[npp$term == "Fine Root NPP", i+1] +
                                                        npp[npp$term == "Coarse Root NPP", i+1] +
                                                        npp[npp$term == "Leaf consumption", i+1])
        
        # Rherbivore
        inout[inout$term == "Rherbivore", i+1] <- with(herbivory_respiration_flux[herbivory_respiration_flux$Ring ==i,],
                                                       sum(respiration_flux*ndays)/sum(ndays)) * conv
        
        # Ra understorey
        inout[inout$term == "Ra understorey", i+1] <- with(understorey_respiration_flux[understorey_respiration_flux$Ring ==i,],
                                                           sum(respiration*ndays)/sum(ndays)) * conv
        
        # Rsoil
        inout[inout$term == "Rsoil", i+1] <- with(soil_respiration_flux[soil_respiration_flux$Ring ==i,],
                                                  sum(soil_respiration_flux*ndays)/sum(ndays)) * conv
            
        # DOC loss
        inout[inout$term == "DOC loss", i+1] <- with(doc_leaching_flux[doc_leaching_flux$Ring ==i,],
                                                     sum(doc_leaching_flux*ndays)/sum(ndays)) * conv
            
        
        # VOC
        inout[inout$term == "VOC", i+1] <- mean(voc_emission_flux[voc_emission_flux$Ring == i, "voc_flux"])
        
        # CH4
        inout[inout$term == "CH4 efflux", i+1] <- with(methane_c_flux[methane_c_flux$Ring ==i,],
                                                      sum(methane_flux*ndays)/sum(ndays)) * conv
    }
    
    ##############################################
    #### Method 2
    #### Standing C pools
    ##############################################    
    ### Define terms and dataframe
    term <- c("Overstorey leaf", "Overstorey wood", "Understorey above-ground",
              "Fine Root", "Coarse Root", "Litter", "Coarse woody debris", 
              "Microbial biomass", "Soil C", "Mycorrhizae", "Insects")
    pool <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(pool) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2", 
                        "diff", "percent_diff", "aCO2_sd", "eCO2_sd")

    for (i in Ring) {
        
        # Overstorey leaf
        pool[pool$term == "Overstorey leaf", i+1] <- mean(leaf_c_pool[leaf_c_pool$Ring == i, "leaf_pool"], na.rm=T)
        
        # Overstorey wood
        pool[pool$term == "Overstorey wood", i+1] <- mean(wood_c_pool[wood_c_pool$Ring == i, "wood_pool"], na.rm=T)
        
        # Fine Root
        pool[pool$term == "Fine Root", i+1] <- mean(fineroot_c_pool[fineroot_c_pool$Ring == i, "fineroot_pool"], na.rm=T)
        
        # Coarse Root
        pool[pool$term == "Coarse Root", i+1] <- mean(coarse_root_c_pool_1[coarse_root_c_pool_1$Ring == i, "coarse_root_pool"], na.rm=T)
        
        # Understorey above-ground
        pool[pool$term == "Understorey above-ground", i+1] <- mean(understorey_aboveground_c_pool[understorey_aboveground_c_pool$Ring == i, 
                                                                                                  "Total_g_C_m2"], na.rm=T)
        
        # Soil C
        pool[pool$term == "Soil C", i+1] <- mean(soil_c_pool[soil_c_pool$Ring == i, "soil_carbon_pool"], na.rm=T)
        
        # Microbial biomass
        pool[pool$term == "Microbial biomass", i+1]  <- mean(microbial_c_pool[microbial_c_pool$Ring == i, "microbial_pool"], na.rm=T)
        
        # Coarse Woody Debris
        pool[pool$term == "Coarse woody debris", i+1]  <- mean(standing_dead_c_pool[standing_dead_c_pool$Ring == i, "wood_pool"], na.rm=T)
        
        # Mycorrhizae
        pool[pool$term == "Mycorrhizae", i+1]  <- mean(mycorrhizal_c_pool[mycorrhizal_c_pool$Ring == i, "mycorrhizal_c_pool"], na.rm=T)
        
        # Insects
        pool[pool$term == "Insects", i+1] <- mean(insect_pool[insect_pool$Ring == i, "insect_pool"], na.rm=T)
        
        # Litter
        pool[pool$term == "Litter", i+1]  <- mean(leaflitter_pool[leaflitter_pool$Ring == i, "leaflitter_pool"], na.rm=T)
        
    }
    
    
    ##############################################
    #### Method 2
    #### change Standing C pools
    ##############################################    
    ### Define terms and dataframe
    term <- c("Overstorey leaf", "Overstorey wood", "Understorey above-ground",
              "Fine Root", "Coarse Root", "Litter", 
              "Microbial biomass", "Soil C", "Mycorrhizae", "Insects")
    delta_pool <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(delta_pool) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2", 
                              "diff", "percent_diff", 
                              "aCO2_sd", "eCO2_sd")
    
    for (i in Ring) {
        
        # Overstorey leaf
        delta_pool[delta_pool$term == "Overstorey leaf", i+1] <- mean(delta_leaf_c_pool$delta[delta_leaf_c_pool$Ring == i], na.rm=T)
        
        # Overstorey wood
        delta_pool[delta_pool$term == "Overstorey wood", i+1] <- mean(delta_wood_c_pool$delta[delta_wood_c_pool$Ring == i], na.rm=T)
        
        # Fine Root
        delta_pool[delta_pool$term == "Fine Root", i+1] <- mean(delta_fineroot_c_pool$delta[delta_fineroot_c_pool$Ring == i], na.rm=T)
        
        # Coarse Root
        delta_pool[delta_pool$term == "Coarse Root", i+1] <- mean(delta_coarse_root_c_pool$delta[delta_coarse_root_c_pool$Ring == i], na.rm=T)
        
        # Understorey above-ground
        delta_pool[delta_pool$term == "Understorey above-ground", i+1] <- mean(delta_understorey_aboveground_c_pool$delta[delta_understorey_aboveground_c_pool$Ring == i], na.rm=T)
        
        # Soil C
        delta_pool[delta_pool$term == "Soil C", i+1] <- mean(delta_soil_c_pool$delta[delta_soil_c_pool$Ring == i], na.rm=T)
        
        # Microbial biomass
        delta_pool[delta_pool$term == "Microbial biomass", i+1]  <- mean(delta_microbial_c_pool$delta[delta_microbial_c_pool$Ring == i], na.rm=T)
        
        # Mycorrhizae
        delta_pool[delta_pool$term == "Mycorrhizae", i+1]  <- mean(delta_mycorrhizal_c_pool$delta[delta_mycorrhizal_c_pool$Ring == i], na.rm=T)
        
        # Insects
        delta_pool[delta_pool$term == "Insects", i+1] <- mean(delta_insect_pool$delta[delta_insect_pool$Ring == i], na.rm=T)
        
        # Litter
        delta_pool[delta_pool$term == "Litter", i+1]  <- mean(delta_leaflitter_pool$delta[delta_leaflitter_pool$Ring == i], na.rm=T)
        
    }
    
    ###### calculate aCO2 and eCO2 results
    inout$aCO2 <- rowMeans(subset(inout, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    inout$eCO2 <- rowMeans(subset(inout, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    inout$aCO2_sd <- rowSds(as.matrix(subset(inout, select=c(Ring_2, Ring_3, Ring_6))), na.rm=T)
    inout$eCO2_sd <- rowSds(as.matrix(subset(inout, select=c(Ring_1, Ring_4, Ring_5))), na.rm=T)
    
    npp$aCO2 <- rowMeans(subset(npp, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    npp$eCO2 <- rowMeans(subset(npp, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    npp$aCO2_sd <- rowSds(as.matrix(subset(npp, select=c(Ring_2, Ring_3, Ring_6))), na.rm=T)
    npp$eCO2_sd <- rowSds(as.matrix(subset(npp, select=c(Ring_1, Ring_4, Ring_5))), na.rm=T)
    
    pool$aCO2 <- rowMeans(subset(pool, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    pool$eCO2 <- rowMeans(subset(pool, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    pool$aCO2_sd <- rowSds(as.matrix(subset(pool, select=c(Ring_2, Ring_3, Ring_6))), na.rm=T)
    pool$eCO2_sd <- rowSds(as.matrix(subset(pool, select=c(Ring_1, Ring_4, Ring_5))), na.rm=T)
    
    delta_pool$aCO2 <- rowMeans(subset(delta_pool, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    delta_pool$eCO2 <- rowMeans(subset(delta_pool, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    delta_pool$aCO2_sd <- rowSds(as.matrix(subset(delta_pool, select=c(Ring_2, Ring_3, Ring_6))), na.rm=T)
    delta_pool$eCO2_sd <- rowSds(as.matrix(subset(delta_pool, select=c(Ring_1, Ring_4, Ring_5))), na.rm=T)
    
    ###### Diff (eCO2 - aCO2)
    inout$diff <- inout$eCO2 - inout$aCO2
    npp$diff <- npp$eCO2 - npp$aCO2
    pool$diff <- pool$eCO2 - pool$aCO2
    delta_pool$diff <- delta_pool$eCO2 - delta_pool$aCO2
    
    
    ###### percent differences (eCO2 - aCO2) / aCO2 * 100
    inout$percent_diff <- (inout$eCO2 - inout$aCO2) / (inout$aCO2) * 100
    npp$percent_diff <- (npp$eCO2 - npp$aCO2) / (npp$aCO2) * 100
    pool$percent_diff <- (pool$eCO2 - pool$aCO2) / (pool$aCO2) * 100
    delta_pool$percent_diff <- (delta_pool$eCO2 - delta_pool$aCO2) / (delta_pool$aCO2) * 100
    
    
    ##### output tables
    return(list(inout = data.table(inout), 
                npp = data.table(npp), 
                pool = data.table(pool),
                delta_pool = data.table(delta_pool)))
}
