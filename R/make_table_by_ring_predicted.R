make_table_by_ring_predicted <- function() {
    #### Generate ring-specific summary table
    #### Based on predicted data
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
        npp[npp$term == "Leaf NPP", i+1] <- mean(leaflitter_flux_ann$predicted[leaflitter_flux_ann$Ring ==i]) 
        
        # Stem NPP
        npp[npp$term == "Stem NPP", i+1] <- mean(wood_production_flux_ann$predicted[wood_production_flux_ann$Ring ==i])
        
        # Fine Root NPP
        npp[npp$term == "Fine Root NPP", i+1] <- mean(fineroot_production_flux_ann$predicted[fineroot_production_flux_ann$Ring ==i])
        
        # Coarse Root NPP
        npp[npp$term == "Coarse Root NPP", i+1] <- mean(coarse_root_production_flux_ann$predicted[coarse_root_production_flux_ann$Ring ==i])
        
        # Other NPP (i.e. twigs, barks and seeds)
        npp[npp$term == "Other NPP", i+1] <- mean(twiglitter_flux_ann$predicted[twiglitter_flux_ann$Ring ==i])+
            mean(barklitter_flux_ann$predicted[barklitter_flux_ann$Ring ==i])+
            mean(seedlitter_flux_ann$predicted[seedlitter_flux_ann$Ring ==i])
        
        # Frass Production
        npp[npp$term == "Frass production", i+1] <- mean(frass_production_flux_ann$predicted[frass_production_flux_ann$Ring == i]) 
        
        # Leaf consumption
        npp[npp$term == "Leaf consumption", i+1] <- mean(herbivory_leaf_consumption_flux_ann$predicted[herbivory_leaf_consumption_flux_ann$Ring == i])
        
        # Understorey NPP
        npp[npp$term == "Understorey NPP", i+1] <- mean(understorey_aboveground_production_flux_ann$predicted[understorey_aboveground_production_flux_ann$Ring == i])
        
        # R heterotrophic respiration
        npp[npp$term == "R hetero", i+1] <- mean(heterotrophic_respiration_flux_ann$predicted[heterotrophic_respiration_flux_ann$Ring == i])

        # Mycorrhizal production
        npp[npp$term == "Mycorrhizal production", i+1] <- mean(mycorrhizal_c_production_flux_ann$predicted[mycorrhizal_c_production_flux_ann$Ring == i])
        
    }

    
    ##############################################
    #### Method 1
    #### In / out fluxes (Method 1 of getting NEP)
    ##############################################
    ### define terms and dataframe
    term <- c("GPP overstorey", "GPP understorey", "CH4 efflux",
              "Ra leaf", "Ra stem", "Ra root", "Ra understorey", "VOC",
              "Rherbivore", "DOC loss", "Rsoil", "Rgrowth")
    inout <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(inout) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2",
                         "diff", "percent_diff", "aCO2_sd", "eCO2_sd")
    
    for (i in Ring) {
        
        # GPP overstorey - already annual
        inout[inout$term == "GPP overstorey", i+1] <- mean(overstorey_gpp_flux_ann$predicted[overstorey_gpp_flux_ann$Ring == i])
        
        # GPP understorey - already annual
        inout[inout$term == "GPP understorey", i+1] <- mean(understorey_gpp_flux_ann$predicted[understorey_gpp_flux_ann$Ring == i])
        
        # Ra leaf - already annual
        inout[inout$term == "Ra leaf", i+1] <- mean(overstorey_leaf_respiration_flux_ann$predicted[overstorey_leaf_respiration_flux_ann$Ring == i])
        
        # Ra root
        inout[inout$term == "Ra root", i+1] <- mean(root_respiration_flux_ann$predicted[root_respiration_flux_ann$Ring == i])
        
        # Ra stem
        inout[inout$term == "Ra stem", i+1] <- mean(wood_respiration_flux_ann$predicted[wood_respiration_flux_ann$Ring == i])

        # Rgrowth
        inout[inout$term == "Rgrowth", i+1] <- ccost * (npp[npp$term == "Leaf NPP", i+1] + 
                                                        npp[npp$term == "Stem NPP", i+1] + 
                                                        #npp[npp$term == "Fine Root NPP", i+1] +
                                                        #npp[npp$term == "Coarse Root NPP", i+1] +
                                                        npp[npp$term == "Understorey NPP", i+1] + 
                                                        npp[npp$term == "Other NPP", i+1] +
                                                        npp[npp$term == "Leaf consumption", i+1])
        
        # Rherbivore
        inout[inout$term == "Rherbivore", i+1] <- mean(herbivory_respiration_flux_ann$predicted[herbivory_respiration_flux_ann$Ring ==i])
        
        # Ra understorey
        inout[inout$term == "Ra understorey", i+1] <- mean(understorey_respiration_flux_ann$predicted[understorey_respiration_flux_ann$Ring ==i])
        
        # Rsoil
        inout[inout$term == "Rsoil", i+1] <- mean(soil_respiration_flux_ann$predicted[soil_respiration_flux_ann$Ring ==i])
            
        # DOC loss
        inout[inout$term == "DOC loss", i+1] <- mean(doc_leaching_flux_ann$predicted[doc_leaching_flux_ann$Ring ==i])
            
        
        # VOC
        
        # CH4
        inout[inout$term == "CH4 efflux", i+1] <- mean(methane_c_flux_ann$predicted[methane_c_flux_ann$Ring ==i])
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
        pool[pool$term == "Overstorey leaf", i+1] <- mean(leaf_c_pool_ann$predicted[leaf_c_pool_ann$Ring == i], na.rm=T)
        
        # Overstorey wood
        pool[pool$term == "Overstorey wood", i+1] <- mean(wood_c_pool_ann$predicted[wood_c_pool_ann$Ring == i], na.rm=T)
        
        # Fine Root
        pool[pool$term == "Fine Root", i+1] <- mean(fineroot_c_pool_ann$predicted[fineroot_c_pool_ann$Ring == i], na.rm=T)
        
        # Coarse Root
        pool[pool$term == "Coarse Root", i+1] <- mean(coarse_root_c_pool_ann$predicted[coarse_root_c_pool_ann$Ring == i], na.rm=T)
        
        # Understorey above-ground
        pool[pool$term == "Understorey above-ground", i+1] <- mean(understorey_aboveground_c_pool_ann$predicted[understorey_aboveground_c_pool_ann$Ring == i], na.rm=T)
        
        # Soil C
        pool[pool$term == "Soil C", i+1] <- mean(soil_c_pool_ann$predicted[soil_c_pool_ann$Ring == i], na.rm=T)
        
        # Microbial biomass
        pool[pool$term == "Microbial biomass", i+1]  <- mean(microbial_c_pool_ann$predicted[microbial_c_pool_ann$Ring == i], na.rm=T)
        
        # Coarse Woody Debris
        pool[pool$term == "Coarse woody debris", i+1]  <- mean(standing_dead_c_pool$wood_pool[standing_dead_c_pool$Ring == i], na.rm=T)
        
        # Mycorrhizae
        pool[pool$term == "Mycorrhizae", i+1]  <- mean(mycorrhizal_c_pool_ann$predicted[mycorrhizal_c_pool_ann$Ring == i], na.rm=T)
        
        # Insects
        pool[pool$term == "Insects", i+1] <- mean(insect_pool_ann$predicted[insect_pool_ann$Ring == i], na.rm=T)
        
        # Litter
        pool[pool$term == "Litter", i+1]  <- mean(leaflitter_pool_ann$predicted[leaflitter_pool_ann$Ring == i], na.rm=T)
        
    }
    
    ###### calculate aCO2 and eCO2 results
    inout$aCO2 <- rowMeans(subset(inout, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    inout$eCO2 <- rowMeans(subset(inout, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    npp$aCO2 <- rowMeans(subset(npp, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    npp$eCO2 <- rowMeans(subset(npp, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    pool$aCO2 <- rowMeans(subset(pool, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    pool$eCO2 <- rowMeans(subset(pool, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    ###### Diff (eCO2 - aCO2)
    inout$diff <- inout$eCO2 - inout$aCO2
    npp$diff <- npp$eCO2 - npp$aCO2
    pool$diff <- pool$eCO2 - pool$aCO2
    
    
    ###### percent differences (eCO2 - aCO2) / aCO2 * 100
    inout$percent_diff <- (inout$eCO2 - inout$aCO2) / (inout$aCO2) * 100
    npp$percent_diff <- (npp$eCO2 - npp$aCO2) / (npp$aCO2) * 100
    pool$percent_diff <- (pool$eCO2 - pool$aCO2) / (pool$aCO2) * 100
    
    ##### calculate ring-based standard deviation for each variable
    aC <- data.frame(inout$Ring_2, inout$Ring_3, inout$Ring_6)
    aCo <- transform(aC, SD = apply(aC, 1, sd, na.rm=T))
    inout$aCO2_sd <- aCo$SD
    
    aC <- data.frame(npp$Ring_2, npp$Ring_3, npp$Ring_6)
    aCo <- transform(aC, SD = apply(aC, 1, sd, na.rm=T))
    npp$aCO2_sd <- aCo$SD
    
    aC <- data.frame(pool$Ring_2, pool$Ring_3, pool$Ring_6)
    aCo <- transform(aC, SD = apply(aC, 1, sd, na.rm=T))
    pool$aCO2_sd <- aCo$SD
    
    eC <- data.frame(inout$Ring_1, inout$Ring_4, inout$Ring_5)
    eCo <- transform(eC, SD = apply(eC, 1, sd, na.rm=T))
    inout$eCO2_sd <- eCo$SD
    
    eC <- data.frame(npp$Ring_1, npp$Ring_4, npp$Ring_5)
    eCo <- transform(eC, SD = apply(eC, 1, sd, na.rm=T))
    npp$eCO2_sd <- eCo$SD
    
    eC <- data.frame(pool$Ring_1, pool$Ring_4, pool$Ring_5)
    eCo <- transform(eC, SD = apply(eC, 1, sd, na.rm=T))
    pool$eCO2_sd <- eCo$SD
    
    ##### output tables
    return(list(inout = data.table(inout), 
                npp = data.table(npp), 
                pool = data.table(pool)))
}
