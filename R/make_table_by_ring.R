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
    inout <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(inout) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2",
                         "diff", "percent_diff", "aCO2_sd", "eCO2_sd")
    
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

        # Rgrowth
        inout[inout$term == "Rgrowth", i+1] <- ccost * (npp[npp$term == "Leaf NPP", i+1] + 
                                                        npp[npp$term == "Stem NPP", i+1] + 
                                                        npp[npp$term == "Fine Root NPP", i+1] +
                                                        npp[npp$term == "Coarse Root NPP", i+1])
        
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
        
        # CH4
        
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
        pool[pool$term == "Overstorey leaf", i+1] <- mean(leaf_c_pool[leaf_c_pool$Ring == i, "leaf_pool"])
        
        # Overstorey wood
        pool[pool$term == "Overstorey wood", i+1] <- mean(wood_c_pool[wood_c_pool$Ring == i, "wood_pool"])
        
        # Fine Root
        pool[pool$term == "Fine Root", i+1] <- mean(fineroot_c_pool[fineroot_c_pool$Ring == i, "fineroot_pool"])
        
        # Coarse Root
        pool[pool$term == "Coarse Root", i+1] <- mean(coarse_root_c_pool_1[coarse_root_c_pool_1$Ring == i, "coarse_root_pool"])
        
        # Understorey above-ground
        pool[pool$term == "Understorey above-ground", i+1] <- mean(understorey_aboveground_c_pool[understorey_aboveground_c_pool$Ring == i, 
                                                                                                  "Total_g_C_m2"])
        
        # Soil C
        pool[pool$term == "Soil C", i+1] <- mean(soil_c_pool[soil_c_pool$Ring == i, "soil_carbon_pool"])
        
        # Microbial biomass
        pool[pool$term == "Microbial biomass", i+1]  <- mean(microbial_c_pool[microbial_c_pool$Ring == i, "microbial_pool"])
        
        # Coarse Woody Debris
        pool[pool$term == "Coarse woody debris", i+1]  <- mean(standing_dead_c_pool[standing_dead_c_pool$Ring == i, "wood_pool"], na.rm=T)
        
        # Mycorrhizae
        
        # Insects
        
        # Litter
        
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
