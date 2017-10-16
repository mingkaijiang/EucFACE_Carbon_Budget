make_table_by_ring <- function() {
    #### Generate ring-specific summary table
    #### ignoring time 
    
    ##############################################
    #### Method 3
    #### NPP fluxes (Method 3 of getting NEP)
    ##############################################
    ### set up dataframe
    term <- c("Leaf NPP", "Stem NPP", "Understorey NPP",
              "Fine Root NPP", "Frass production", "R hetero", "Mycorrhizal production",
              "Flower production")
    npp <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(npp) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2", "notes")
    Ring <- c(1:6)
    
    
    # leaflitter_flux$twig_flux[leaflitter_flux$twig_flux > 5000] <- 0   # v high twig fluxes - maybe leave in? 
    leaflitter_flux$total <- with(leaflitter_flux, twig_flux + bark_flux + seed_flux + leaf_flux)
    leaflitter_flux$days <- as.numeric(with(leaflitter_flux,End_date - Start_date))
    
    litter_prod <- data.frame(Ring)
    litter_prod$value <- rep(NA)
    stem_prod <- litter_prod
    froot_prod <- litter_prod
    frass_prod <- litter_prod
    heter_resp <- litter_prod
    
    frass_production_flux$days <- as.numeric(with(frass_production_flux,End_date - Start_date))
    
    for (i in Ring) {
        litter_prod[litter_prod$Ring == i, "value"] <- with(leaflitter_flux[leaflitter_flux$Ring ==i,],sum(total*days)/sum(days)) * conv
        stem_prod[stem_prod$Ring == i, "value"] <- mean(wood_production_flux[wood_production_flux$Ring == i, "wood_production_flux"]) * conv 
        froot_prod[froot_prod$Ring == i, "value"] <- mean(fineroot_production_flux[fineroot_production_flux$Ring == i, 
                                                                                   "fineroot_production_flux"]) * conv 
        frass_prod[frass_prod$Ring == i, "value"] <-with(frass_production_flux[frass_production_flux$Ring == i,],
                                                         sum(frass_production_flux*days)/sum(days)) * conv 
        heter_resp[heter_resp$Ring == i, "value"] <- mean(heterotrophic_respiration_flux[heterotrophic_respiration_flux$Ring == i, 
                                                                                   "heterotrophic_respiration_flux"]) * conv 
    }

    npp[npp$term == "Leaf NPP", 2:7] <- litter_prod$value
    npp$notes[npp$term == "Leaf NPP"] <- "Calculated from leaf, twig, bark and seed litterfall"
    
    npp[npp$term == "Stem NPP", 2:7] <- stem_prod$value
    npp$notes[npp$term == "Stem NPP"] <- "Calculated from stem diameter + allometry. Includes all trees"
    
    npp[npp$term == "Fine Root NPP", 2:7] <- froot_prod$value
    npp$notes[npp$term == "Fine Root NPP"] <- "One year's data only"
    
    npp[npp$term == "Frass production", 2:7] <- frass_prod$value
    
    ### Rh
    npp[npp$term == "R hetero", 2:7] <- heter_resp$value
    npp$notes[npp$term == "R hetero"] <- "Temperature-dependent function derived from WTC3"
    
    
    
    ##############################################
    #### Method 1
    #### In / out fluxes (Method 1 of getting NEP)
    ##############################################
    ### define terms and dataframe
    term <- c("GPP overstorey", "GPP understorey", "CH4 uptake",
              "Ra leaf", "Ra stem", "Ra understorey", "VOC",
              "Rherbivore", "DOC loss", "Rsoil", "Rgrowth")
    inout <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(inout) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2", "notes")
    
    maespa <- read.csv("data/2013_maespa_gpp_respiration.csv")
    
    for (i in Ring) {
        inout[inout$term == "GPP overstorey", i+1] <- maespa[maespa$Ring==i, "GPP.mg.m2.d"] * conv
        inout[inout$term == "Ra leaf", i+1] <- maespa[maespa$Ring==i, "Respiration.mg.m2.d"] * conv
        inout[inout$term == "Rsoil", i+1] <- mean(soil_respiration_flux[soil_respiration_flux$Ring == i, "soil_respiration_flux"]) * conv
        inout[inout$term == "Rgrowth", i+1] <- ccost * (litter_prod[i,"value"] + stem_prod[i,"value"] + froot_prod[i,"value"])
        inout[inout$term == "Rherbivore", i+1] <- mean(herbivory_leaf_consumption_flux[herbivory_leaf_consumption_flux$Ring == i, 
                                                                                       "herbivory_leaf_consumption_flux"] - 
                                                           frass_production_flux[frass_production_flux$Ring == i, "frass_production_flux"])  * conv
        inout[inout$term == "DOC loss", i+1] <- mean(doc_leaching_flux[doc_leaching_flux$Ring == i, "doc_leaching_flux"]) * conv
        inout[inout$term == "CH4 uptake", i+1] <- mean(methane_flux[methane_flux$Ring, "methane_flux"]) * conv
        
    }

    inout$notes[inout$term == "GPP overstorey"] <- "MAESPA output - still working on parameterisation"
    inout$notes[inout$term == "Ra leaf"] <- "MAESPA output - still working on parameterisation"
    inout$notes[inout$term == "Rsoil"] <- "Three years soil respiration data"
    inout$notes[inout$term == "Rgrowth"] <- "Calculated by multiplying NPP by 0.3"
    inout$notes[inout$term == "Rherbivore"] <- "Leaf consumption minus frass production"
    inout$notes[inout$term == "DOC loss"] <- "Deep soil layer depth"
    

    
    ##############################################
    #### Method 2
    #### Standing C pools
    ##############################################    
    ### Define terms and dataframe
    term <- c("Overstorey leaf", "Overstorey wood", "Understorey above-ground",
              "Fine Root", "Coarse Root", "Litter", "Coarse woody debris", 
              "Microbial biomass", "Soil C", "Mycorrhizae", "Insects")
    pool <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(pool) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2", "notes")

    for (i in Ring) {
        pool[pool$term == "Overstorey leaf", i+1] <- mean(leaf_pool[leaf_pool$Ring == i, "leaf_pool"])
        pool[pool$term == "Overstorey wood", i+1] <- mean(wood_pool[wood_pool$Ring == i, "wood_pool"])
        pool[pool$term == "Fine Root", i+1] <- mean(fineroot_pool[fineroot_pool$Ring == i, "fineroot_pool"])
        pool[pool$term == "Soil C", i+1] <- mean(soil_carbon_pool[soil_carbon_pool$Ring == i, "soil_carbon_pool"])
        pool[pool$term == "Microbial biomass", i+1]  <- mean(microbial_pool[microbial_pool$ring == i, "Cmic_g_m2"])
        
    }

    pool$notes[pool$term == "Overstorey leaf"] <- "Calculated from plant area index using constant SLA"
    pool$notes[pool$term == "Microbial biomass"]  <- "For 0 - 10 cm depth"
    
    
    ###### calculate aCO2 and eCO2 results
    inout$aCO2 <- rowMeans(subset(inout, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    inout$eCO2 <- rowMeans(subset(inout, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    npp$aCO2 <- rowMeans(subset(npp, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    npp$eCO2 <- rowMeans(subset(npp, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    pool$aCO2 <- rowMeans(subset(pool, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    pool$eCO2 <- rowMeans(subset(pool, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    ##### output tables
    return(list(inout = data.table(inout), 
                npp = data.table(npp), 
                pool = data.table(pool)))
}
