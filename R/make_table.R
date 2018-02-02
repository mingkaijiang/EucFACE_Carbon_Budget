#### To make EucFACE table
#### Ignore time and ring variability
make_EucFACE_table <- function() {
    
    
    ##############################################
    #### Method 3
    #### NPP fluxes (Method 3 of getting NEP)
    ##############################################
    #### Define dataframe
    term <- c("Leaf NPP", "Stem NPP", "Fine Root NPP", 
              "Coarse Root NPP", "Understorey NPP",
              "Frass production", "R hetero", 
              "Mycorrhizal production", "Flower production")
    
    npp <- data.frame(term)
    npp$value <- rep(NA, length(npp$term))
    npp$notes <- rep("", length(npp$term))
    
    ### prepare the ndays in all flux calculations
    leaflitter_flux$days <- as.numeric(with(leaflitter_flux,End_date - Start_date))
    frass_production_flux$days <- as.numeric(with(frass_production_flux,End_date - Start_date))
    fineroot_production_flux$days <- as.numeric(with(fineroot_production_flux,End_date - Start_date))
    understorey_aboveground_production_flux$days <- as.numeric(with(understorey_aboveground_production_flux,
                                                                    End_date - Start_date))
    wood_production_flux$days <- as.numeric(with(wood_production_flux,End_date - Start_date))
    coarse_root_production_flux_1$days <- as.numeric(with(coarse_root_production_flux_1,
                                                          End_date - Start_date))
    
    ### leaf NPP
    litter_prod <- with(leaflitter_flux,sum(leaf_flux*days)/sum(days)) * conv
    npp$value[npp$term == "Leaf NPP"] <- litter_prod
    npp$notes[npp$term == "Leaf NPP"] <- "Calculated from leaf litterfall only"
    
    ### stem NPP
    stem_prod <- with(wood_production_flux,sum(wood_production_flux*days)/sum(days)) * conv
    npp$value[npp$term == "Stem NPP"] <- stem_prod
    npp$notes[npp$term == "Stem NPP"] <- "Calculated from stem diameter + allometry. Includes all trees"
    
    ### root NPP
    froot_prod <- with(fineroot_production_flux,sum(fineroot_production_flux*days)/sum(days)) * conv
    npp$value[npp$term == "Fine Root NPP"] <- froot_prod
    npp$notes[npp$term == "Fine Root NPP"] <- "One year's data only"
    
    ### Coarse root NPP
    cr_prod <- with(coarse_root_production_flux_1,
                      sum(coarse_root_production_flux*days)/sum(days)) * conv
    npp$value[npp$term == "Coarse Root NPP"] <- cr_prod
    npp$notes[npp$term == "Coarse Root NPP"] <- "Calculated from stem diameter + allometry. Includes all trees"
    
    ### frass production
    npp$value[npp$term == "Frass production"] <- with(frass_production_flux,
                                                      sum(frass_production_flux*days)/sum(days)) * conv
    
    ### Rh
    npp$value[npp$term == "R hetero"] <- mean(heterotrophic_respiration_flux$heterotrophic_respiration_flux) * conv
    npp$notes[npp$term == "R hetero"] <- "Temperature-dependent function derived from WTC3"
        
    ### Understorey NPP
    und_prod <- with(understorey_aboveground_production_flux,
                     sum(understorey_production_flux*days)/sum(days)) * conv
    npp$value[npp$term == "Understorey NPP"] <- und_prod
    npp$notes[npp$term == "Understorey NPP"] <- "Harvest data"    
        
    ##############################################
    #### Method 1
    #### In / out fluxes (Method 1 of getting NEP)
    ##############################################
    ### define terms and dataframe
    term <- c("GPP overstorey", "GPP understorey", "CH4 efflux",
              "Ra leaf", "Ra stem", "Ra root", "Ra understorey", "VOC",
              "Rherbivore", "DOC loss", "Rsoil", "Rgrowth")
    inout <- data.frame(term)
    inout$value <- rep(NA, length(inout$term))
    inout$notes <- rep("", length(inout$term))
    
    herbivory_respiration_flux$days <- as.numeric(with(herbivory_respiration_flux,End_date - Start_date))
    
    
    ### GPP 
    maespa <- read.csv("data/2013_maespa_gpp_respiration.csv")
    inout$value[inout$term == "GPP overstorey"] <- mean(maespa$GPP.mg.m2.d) * conv
    inout$notes[inout$term == "GPP overstorey"] <- "MAESPA output - still working on parameterisation"
    
    ### Ra leaf
    inout$value[inout$term == "Ra leaf"] <- mean(maespa$Respiration.mg.m2.d) * conv
    inout$notes[inout$term == "Ra leaf"] <- "MAESPA output - still working on parameterisation"
    
    ### Ra stem
    
    ### Ra fine root
    inout$value[inout$term == "Ra root"] <- mean(root_respiration_flux$root_respiration_flux) * conv
    
    ### Ra understorey
    inout$value[inout$term == "Ra understorey"] <- with(understorey_respiration_flux,
                                                        sum(respiration*days)/sum(days)) * conv
    inout$notes[inout$term == "Ra understorey"] <- "Used one fixed Rd value, huge uncertainty"
    

    # Rsoil
    inout$value[inout$term == "Rsoil"] <- mean(soil_respiration_flux$soil_respiration_flux) * conv
    inout$notes[inout$term == "Rsoil"] <- "Three years soil respiration data"
    
    # Rherbivore
    inout$value[inout$term == "Rherbivore"] <- with(herbivory_respiration_flux,
                                                    sum(respiration_flux*days)/sum(days)) * conv
    inout$notes[inout$term == "Rherbivore"] <- "Leaf consumption minus frass production"
    
    # Rgrowth
    inout$value[inout$term == "Rgrowth"] <- ccost * (litter_prod + stem_prod + froot_prod)
    inout$notes[inout$term == "Rgrowth"] <- "Calculated by multiplying NPP by 0.3"
    
    # DOC
    inout$value[inout$term == "DOC loss"] <- mean(doc_leaching_flux$doc_leaching_flux) * conv
    inout$notes[inout$term == "DOC loss"] <- "Deep soil layer depth"
    
    #CH4
    # inout$value[inout$term == "CH4 efflux"] <- mean(methane_c_flux$methane_flux) * conv
    
    ##############################################
    #### Method 2
    #### Standing C pools
    ##############################################    
    ### Define terms and dataframe
    term <- c("Overstorey leaf", "Overstorey wood", "Understorey above-ground",
              "Fine Root", "Coarse Root", "Litter", "Coarse woody debris", 
              "Microbial biomass", "Soil C", "Mycorrhizae", "Insects")
    pool <- data.frame(term)
    pool$value <- rep(NA, length(pool$term))
    pool$notes <- rep("", length(pool$term))
    
    ### Overstorey leaf
    pool$value[pool$term == "Overstorey leaf"] <- mean(leaf_c_pool$leaf_pool)
    pool$notes[pool$term == "Overstorey leaf"] <- "Calculated from plant area index using constant SLA"
    
    ### Overstorey wood
    pool$value[pool$term == "Overstorey wood"] <- mean(wood_c_pool$wood_pool)
    
    ### Understorey aboveground
    pool$value[pool$term == "Understorey above-ground"] <- mean(understorey_aboveground_c_pool$Total_g_C_m2)
    pool$notes[pool$term == "Understorey above-ground"] <- "Based on harvesting data"
    
    ### Fine root
    pool$value[pool$term == "Fine Root"] <- mean(fineroot_c_pool$fineroot_pool)
    
    ### Coarse root
    pool$value[pool$term == "Coarse Root"] <- mean(coarse_root_c_pool_1$coarse_root_pool)
    pool$notes[pool$term == "Coarse Root"] <- "Allometric relationship with DBH"
    
    ### Soil C
    pool$value[pool$term == "Soil C"] <- mean(soil_c_pool$soil_carbon_pool)
    pool$notes[pool$term == "Soil C"] <- "For all depths"
    
    ### microbial pool
    pool$value[pool$term == "Microbial biomass"]  <- mean(microbial_c_pool$Cmic_g_m2)
    pool$notes[pool$term == "Microbial biomass"]  <- "For 0 - 10 cm depth"
        
        
        
    ##### output tables
    return(list(inout = data.table(inout), 
                npp = data.table(npp), 
                pool = data.table(pool)))
    
}
