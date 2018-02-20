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
    npp$start_year <- rep(NA, length(npp$term))
    npp$end_year <- rep(NA, length(npp$term))
    npp$timepoint <- rep(NA, length(npp$term))
    npp$data_notes <- rep("", length(npp$term))
    npp$processing_notes <- rep("", length(npp$term))

    ### leaf NPP
    litter_prod <- with(leaflitter_flux,sum(leaf_flux*ndays)/sum(ndays)) * conv
    npp$value[npp$term == "Leaf NPP"] <- litter_prod
    npp$start_year[npp$term == "Leaf NPP"] <- min(year(leaflitter_flux$Start_date))
    npp$end_year[npp$term == "Leaf NPP"] <- max(year(leaflitter_flux$End_date))
    npp$timepoint[npp$term == "Leaf NPP"] <- length(unique(leaflitter_flux$Date))
    npp$data_notes[npp$term == "Leaf NPP"] <- ""
    npp$processing_notes[npp$term == "Leaf NPP"] <- "Calculated from leaf litterfall only"
    
    ### stem NPP
    stem_prod <- with(wood_production_flux,sum(wood_production_flux*ndays)/sum(ndays)) * conv
    npp$value[npp$term == "Stem NPP"] <- stem_prod
    npp$start_year[npp$term == "Stem NPP"] <- min(year(wood_production_flux$Start_date))
    npp$end_year[npp$term == "Stem NPP"] <- max(year(wood_production_flux$End_date))
    npp$timepoint[npp$term == "Stem NPP"] <- length(unique(wood_production_flux$Date))
    npp$data_notes[npp$term == "Stem NPP"] <- ""
    npp$processing_notes[npp$term == "Stem NPP"] <- "Calculated from stem diameter + allometry. Includes all trees"
    
    ### root NPP
    froot_prod <- with(fineroot_production_flux,sum(fineroot_production_flux*ndays)/sum(ndays)) * conv
    npp$value[npp$term == "Fine Root NPP"] <- froot_prod
    npp$start_year[npp$term == "Fine Root NPP"] <- min(year(fineroot_production_flux$Start_date))
    npp$end_year[npp$term == "Fine Root NPP"] <- max(year(fineroot_production_flux$End_date))
    npp$timepoint[npp$term == "Fine Root NPP"] <- length(unique(fineroot_production_flux$Date))
    npp$data_notes[npp$term == "Fine Root NPP"] <- ""
    npp$processing_notes[npp$term == "Fine Root NPP"] <- "One year's data only"
    
    ### Coarse root NPP
    cr_prod <- with(coarse_root_production_flux_1,
                      sum(coarse_root_production_flux*ndays)/sum(ndays)) * conv
    npp$value[npp$term == "Coarse Root NPP"] <- cr_prod
    npp$start_year[npp$term == "Coarse Root NPP"] <- min(year(coarse_root_production_flux_1$Start_date))
    npp$end_year[npp$term == "Coarse Root NPP"] <- max(year(coarse_root_production_flux_1$End_date))
    npp$timepoint[npp$term == "Coarse Root NPP"] <- length(unique(coarse_root_production_flux_1$Date))
    npp$data_notes[npp$term == "Coarse Root NPP"] <- ""
    npp$processing_notes[npp$term == "Coarse Root NPP"] <- "Calculated from stem diameter + allometry. Includes all trees"
    
    ### frass production
    npp$value[npp$term == "Frass production"] <- with(frass_production_flux,
                                                      sum(frass_production_flux*ndays)/sum(ndays)) * conv
    npp$start_year[npp$term == "Frass production"] <- min(year(frass_production_flux$Start_date))
    npp$end_year[npp$term == "Frass production"] <- max(year(frass_production_flux$End_date))
    npp$timepoint[npp$term == "Frass production"] <- length(unique(frass_production_flux$Date))
    npp$data_notes[npp$term == "Frass production"] <- ""
    npp$processing_notes[npp$term == "Frass production"] <- ""
    
    ### Rh
    npp$value[npp$term == "R hetero"] <- mean(heterotrophic_respiration_flux$heterotrophic_respiration_flux) * conv
    npp$start_year[npp$term == "R hetero"] <- min(year(heterotrophic_respiration_flux$Start_date))
    npp$end_year[npp$term == "R hetero"] <- max(year(heterotrophic_respiration_flux$End_date))
    npp$timepoint[npp$term == "R hetero"] <- length(unique(heterotrophic_respiration_flux$Date))
    npp$data_notes[npp$term == "R hetero"] <- ""
    npp$processing_notes[npp$term == "R hetero"] <- "Temperature-dependent function derived from WTC3"
        
    ### Understorey NPP
    und_prod <- with(understorey_aboveground_production_flux,
                     sum(understorey_production_flux*ndays)/sum(ndays)) * conv
    npp$value[npp$term == "Understorey NPP"] <- und_prod
    npp$start_year[npp$term == "Understorey NPP"] <- min(year(understorey_aboveground_production_flux$Start_date))
    npp$end_year[npp$term == "Understorey NPP"] <- max(year(understorey_aboveground_production_flux$End_date))
    npp$timepoint[npp$term == "Understorey NPP"] <- length(unique(understorey_aboveground_production_flux$Date))
    npp$data_notes[npp$term == "Understorey NPP"] <- ""
    npp$processing_notes[npp$term == "Understorey NPP"] <- "Harvest data"    
    
    ### Mycorrhizal production
    npp$data_notes[npp$term == "Mycorrhizal production"] <- "Need data from Jeff"
    npp$processing_notes[npp$term == "Mycorrhizal production"] <- ""  
    
    ### Flower production
    npp$data_notes[npp$term == "Flower production"] <- "No data yet"
    npp$processing_notes[npp$term == "Flower production"] <- "" 
    
    
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
    inout$start_year <- rep(NA, length(inout$term))
    inout$end_year <- rep(NA, length(inout$term))
    inout$timepoint <- rep(NA, length(inout$term))
    inout$data_notes <- rep("", length(inout$term))
    inout$processing_notes <- rep("", length(inout$term))
    
    
    ### GPP 
    inout$value[inout$term == "GPP overstorey"] <- mean(overstorey_gpp_flux$GPP) 
    inout$start_year[inout$term == "GPP overstorey"] <- min(overstorey_gpp_flux$year)
    inout$end_year[inout$term == "GPP overstorey"] <- max(overstorey_gpp_flux$year)
    inout$timepoint[inout$term == "GPP overstorey"] <- length(unique(overstorey_gpp_flux$year))
    inout$data_notes[inout$term == "GPP overstorey"] <- ""
    inout$processing_notes[inout$term == "GPP overstorey"] <- "MAESPA annual output"
    
    ### Ra leaf
    inout$value[inout$term == "Ra leaf"] <- mean(overstorey_leaf_respiration_flux$Rfoliage) 
    inout$start_year[inout$term == "Ra leaf"] <- min(overstorey_leaf_respiration_flux$year)
    inout$end_year[inout$term == "Ra leaf"] <- max(overstorey_leaf_respiration_flux$year)
    inout$timepoint[inout$term == "Ra leaf"] <- length(unique(overstorey_leaf_respiration_flux$year))
    inout$data_notes[inout$term == "Ra leaf"] <- ""
    inout$processing_notes[inout$term == "Ra leaf"] <- "MAESPA annual output"
    
    ### GPP understorey
    inout$value[inout$term == "GPP understorey"] <- mean(understorey_gpp_flux$GPP) 
    inout$start_year[inout$term == "GPP understorey"] <- min(understorey_gpp_flux$year)
    inout$end_year[inout$term == "GPP understorey"] <- max(understorey_gpp_flux$year)
    inout$timepoint[inout$term == "GPP understorey"] <- length(unique(understorey_gpp_flux$year))
    inout$data_notes[inout$term == "GPP understorey"] <- ""
    inout$processing_notes[inout$term == "GPP understorey"] <- "40% of overstorey GPP"
    
    
    ### Ra stem
    inout$data_notes[inout$term == "Ra stem"] <- ""
    inout$processing_notes[inout$term == "Ra stem"] <- ""
    
    
    ### Ra fine root
    inout$value[inout$term == "Ra root"] <- mean(root_respiration_flux$root_respiration_flux) * conv
    inout$start_year[inout$term == "Ra root"] <- min(year(root_respiration_flux$Start_date))
    inout$end_year[inout$term == "Ra root"] <- max(year(root_respiration_flux$End_date))
    inout$timepoint[inout$term == "Ra root"] <- length(unique(root_respiration_flux$Date))
    inout$data_notes[inout$term == "Ra root"] <- ""
    inout$processing_notes[inout$term == "Ra root"] <- "Only fineroot respiration"
    
    ### Ra understorey
    inout$value[inout$term == "Ra understorey"] <- with(understorey_respiration_flux,
                                                        sum(respiration*ndays)/sum(ndays)) * conv
    inout$start_year[inout$term == "Ra understorey"] <- min(year(understorey_respiration_flux$Start_date))
    inout$end_year[inout$term == "Ra understorey"] <- max(year(understorey_respiration_flux$End_date))
    inout$timepoint[inout$term == "Ra understorey"] <- length(unique(understorey_respiration_flux$Date))
    inout$data_notes[inout$term == "Ra understorey"] <- ""
    inout$processing_notes[inout$term == "Ra understorey"] <- "Used fixed CUE approach"
    

    # Rsoil
    inout$value[inout$term == "Rsoil"] <- mean(soil_respiration_flux$soil_respiration_flux) * conv
    inout$start_year[inout$term == "Rsoil"] <- min(year(soil_respiration_flux$Start_date))
    inout$end_year[inout$term == "Rsoil"] <- max(year(soil_respiration_flux$End_date))
    inout$timepoint[inout$term == "Rsoil"] <- length(unique(soil_respiration_flux$Date))
    inout$data_notes[inout$term == "Rsoil"] <- ""
    inout$processing_notes[inout$term == "Rsoil"] <- "Three years soil respiration data"
    
    # Rherbivore
    inout$value[inout$term == "Rherbivore"] <- with(herbivory_respiration_flux,
                                                    sum(respiration_flux*ndays)/sum(ndays)) * conv
    inout$start_year[inout$term == "Rherbivore"] <- min(year(herbivory_respiration_flux$Start_date))
    inout$end_year[inout$term == "Rherbivore"] <- max(year(herbivory_respiration_flux$End_date))
    inout$timepoint[inout$term == "Rherbivore"] <- length(unique(herbivory_respiration_flux$Date))
    inout$data_notes[inout$term == "Rherbivore"] <- ""
    inout$processing_notes[inout$term == "Rherbivore"] <- "Leaf consumption minus frass production"
    
    # Rgrowth
    inout$value[inout$term == "Rgrowth"] <- ccost * (litter_prod + stem_prod + froot_prod + cr_prod)
    inout$start_year[inout$term == "Rgrowth"] <- min(npp$start_year[npp$term == "Leaf NPP"],
                                                     npp$start_year[npp$term == "Stem NPP"],
                                                     npp$start_year[npp$term == "Fine Root NPP"],
                                                     npp$start_year[npp$term == "Coarse Root NPP"])
    inout$end_year[inout$term == "Rgrowth"] <- max(npp$end_year[npp$term == "Leaf NPP"],
                                                   npp$end_year[npp$term == "Stem NPP"],
                                                   npp$end_year[npp$term == "Fine Root NPP"],
                                                   npp$end_year[npp$term == "Coarse Root NPP"])
    inout$timepoint[inout$term == "Rgrowth"] <- min(npp$timepoint[npp$term == "Leaf NPP"],
                                                    npp$timepoint[npp$term == "Stem NPP"],
                                                    npp$timepoint[npp$term == "Fine Root NPP"],
                                                    npp$timepoint[npp$term == "Coarse Root NPP"])
    inout$data_notes[inout$term == "Rgrowth"] <- ""
    inout$processing_notes[inout$term == "Rgrowth"] <- "Calculated by multiplying NPP by 0.3"
    
    # DOC
    inout$value[inout$term == "DOC loss"] <- mean(doc_leaching_flux$doc_leaching_flux) * conv
    inout$start_year[inout$term == "DOC loss"] <- min(year(doc_leaching_flux$Start_date))
    inout$end_year[inout$term == "DOC loss"] <- max(year(doc_leaching_flux$End_date))
    inout$timepoint[inout$term == "DOC loss"] <- length(unique(doc_leaching_flux$Date))
    inout$data_notes[inout$term == "DOC loss"] <- ""
    inout$processing_notes[inout$term == "DOC loss"] <- "Deep soil layer depth"
    
    #CH4
    # inout$value[inout$term == "CH4 efflux"] <- mean(methane_c_flux$methane_flux) * conv
    inout$data_notes[inout$term == "CH4 efflux"] <- "Data on HIEv, but need to understand its calculations"
    inout$processing_notes[inout$term == "CH4 efflux"] <- ""
    
    # VOC
    inout$data_notes[inout$term == "VOC"] <- "Needs data from David"
    inout$processing_notes[inout$term == "VOC"] <- ""
    
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
    pool$start_year <- rep(NA, length(pool$term))
    pool$end_year <- rep(NA, length(pool$term))
    pool$timepoint <- rep(NA, length(pool$term))
    pool$data_notes <- rep("", length(pool$term))
    pool$processing_notes <- rep("", length(pool$term))
    
    ### Overstorey leaf
    pool$value[pool$term == "Overstorey leaf"] <- mean(leaf_c_pool$leaf_pool)
    pool$start_year[pool$term == "Overstorey leaf"] <- min(year(leaf_c_pool$Date))
    pool$end_year[pool$term == "Overstorey leaf"] <- max(year(leaf_c_pool$Date))
    pool$timepoint[pool$term == "Overstorey leaf"] <- length(unique(leaf_c_pool$Date))
    pool$data_notes[pool$term == "Overstorey leaf"] <- ""
    pool$processing_notes[pool$term == "Overstorey leaf"] <- "Calculated from plant area index using constant SLA"
    
    ### Overstorey wood
    pool$value[pool$term == "Overstorey wood"] <- mean(wood_c_pool$wood_pool)
    pool$start_year[pool$term == "Overstorey wood"] <- min(year(wood_c_pool$Date))
    pool$end_year[pool$term == "Overstorey wood"] <- max(year(wood_c_pool$Date))
    pool$timepoint[pool$term == "Overstorey wood"] <- length(unique(wood_c_pool$Date))
    pool$data_notes[pool$term == "Overstorey wood"] <- "Year 2011-12 data, David to upload"
    pool$processing_notes[pool$term == "Overstorey wood"] <- "scaled with height"
    
    ### Understorey aboveground
    pool$value[pool$term == "Understorey above-ground"] <- mean(understorey_aboveground_c_pool$Total_g_C_m2)
    pool$start_year[pool$term == "Understorey above-ground"] <- min(year(understorey_aboveground_c_pool$Date))
    pool$end_year[pool$term == "Understorey above-ground"] <- max(year(understorey_aboveground_c_pool$Date))
    pool$timepoint[pool$term == "Understorey above-ground"] <- length(unique(understorey_aboveground_c_pool$Date))
    pool$data_notes[pool$term == "Understorey above-ground"] <- "Matthias' recent harvest data not on HIEv"
    pool$processing_notes[pool$term == "Understorey above-ground"] <- "Based on harvesting data"
    
    ### Fine root
    pool$value[pool$term == "Fine Root"] <- mean(fineroot_c_pool$fineroot_pool)
    pool$start_year[pool$term == "Fine Root"] <- min(year(fineroot_c_pool$Date))
    pool$end_year[pool$term == "Fine Root"] <- max(year(fineroot_c_pool$Date))
    pool$timepoint[pool$term == "Fine Root"] <- length(unique(fineroot_c_pool$Date))
    pool$data_notes[pool$term == "Fine Root"] <- ""
    pool$processing_notes[pool$term == "Understorey above-ground"] <- ""
    
    ### Coarse root
    pool$value[pool$term == "Coarse Root"] <- mean(coarse_root_c_pool_1$coarse_root_pool)
    pool$start_year[pool$term == "Coarse Root"] <- min(year(coarse_root_c_pool_1$Date))
    pool$end_year[pool$term == "Coarse Root"] <- max(year(coarse_root_c_pool_1$Date))
    pool$timepoint[pool$term == "Coarse Root"] <- length(unique(coarse_root_c_pool_1$Date))
    pool$data_notes[pool$term == "Coarse Root"] <- ""
    pool$processing_notes[pool$term == "Coarse Root"] <- "Allometric relationship with DBH"
    
    ### Soil C
    pool$value[pool$term == "Soil C"] <- mean(soil_c_pool$soil_carbon_pool)
    pool$start_year[pool$term == "Soil C"] <- min(year(soil_c_pool$Date))
    pool$end_year[pool$term == "Soil C"] <- max(year(soil_c_pool$Date))
    pool$timepoint[pool$term == "Soil C"] <- length(unique(soil_c_pool$Date))
    pool$data_notes[pool$term == "Soil C"] <- ""
    pool$processing_notes[pool$term == "Soil C"] <- "For all depths (0 - 30 cm)"
    
    ### microbial pool
    pool$value[pool$term == "Microbial biomass"]  <- mean(microbial_c_pool$Cmic_g_m2)
    pool$start_year[pool$term == "Microbial biomass"] <- min(year(microbial_c_pool$date))
    pool$end_year[pool$term == "Microbial biomass"] <- max(year(microbial_c_pool$date))
    pool$timepoint[pool$term == "Microbial biomass"] <- length(unique(microbial_c_pool$date))
    pool$data_notes[pool$term == "Microbial biomass"] <- ""
    pool$processing_notes[pool$term == "Microbial biomass"]  <- "For 0 - 10 cm depth"
        
    ### Mycorrhizae
    pool$value[pool$term == "Mycorrhizae"]  <- 0.0
    pool$data_notes[pool$term == "Mycorrhizae"] <- "Waiting for data Jeff Power"
    pool$processing_notes[pool$term == "Mycorrhizae"]  <- "Assume 0 for now"
    
    ### Insects
    pool$value[pool$term == "Insects"]  <- 0.0
    pool$data_notes[pool$term == "Insects"] <- "No data"
    pool$processing_notes[pool$term == "Insects"]  <- "Assume it to be 0"
    
    ### CWD or standing dead
    pool$value[pool$term == "Coarse woody debris"]  <- mean(standing_dead_c_pool$wood_pool)
    pool$start_year[pool$term == "Coarse woody debris"] <- min(year(standing_dead_c_pool$Date))
    pool$end_year[pool$term == "Coarse woody debris"] <- max(year(standing_dead_c_pool$Date))
    pool$timepoint[pool$term == "Coarse woody debris"] <- length(unique(standing_dead_c_pool$Date))
    pool$data_notes[pool$term == "Coarse woody debris"] <- ""
    pool$processing_notes[pool$term == "Coarse woody debris"]  <- "Taken from the standing dead pool"
    
    ### Litter
    pool$data_notes[pool$term == "Litter"] <- ""
    pool$processing_notes[pool$term == "Litter"]  <- "Assume 1 - %live"
    
    ##### output tables
    return(list(inout = data.table(inout), 
                npp = data.table(npp), 
                pool = data.table(pool)))
    
}
