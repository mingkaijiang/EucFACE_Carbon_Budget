#### To make EucFACE table by year
#### Ignore ring variability
make_EucFACE_table_by_year <- function() {
    
    yr.list <- c(2012:2017)
    
    ##############################################
    #### Method 3
    #### NPP fluxes (Method 3 of getting NEP)
    ##############################################
    #### Define dataframe
    term <- c("LeafNPP", "StemNPP", "UnderstoreyNPP",
              "FineRootNPP", "FrassProduction", "RHetero", "MycorrhizalProduction",
              "FlowerProduction")
    
    npp <- matrix(nrow=length(yr.list)+1, ncol=length(term)+1)
    colnames(npp) <- c("year", term)
    npp <- as.data.frame(npp)
    npp$year <- c(yr.list, "notes")
    
    
    ### leaf NPP
    leaflitter_flux$total <- with(leaflitter_flux, twig_flux + bark_flux + seed_flux + leaf_flux)
    leaflitter_flux$days <- as.numeric(with(leaflitter_flux,End_date - Start_date))
    leaflitter_flux$year <- year(leaflitter_flux$Date)
    
    for (i in yr.list) {
        litter_prod <- with(leaflitter_flux[leaflitter_flux$year == i, ],sum(total*days, na.rm=T)/sum(days, na.rm=T)) * conv 
        npp$LeafNPP[npp$year == i] <- round(litter_prod,2)

    }
    npp$LeafNPP[npp$year == "notes"] <- "Calculated from leaf, twig, bark and seed litterfall"
    
    ### stem NPP
    wood_production_flux$year <- year(wood_production_flux$Start_date)
    for(i in yr.list) {
        stem_prod <- mean(wood_production_flux[wood_production_flux$year == i, "wood_production_flux"], na.rm=T) * conv
        npp$StemNPP[npp$year == i] <- round(stem_prod,2)
    }
    npp$StemNPP[npp$year == "notes"] <- "Calculated from stem diameter + allometry. Includes all trees"
    
    ### root NPP
    fineroot_production_flux$year <- year(fineroot_production_flux$Date)
    for(i in yr.list) {
        froot_prod <- mean(fineroot_production_flux[fineroot_production_flux$year == i, "fineroot_production_flux"]) * conv
        npp$FineRootNPP[npp$year == i] <- round(froot_prod,2)
    }
    npp$FineRootNPP[npp$year == "notes"] <- "One year's data only"

    ### frass production
    frass_production_flux$year <- year(frass_production_flux$Start_date)
    for(i in yr.list) {
        npp$FrassProduction[npp$year == i] <- mean(frass_production_flux[frass_production_flux$year == i, "frass_production_flux"]) * conv
        
    }

    ### Rh
    heterotrophic_respiration_flux$year <- year(heterotrophic_respiration_flux$Start_date)
    for(i in yr.list) {
        npp$RHetero[npp$year == i] <- round(mean(heterotrophic_respiration_flux[heterotrophic_respiration_flux$year == i, "heterotrophic_respiration_flux"]) * conv,2)
        
    }
    npp$RHetero[npp$year == "notes"] <- "Temperature-dependent function derived from WTC3"

    #npp <- data.frame(lapply(npp, function(x) {
    #    gsub("NaN", "NA", x)
    #}))
    
        
    ##############################################
    #### Method 1
    #### In / out fluxes (Method 1 of getting NEP)
    ##############################################
    ### define terms and dataframe
    term <- c("GPPoverstorey", "GPPunderstorey", "CH4uptake",
              "RaLeaf", "RaStem", "RaUnderstorey", "VOC",
              "Rherbivore", "DOCloss", "Rsoil", "Rgrowth")
    
    inout <- matrix(nrow=length(yr.list)+1, ncol=length(term)+1)
    colnames(inout) <- c("year", term)
    inout <- as.data.frame(inout)
    inout$year <- c(yr.list, "notes")
    
    ### GPP 
    maespa <- read.csv("data/2013_maespa_gpp_respiration.csv")
    inout$GPPoverstorey[inout$year == "2013"] <- round(mean(maespa$GPP.mg.m2.d) * conv, 2)
    inout$GPPoverstorey[inout$year == "notes"] <- "MAESPA output - still working on parameterisation"
    
    ### Ra leaf
    inout$RaLeaf[inout$year == "2013"] <- round(mean(maespa$Respiration.mg.m2.d) * conv, 2)
    inout$RaLeaf[inout$year == "notes"] <- "MAESPA output - still working on parameterisation"
    
    # Rsoil
    soil_respiration_flux$year <- year(soil_respiration_flux$Start_date)
    for(i in yr.list) {
        inout$Rsoil[inout$year == i] <- round(mean(soil_respiration_flux[soil_respiration_flux$year == i, "soil_respiration_flux"]) * conv, 2)
        
    }
    inout$Rsoil[inout$year == "notes"] <- "Three years soil respiration data"
    
    # Rherbivore
    herbivory_leaf_consumption_flux$year <- year(herbivory_leaf_consumption_flux$Start_date)
    for (i in yr.list) {
        inout$Rherbivore[inout$year == i] <- round(mean(herbivory_leaf_consumption_flux[herbivory_leaf_consumption_flux$year == i, 
                                                                                        "herbivory_leaf_consumption_flux"]) - 
            mean(frass_production_flux[frass_production_flux$year == i, "frass_production_flux"]) * conv, 2)
    }
    inout$Rherbivore[inout$year == "notes"] <- "Leaf consumption minus frass production"
    
    # Rgrowth
    for (i in yr.list) {
        inout$Rgrowth[inout$year == i] <- ccost * (as.numeric(npp[npp$year == i, "LeafNPP"]) + as.numeric(npp[npp$year == i, "StemNPP"]) + as.numeric(npp[npp$year == i, "FineRootNPP"]))
    }
    inout$Rgrowth[inout$year == "notes"] <- "Calculated by multiplying NPP by 0.3"
    
    # DOC
    doc_leaching_flux$year <- year(doc_leaching_flux$Start_date)
    for (i in yr.list) {
        inout$DOCloss[inout$year == i] <- round(mean(doc_leaching_flux[doc_leaching_flux$year == i, "doc_leaching_flux"]) * conv, 2)
        
    }
    inout$DOCloss[inout$year == "notes"] <- "Deep soil layer depth"
    
    #CH4
    methane_flux$year <- year(methane_flux$Date)
    for (i in yr.list) {
        inout$CH4uptake[inout$year == i] <- round(mean(methane_flux[methane_flux$year == i, "methane_flux"]) * conv, 2)
        
    }

    ##############################################
    #### Method 2
    #### Standing C pools
    ##############################################    
    ### Define terms and dataframe
    term <- c("OverstoreyLeaf", "OverstoreyWood", "UnderstoreyAboveground",
              "FineRoot", "CoarseRoot", "Litter", "CoarseWoodyDebris", 
              "MicrobialBiomass", "SoilC", "Mycorrhizae", "Insects")
    
    pool <- matrix(nrow=length(yr.list)+1, ncol=length(term)+1)
    colnames(pool) <- c("year", term)
    pool <- as.data.frame(pool)
    pool$year <- c(yr.list, "notes")
    
    
    ### Overstorey leaf
    leaf_pool$year <- year(leaf_pool$Date)
    for (i in yr.list) {
        pool$OverstoreyLeaf[pool$year == i] <- round(mean(leaf_pool[leaf_pool$year == i, "leaf_pool"]), 2)
        
    }
    pool$OverstoreyLeaf[pool$year == "notes"] <- "Calculated from plant area index using constant SLA"
    
    ### Overstorey wood
    wood_pool$year <- year(wood_pool$Date) 
    for (i in yr.list) {
        pool$OverstoreyWood[pool$year == i] <- round(mean(wood_pool[wood_pool$year == i, "wood_pool"]), 2)
        
    }

    ### Understorey aboveground
    understorey_aboveground_biomass_pool$year <- year(understorey_aboveground_biomass_pool$Date)
    for (i in yr.list) {
        pool$UnderstoreyAboveground[pool$year == i] <- round(mean(understorey_aboveground_biomass_pool[understorey_aboveground_biomass_pool$year == i, "Total_g_C_m2"]), 2)
        
    }
    pool$UnderstoreyAboveground[pool$year == "notes"] <- "Based on harvesting data"
    
    ### Fine root
    fineroot_pool$year <- year(fineroot_pool$Date) 
    for (i in yr.list) {
        pool$FineRoot[pool$year == i] <- round(mean(fineroot_pool[fineroot_pool$year == i, "fineroot_pool"]), 2)
        
    }

    ### Soil C
    soil_carbon_pool$year <- year(soil_carbon_pool$Date)
    for (i in yr.list) {
        pool$SoilC[pool$year == i] <- round(mean(soil_carbon_pool[soil_carbon_pool$year == i, "soil_carbon_pool"]), 2)
        
    }
    pool$SoilC[pool$year == "notes"] <- "For all depths"
    
    ### microbial pool
    microbial_pool$year <- year(microbial_pool$date)
    for (i in yr.list) {
        pool$MicrobialBiomass[pool$year == i]  <- round(mean(microbial_pool[microbial_pool$year == i, "Cmic_g_m2"]), 2)
        
    }
    pool$MicrobialBiomass[pool$year == "notes"]  <- "For 0 - 10 cm depth"
        
        
        
    ##### output tables
    return(list(inout = data.table(inout), 
                npp = data.table(npp), 
                pool = data.table(pool)))
    
}
