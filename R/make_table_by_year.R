#### To make EucFACE table by year
#### Ignore ring variability
make_EucFACE_table_by_year <- function() {
    
    yr.list <- c(2012:2017)
    
    ##############################################
    #### Method 3
    #### NPP fluxes (Method 3 of getting NEP)
    ##############################################
    #### Define dataframe
    term <- c("LeafNPP", "StemNPP", "FineRootNPP", "CoarseRootNPP",
              "OtherNPP", "UnderstoreyNPP", "FrassProduction", "LeafConsumption",
              "RHetero", "MycorrhizalProduction",
              "FlowerProduction")
    
    npp <- matrix(nrow=length(yr.list), ncol=length(term)+1)
    colnames(npp) <- c("year", term)
    npp <- as.data.frame(npp)
    npp$year <- yr.list
    
    
    ### leaf NPP
    leaflitter_flux$year <- year(leaflitter_flux$Date)
    for (i in yr.list) {
        litter_prod <- with(leaflitter_flux[leaflitter_flux$year == i, ],
                            sum(leaf_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
        npp$LeafNPP[npp$year == i] <- round(litter_prod,2)
    }

    ### stem NPP
    wood_production_flux$year <- year(wood_production_flux$Date)
    for(i in yr.list) {
        stem_prod <- with(wood_production_flux[wood_production_flux$year == i, ],
                          sum(wood_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
        npp$StemNPP[npp$year == i] <- round(stem_prod,2)
    }

    ### fine root NPP
    fineroot_production_flux$year <- year(fineroot_production_flux$Date)
    for(i in yr.list) {
        froot_prod <- with(fineroot_production_flux[fineroot_production_flux$year == i, ],
                           sum(fineroot_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
        npp$FineRootNPP[npp$year == i] <- round(froot_prod,2)
    }

    ### Coarse root NPP
    coarse_root_production_flux_1$year <- year(coarse_root_production_flux_1$Date)
    for(i in yr.list) {
        croot_prod <- with(coarse_root_production_flux_1[coarse_root_production_flux_1$year == i, ],
                          sum(coarse_root_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
        npp$CoarseRootNPP[npp$year == i] <- round(croot_prod,2)
    }
    
    ### leaf NPP
    leaflitter_flux$year <- year(leaflitter_flux$Date)
    for (i in yr.list) {
        other_prod <- with(leaflitter_flux[leaflitter_flux$year == i, ],
                            sum((twig_flux+seed_flux+bark_flux)*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
        npp$OtherNPP[npp$year == i] <- round(other_prod,2)
    }
    
    ### frass production
    frass_production_flux$year <- year(frass_production_flux$Date)
    for(i in yr.list) {
        frass_prod <- with(frass_production_flux[frass_production_flux$year == i, ],
                           sum(frass_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
        npp$FrassProduction[npp$year == i] <- round(frass_prod, 2)
    }
    
    ### leaf consumption
    herbivory_leaf_consumption_flux$year <- year(herbivory_leaf_consumption_flux$Date)
    for(i in yr.list) {
        leaf_cons <- with(herbivory_leaf_consumption_flux[herbivory_leaf_consumption_flux$year == i, ],
                           sum(herbivory_leaf_consumption_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
        npp$LeafConsumption[npp$year == i] <- round(leaf_cons, 2)
    }

    ### Rh
    heterotrophic_respiration_flux$year <- year(heterotrophic_respiration_flux$Date)
    for(i in yr.list) {
        npp$RHetero[npp$year == i] <- with(heterotrophic_respiration_flux[heterotrophic_respiration_flux$year == i, ],
                                           sum(heterotrophic_respiration_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    }

    ### Understorey aboveground production
    understorey_aboveground_production_flux$year <- year(understorey_aboveground_production_flux$Date)
    for(i in yr.list) {
        under_prod <- with(understorey_aboveground_production_flux[understorey_aboveground_production_flux$year == i, ],
                           sum(understorey_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    }
    
    ### MycorrhizalProduction
    mycorrhizal_c_production_flux$year <- year(mycorrhizal_c_production_flux$Date)
    for(i in yr.list) {
        myc_prod <- with(mycorrhizal_c_production_flux[mycorrhizal_c_production_flux$year == i, ],
                           sum(mycorrhizal_production*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    }
    
    ### FlowerProduction
    
    
        
    ##############################################
    #### Method 1
    #### In / out fluxes (Method 1 of getting NEP)
    ##############################################
    ### define terms and dataframe
    term <- c("GPPoverstorey", "GPPunderstorey", "CH4",
              "RaLeaf", "RaStem", "RaRoot", "RaUnderstorey", "VOC",
              "Rherbivore", "DOCloss", "Rsoil", "Rgrowth")
    
    inout <- matrix(nrow=length(yr.list), ncol=length(term)+1)
    colnames(inout) <- c("year", term)
    inout <- as.data.frame(inout)
    inout$year <- yr.list
    
    ### GPP 
    for (i in yr.list) {
        inout$GPPoverstorey[inout$year == i] <- mean(overstorey_gpp_flux[overstorey_gpp_flux$year == i, "GPP"])
    }

    ### Ra leaf
    for (i in yr.list) {
        inout$RaLeaf[inout$year == i] <- mean(overstorey_leaf_respiration_flux[overstorey_leaf_respiration_flux$year == i, "Rfoliage"])
    }

    ### understorey GPP
    for (i in yr.list) {
        inout$GPPunderstorey[inout$year == i] <- mean(understorey_gpp_flux[understorey_gpp_flux$year == i, "GPP"])
    }

    ### Ra root
    root_respiration_flux$year <- year(root_respiration_flux$Start_date)
    for (i in yr.list) {
        inout$RaRoot[inout$year == i] <- with(root_respiration_flux[root_respiration_flux$year == i, ],
                                              sum(root_respiration_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    }
    
    # Rsoil
    soil_respiration_flux$year <- year(soil_respiration_flux$Start_date)
    for(i in yr.list) {
        inout$Rsoil[inout$year == i] <- with(soil_respiration_flux[soil_respiration_flux$year == i, ],
                                             sum(soil_respiration_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    }

    # Rherbivore
    herbivory_respiration_flux$year <- year(herbivory_respiration_flux$Start_date)
    for (i in yr.list) {
        inout$Rherbivore[inout$year == i] <- with(herbivory_respiration_flux[herbivory_respiration_flux$year == i,],
                                                  sum(respiration_flux*ndays)/sum(ndays)) * conv
    }

    # Rgrowth
    for (i in yr.list) {
        inout$Rgrowth[inout$year == i] <- ccost * (npp[npp$year == i, "LeafNPP"] + 
                                                   npp[npp$year == i, "StemNPP"] + 
                                                   npp[npp$year == i, "FineRootNPP"] +
                                                   npp[npp$year == i, "CoarseRootNPP"])
    }

    # DOC
    doc_leaching_flux$year <- year(doc_leaching_flux$Start_date)
    for (i in yr.list) {
        inout$DOCloss[inout$year == i] <- with(doc_leaching_flux[doc_leaching_flux$year == i,],
                                               sum(doc_leaching_flux*ndays)/sum(ndays)) * conv
    }

    # CH4
    methane_c_flux$year <- year(methane_c_flux$Start_date)
    for (i in yr.list) {
        inout$CH4[inout$year == i] <- with(methane_c_flux[methane_c_flux$year == i,],
                                               sum(methane_flux*ndays)/sum(ndays)) * conv
    }
    
    # VOC

    
    # Ra understorey
    understorey_respiration_flux$year <- year(understorey_respiration_flux$Start_date)
    for (i in yr.list) {
        inout$RaUnderstorey[inout$year == i] <- with(understorey_respiration_flux[understorey_respiration_flux$year == i,],
                                                  sum(respiration*ndays)/sum(ndays)) * conv
    }

    ##############################################
    #### Method 2
    #### Standing C pools
    ##############################################    
    ### Define terms and dataframe
    term <- c("OverstoreyLeaf", "OverstoreyWood", "UnderstoreyAboveground",
              "FineRoot", "CoarseRoot", "Litter", "CoarseWoodyDebris", 
              "MicrobialBiomass", "SoilC", "Mycorrhizae", "Insects")
    
    pool <- matrix(nrow=length(yr.list), ncol=length(term)+1)
    colnames(pool) <- c("year", term)
    pool <- as.data.frame(pool)
    pool$year <- yr.list
    
    
    ### Overstorey leaf
    leaf_c_pool$year <- year(leaf_c_pool$Date)
    for (i in yr.list) {
        pool$OverstoreyLeaf[pool$year == i] <- round(mean(leaf_c_pool[leaf_c_pool$year == i, "leaf_pool"]), 2)
    }

    ### Overstorey wood
    wood_c_pool$year <- year(wood_c_pool$Date) 
    for (i in yr.list) {
        pool$OverstoreyWood[pool$year == i] <- round(mean(wood_c_pool[wood_c_pool$year == i, "wood_pool"]), 2)
    }

    ### Understorey aboveground
    understorey_aboveground_c_pool$year <- year(understorey_aboveground_c_pool$Date)
    for (i in yr.list) {
        pool$UnderstoreyAboveground[pool$year == i] <- round(mean(understorey_aboveground_c_pool[understorey_aboveground_c_pool$year == i, 
                                                                                                 "Total_g_C_m2"]), 2)
    }

    ### Fine root
    fineroot_c_pool$year <- year(fineroot_c_pool$Date) 
    for (i in yr.list) {
        pool$FineRoot[pool$year == i] <- round(mean(fineroot_c_pool[fineroot_c_pool$year == i, "fineroot_pool"]), 2)
    }
    
    ### Coarse root
    coarse_root_c_pool_1$year <- year(coarse_root_c_pool_1$Date) 
    for (i in yr.list) {
        pool$CoarseRoot[pool$year == i] <- round(mean(coarse_root_c_pool_1[coarse_root_c_pool_1$year == i, "coarse_root_pool"]), 2)
    }

    ### Soil C
    soil_c_pool$year <- year(soil_c_pool$Date)
    for (i in yr.list) {
        pool$SoilC[pool$year == i] <- round(mean(soil_c_pool[soil_c_pool$year == i, "soil_carbon_pool"]), 2)
    }

    ### microbial pool
    microbial_c_pool$year <- year(microbial_c_pool$Date)
    for (i in yr.list) {
        pool$MicrobialBiomass[pool$year == i]  <- round(mean(microbial_c_pool[microbial_c_pool$year == i, "microbial_pool"]), 2)
    }
        
    ### Mycorrhizae
    mycorrhizal_c_pool$year <- year(mycorrhizal_c_pool$Date)
    for (i in yr.list) {
        pool$Mycorrhizae[pool$year == i]  <- round(mean(mycorrhizal_c_pool[mycorrhizal_c_pool$year == i, "mycorrhizal_c_pool"]), 2)
    }
    
    ### Litter
    leaflitter_pool$year <- year(leaflitter_pool$Date)
    for (i in yr.list) {
        pool$Litter[pool$year == i]  <- round(mean(leaflitter_pool[leaflitter_pool$year == i, "leaflitter_pool"]), 2)
    }
    
    ### Insects
    insect_pool$year <- year(insect_pool$Date)
    for (i in yr.list) {
        pool$Insects[pool$year == i]  <- round(mean(insect_pool[insect_pool$year == i, "insect_pool"]), 2)
    }
    
    ### CWD
    standing_dead_c_pool$year <- year(standing_dead_c_pool$Date)
    for (i in yr.list) {
        pool$CoarseWoodyDebris[pool$year == i]  <- round(mean(standing_dead_c_pool[standing_dead_c_pool$year == i, "wood_pool"]), 2)
    }
    
    
    ##### output tables
    return(list(inout = data.table(inout), 
                npp = data.table(npp), 
                pool = data.table(pool)))
    
}
