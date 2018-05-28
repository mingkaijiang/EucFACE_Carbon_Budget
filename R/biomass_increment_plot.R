#### To make EucFACE table by year
#### Ignore ring variability
biomass_increment_plot <- function() {
    
    ### Define terms and dataframe
    term <- c("OverstoreyLeaf", "OverstoreyWood", "UnderstoreyAboveground",
              "FineRoot", "CoarseRoot", "Litter", "CoarseWoodyDebris", 
              "MicrobialBiomass", "SoilC", "Mycorrhizae", "Insects")
    
    incDF <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(incDF) <- c("term", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    
    
    ### Overstorey leaf
    min.date <- min(leaf_c_pool$Date)
    max.date <- max(leaf_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- leaf_c_pool[leaf_c_pool$Ring == i & leaf_c_pool$Date == max.date, "leaf_pool"] - leaf_c_pool[leaf_c_pool$Ring == i & leaf_c_pool$Date == min.date, "leaf_pool"]
        incDF[incDF$term == "OverstoreyLeaf", i+1] <- round(diff / date.diff * 365, 2)
    }


    ### Overstorey wood
    min.date <- min(wood_c_pool$Date)
    max.date <- max(wood_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- wood_c_pool[wood_c_pool$Ring == i & wood_c_pool$Date == max.date, "wood_pool"] - wood_c_pool[wood_c_pool$Ring == i & wood_c_pool$Date == min.date, "wood_pool"]
        incDF[incDF$term == "OverstoreyWood", i+1] <- round(diff / date.diff * 365, 2)
    }
    

    ### Understorey aboveground
    min.date <- min(understorey_aboveground_c_pool$Date)
    max.date <- max(understorey_aboveground_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- understorey_aboveground_c_pool[understorey_aboveground_c_pool$Ring == i & understorey_aboveground_c_pool$Date == max.date, "Total_g_C_m2"] - understorey_aboveground_c_pool[understorey_aboveground_c_pool$Ring == i & understorey_aboveground_c_pool$Date == min.date, "Total_g_C_m2"]
        incDF[incDF$term == "UnderstoreyAboveground", i+1] <- round(diff / date.diff * 365, 2)
    }

    ### Fine root
    min.date <- min(fineroot_c_pool$Date)
    max.date <- max(fineroot_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- fineroot_c_pool[fineroot_c_pool$Ring == i & fineroot_c_pool$Date == max.date, "fineroot_pool"] - fineroot_c_pool[fineroot_c_pool$Ring == i & fineroot_c_pool$Date == min.date, "fineroot_pool"]
        incDF[incDF$term == "FineRoot", i+1] <- round(diff / date.diff * 365, 2)
    }

    ### Coarse root
    min.date <- min(coarse_root_c_pool_1$Date)
    max.date <- max(coarse_root_c_pool_1$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- coarse_root_c_pool_1[coarse_root_c_pool_1$Ring == i & coarse_root_c_pool_1$Date == max.date, "coarse_root_pool"] - coarse_root_c_pool_1[coarse_root_c_pool_1$Ring == i & coarse_root_c_pool_1$Date == min.date, "coarse_root_pool"]
        incDF[incDF$term == "CoarseRoot", i+1] <- round(diff / date.diff * 365, 2)
    }

    ### Soil C
    min.date <- min(soil_c_pool$Date)
    max.date <- max(soil_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- soil_c_pool[soil_c_pool$Ring == i & soil_c_pool$Date == max.date, "soil_carbon_pool"] - soil_c_pool[soil_c_pool$Ring == i & soil_c_pool$Date == min.date, "soil_carbon_pool"]
        incDF[incDF$term == "SoilC", i+1] <- round(diff / date.diff * 365, 2)
    }

    ### microbial pool
    min.date <- min(microbial_c_pool$Date)
    max.date <- max(microbial_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- microbial_c_pool[microbial_c_pool$Ring == i & microbial_c_pool$Date == max.date, "microbial_pool"] - microbial_c_pool[microbial_c_pool$Ring == i & microbial_c_pool$Date == min.date, "microbial_pool"]
        incDF[incDF$term == "MicrobialBiomass", i+1] <- round(diff / date.diff * 365, 2)
    }
        
    # CO2 effect
    incDF$aCO2 <- rowMeans(subset(incDF, select=c(R2, R3, R6)), na.rm=T)
    incDF$eCO2 <- rowMeans(subset(incDF, select=c(R1, R4, R5)), na.rm=T)
    
    incDF$diff <- round(incDF$eCO2 - incDF$aCO2,2)
    incDF$percent_diff <- round(incDF$diff / incDF$aCO2,2)
}
