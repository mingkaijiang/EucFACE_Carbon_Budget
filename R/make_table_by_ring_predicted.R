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
              "Understorey NPP", "Understorey Litter",
              "Frass production", "Leaf consumption", "R hetero", 
              "Mycorrhizal production", "Flower production")
    npp <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,NA)
    colnames(npp) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2", 
                       "diff", "percent_diff", "aCO2_conf", "eCO2_conf",
                       "aCO2_sd", "eCO2_sd")
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
        
        # Understorey Litter
        npp[npp$term == "Understorey Litter", i+1] <- mean(understorey_litter_production_flux_ann$predicted[understorey_litter_production_flux_ann$Ring == i])
        
        # R heterotrophic respiration
        npp[npp$term == "R hetero", i+1] <- mean(heterotrophic_respiration_flux_ann$predicted[heterotrophic_respiration_flux_ann$Ring == i])

        # Mycorrhizal production
        #npp[npp$term == "Mycorrhizal production", i+1] <- mean(mycorrhizal_c_production_flux_ann$predicted[mycorrhizal_c_production_flux_ann$Ring == i])
        
    }

    
    ##############################################
    #### Method 1
    #### In / out fluxes (Method 1 of getting NEP)
    ##############################################
    ### define terms and dataframe
    term <- c("GPP overstorey", "GPP understorey", "CH4 efflux",
              "Ra leaf", "Ra stem", "Ra root", "Ra understorey", "VOC",
              "Rherbivore", "DOC loss", "Rsoil", "Rgrowth")
    inout <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,NA)
    colnames(inout) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2",
                         "diff", "percent_diff", "aCO2_conf", "eCO2_conf",
                         "aCO2_sd", "eCO2_sd")
    
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
                                                        npp[npp$term == "Fine Root NPP", i+1] +
                                                        npp[npp$term == "Coarse Root NPP", i+1] +
                                                        #npp[npp$term == "Understorey NPP", i+1] + 
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
        inout[inout$term == "VOC", i+1] <- mean(voc_c_flux_ann$predicted[voc_c_flux_ann$Ring == i])
        
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
    pool <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,NA)
    colnames(pool) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2", 
                        "diff", "percent_diff", "aCO2_conf", "eCO2_conf",
                       "aCO2_sd", "eCO2_sd")

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
    
    
    ##############################################
    #### Method 2
    #### change Standing C pools
    ##############################################    
    ### Define terms and dataframe
    term <- c("Overstorey leaf", "Overstorey wood", "Understorey above-ground",
              "Fine Root", "Coarse Root", "Litter", 
              "Microbial biomass", "Soil C", "Mycorrhizae", "Insects")
    delta_pool <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,NA,NA)
    colnames(delta_pool) <- c("term", paste("Ring", c(1:6), sep="_"), "aCO2", "eCO2", 
                        "diff", "percent_diff", "aCO2_conf", "eCO2_conf",
                        "aCO2_sd", "eCO2_sd")
    
    for (i in Ring) {
        
        # Overstorey leaf
        delta_pool[delta_pool$term == "Overstorey leaf", i+1] <- mean(delta_leaf_c_pool_ann$predicted[delta_leaf_c_pool_ann$Ring == i], na.rm=T)
        
        # Overstorey wood
        delta_pool[delta_pool$term == "Overstorey wood", i+1] <- mean(delta_wood_c_pool_ann$predicted[delta_wood_c_pool_ann$Ring == i], na.rm=T)
        
        # Fine Root
        delta_pool[delta_pool$term == "Fine Root", i+1] <- mean(delta_fineroot_c_pool_ann$predicted[delta_fineroot_c_pool_ann$Ring == i], na.rm=T)
        
        # Coarse Root
        delta_pool[delta_pool$term == "Coarse Root", i+1] <- mean(delta_coarse_root_c_pool_ann$predicted[delta_coarse_root_c_pool_ann$Ring == i], na.rm=T)
        
        # Understorey above-ground
        delta_pool[delta_pool$term == "Understorey above-ground", i+1] <- mean(delta_understorey_aboveground_c_pool_ann$predicted[delta_understorey_aboveground_c_pool_ann$Ring == i], na.rm=T)
        
        # Soil C
        delta_pool[delta_pool$term == "Soil C", i+1] <- mean(delta_soil_c_pool_ann$predicted[delta_soil_c_pool_ann$Ring == i], na.rm=T)
        
        # Microbial biomass
        delta_pool[delta_pool$term == "Microbial biomass", i+1]  <- mean(delta_microbial_c_pool_ann$predicted[delta_microbial_c_pool_ann$Ring == i], na.rm=T)
        
        # Mycorrhizae
        delta_pool[delta_pool$term == "Mycorrhizae", i+1]  <- mean(delta_mycorrhizal_c_pool_ann$predicted[delta_mycorrhizal_c_pool_ann$Ring == i], na.rm=T)
        
        # Insects
        delta_pool[delta_pool$term == "Insects", i+1] <- mean(delta_insect_pool_ann$predicted[delta_insect_pool_ann$Ring == i], na.rm=T)
        
        # Litter
        delta_pool[delta_pool$term == "Litter", i+1]  <- mean(delta_leaflitter_pool_ann$predicted[delta_leaflitter_pool_ann$Ring == i], na.rm=T)
        
    }
    
    ###### calculate aCO2 and eCO2 results
    inout$aCO2 <- rowMeans(subset(inout, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    inout$eCO2 <- rowMeans(subset(inout, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    npp$aCO2 <- rowMeans(subset(npp, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    npp$eCO2 <- rowMeans(subset(npp, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    pool$aCO2 <- rowMeans(subset(pool, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    pool$eCO2 <- rowMeans(subset(pool, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
    delta_pool$aCO2 <- rowMeans(subset(delta_pool, select=c(Ring_2, Ring_3, Ring_6)), na.rm=T)
    delta_pool$eCO2 <- rowMeans(subset(delta_pool, select=c(Ring_1, Ring_4, Ring_5)), na.rm=T)
    
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
    
    ### Function to calculate standard deviation of predicted value to real value
    calc_conf_of_predicted_variable <- function(myDF) {
        
        ### calculate diff and square it
        myDF$diff <- abs(with(myDF, predicted-Value))^2
        
        ### get N - sample size
        n1 <- length(myDF$Trt[myDF$Trt=="amb"])
        n2 <- length(myDF$Trt[myDF$Trt=="ele"])
        
        ### sum all diff per treatment
        out <- summaryBy(diff~Trt, data=myDF, FUN=sum, keep.names=T, na.rm=T)
        
        ## sample size
        out$n[out$Trt=="amb"] <- n1
        out$n[out$Trt=="ele"] <- n2
        
        ### calculate variance
        out$variance <- out$diff / out$n
        out$variance <- out$diff / out$n
        
        ### calculate sd, se and confidence interval
        out$sd <- sqrt(out$variance)
        out$se <- out$sd / sqrt(out$n)
        out$conf <- out$se * 1.96
        
        return(data.frame(out[,c("Trt", "n", "sd", "se","conf")]))
    }
    
    calc_conf_of_predicted_delta_pool <- function(myDF) {
        
        ### calculate diff and square it
        myDF$diff <- abs(with(myDF, predicted-delta))^2
        
        ### get N - sample size
        n1 <- length(myDF$Trt[myDF$Trt=="amb"])
        n2 <- length(myDF$Trt[myDF$Trt=="ele"])
        
        ### sum all diff per treatment
        out <- summaryBy(diff~Trt, data=myDF, FUN=sum, keep.names=T, na.rm=T)
        
        ## sample size
        out$n[out$Trt=="amb"] <- n1
        out$n[out$Trt=="ele"] <- n2
        
        ### calculate variance
        out$variance <- out$diff / out$n
        out$variance <- out$diff / out$n
        
        ### calculate sd, se and confidence interval
        out$sd <- sqrt(out$variance)
        out$se <- out$sd / sqrt(out$n)
        out$conf <- out$se * 1.96
        
        return(data.frame(out[,c("Trt", "n", "sd", "se","conf")]))
    }
    
    ### make sd, se and conf calculations for each variable
    tmpDF <- calc_conf_of_predicted_variable(myDF=overstorey_gpp_flux_ann)
    inout$aCO2_conf[inout$term=="GPP overstorey"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    inout$eCO2_conf[inout$term=="GPP overstorey"] <- tmpDF$conf[tmpDF$Trt=="ele"]
    inout$aCO2_sd[inout$term=="GPP overstorey"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    inout$eCO2_sd[inout$term=="GPP overstorey"] <- tmpDF$sd[tmpDF$Trt=="ele"]
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=understorey_gpp_flux_ann)
    inout$aCO2_conf[inout$term=="GPP understorey"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    inout$eCO2_conf[inout$term=="GPP understorey"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    inout$aCO2_sd[inout$term=="GPP understorey"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    inout$eCO2_sd[inout$term=="GPP understorey"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=methane_c_flux_ann)
    inout$aCO2_conf[inout$term=="CH4 efflux"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    inout$eCO2_conf[inout$term=="CH4 efflux"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    inout$aCO2_sd[inout$term=="CH4 efflux"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    inout$eCO2_sd[inout$term=="CH4 efflux"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=overstorey_leaf_respiration_flux_ann)
    inout$aCO2_conf[inout$term=="Ra leaf"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    inout$eCO2_conf[inout$term=="Ra leaf"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    inout$aCO2_sd[inout$term=="Ra leaf"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    inout$eCO2_sd[inout$term=="Ra leaf"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=wood_respiration_flux_ann)
    inout$aCO2_conf[inout$term=="Ra stem"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    inout$eCO2_conf[inout$term=="Ra stem"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    inout$aCO2_sd[inout$term=="Ra stem"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    inout$eCO2_sd[inout$term=="Ra stem"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=root_respiration_flux_ann)
    inout$aCO2_conf[inout$term=="Ra root"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    inout$eCO2_conf[inout$term=="Ra root"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    inout$aCO2_sd[inout$term=="Ra root"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    inout$eCO2_sd[inout$term=="Ra root"] <- tmpDF$sd[tmpDF$Trt=="ele"]
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=understorey_respiration_flux_ann)
    inout$aCO2_conf[inout$term=="Ra understorey"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    inout$eCO2_conf[inout$term=="Ra understorey"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    inout$aCO2_sd[inout$term=="Ra understorey"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    inout$eCO2_sd[inout$term=="Ra understorey"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=herbivory_respiration_flux_ann)
    inout$aCO2_conf[inout$term=="Rherbivore"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    inout$eCO2_conf[inout$term=="Rherbivore"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    inout$aCO2_sd[inout$term=="Rherbivore"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    inout$eCO2_sd[inout$term=="Rherbivore"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=doc_leaching_flux_ann)
    inout$aCO2_conf[inout$term=="DOC loss"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    inout$eCO2_conf[inout$term=="DOC loss"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    inout$aCO2_sd[inout$term=="DOC loss"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    inout$eCO2_sd[inout$term=="DOC loss"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=soil_respiration_flux_ann)
    inout$aCO2_conf[inout$term=="Rsoil"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    inout$eCO2_conf[inout$term=="Rsoil"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    inout$aCO2_sd[inout$term=="Rsoil"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    inout$eCO2_sd[inout$term=="Rsoil"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=voc_c_flux_ann)
    inout$aCO2_conf[inout$term=="VOC"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    inout$eCO2_conf[inout$term=="VOC"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    inout$aCO2_sd[inout$term=="VOC"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    inout$eCO2_sd[inout$term=="VOC"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF1 <- data.frame(inout$Ring_2[inout$term=="Rgrowth"], 
                         inout$Ring_3[inout$term=="Rgrowth"], 
                         inout$Ring_6[inout$term=="Rgrowth"])
    tmpDF2 <- data.frame(inout$Ring_1[inout$term=="Rgrowth"], 
                         inout$Ring_4[inout$term=="Rgrowth"], 
                         inout$Ring_5[inout$term=="Rgrowth"])
    tmpDF1$conf <- sd(tmpDF1[,1:3])/sqrt(3)*1.96
    tmpDF2$conf <- sd(tmpDF2[,1:3])/sqrt(3)*1.96
    inout$aCO2_conf[inout$term=="Rgrowth"] <- tmpDF1$conf
    inout$eCO2_conf[inout$term=="Rgrowth"] <- tmpDF2$conf
    tmpDF1$sd <- sd(tmpDF1[,1:3])
    tmpDF2$sd <- sd(tmpDF2[,1:3])
    inout$aCO2_sd[inout$term=="Rgrowth"] <- tmpDF1$sd
    inout$eCO2_sd[inout$term=="Rgrowth"] <- tmpDF2$sd
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=understorey_gpp_flux_ann)
    inout$aCO2_conf[inout$term=="GPP understorey"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    inout$eCO2_conf[inout$term=="GPP understorey"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    inout$aCO2_sd[inout$term=="GPP understorey"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    inout$eCO2_sd[inout$term=="GPP understorey"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=leaflitter_flux_ann)
    npp$aCO2_conf[npp$term=="Leaf NPP"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    npp$eCO2_conf[npp$term=="Leaf NPP"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    npp$aCO2_sd[npp$term=="Leaf NPP"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    npp$eCO2_sd[npp$term=="Leaf NPP"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=wood_production_flux_ann)
    npp$aCO2_conf[npp$term=="Stem NPP"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    npp$eCO2_conf[npp$term=="Stem NPP"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    npp$aCO2_sd[npp$term=="Stem NPP"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    npp$eCO2_sd[npp$term=="Stem NPP"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=fineroot_production_flux_ann)
    npp$aCO2_conf[npp$term=="Fine Root NPP"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    npp$eCO2_conf[npp$term=="Fine Root NPP"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    npp$aCO2_sd[npp$term=="Fine Root NPP"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    npp$eCO2_sd[npp$term=="Fine Root NPP"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=coarse_root_production_flux_ann)
    npp$aCO2_conf[npp$term=="Coarse Root NPP"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    npp$eCO2_conf[npp$term=="Coarse Root NPP"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    npp$aCO2_sd[npp$term=="Coarse Root NPP"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    npp$eCO2_sd[npp$term=="Coarse Root NPP"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    otherlitter_flux_ann <- twiglitter_flux_ann
    otherlitter_flux_ann$Value <- twiglitter_flux_ann$Value + barklitter_flux_ann$Value + seedlitter_flux_ann$Value
    otherlitter_flux_ann$predicted <- twiglitter_flux_ann$predicted + barklitter_flux_ann$predicted + seedlitter_flux_ann$predicted
    tmpDF <- calc_conf_of_predicted_variable(myDF=otherlitter_flux_ann)
    npp$aCO2_conf[npp$term=="Other NPP"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    npp$eCO2_conf[npp$term=="Other NPP"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    npp$aCO2_sd[npp$term=="Other NPP"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    npp$eCO2_sd[npp$term=="Other NPP"] <- tmpDF$sd[tmpDF$Trt=="ele"]
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=understorey_aboveground_production_flux_ann)
    npp$aCO2_conf[npp$term=="Understorey NPP"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    npp$eCO2_conf[npp$term=="Understorey NPP"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    npp$aCO2_sd[npp$term=="Understorey NPP"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    npp$eCO2_sd[npp$term=="Understorey NPP"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=understorey_litter_production_flux_ann)
    npp$aCO2_conf[npp$term=="Understorey Litter"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    npp$eCO2_conf[npp$term=="Understorey Litter"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    npp$aCO2_sd[npp$term=="Understorey Litter"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    npp$eCO2_sd[npp$term=="Understorey Litter"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=frass_production_flux_ann)
    npp$aCO2_conf[npp$term=="Frass production"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    npp$eCO2_conf[npp$term=="Frass production"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    npp$aCO2_sd[npp$term=="Frass production"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    npp$eCO2_sd[npp$term=="Frass production"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=herbivory_leaf_consumption_flux_ann)
    npp$aCO2_conf[npp$term=="Leaf consumption"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    npp$eCO2_conf[npp$term=="Leaf consumption"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    npp$aCO2_sd[npp$term=="Leaf consumption"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    npp$eCO2_sd[npp$term=="Leaf consumption"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=heterotrophic_respiration_flux_ann)
    npp$aCO2_conf[npp$term=="R hetero"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    npp$eCO2_conf[npp$term=="R hetero"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    npp$aCO2_sd[npp$term=="R hetero"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    npp$eCO2_sd[npp$term=="R hetero"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    #tmpDF <- calc_conf_of_predicted_variable(myDF=)
    #npp$aCO2_conf[npp$term=="Mycorrhizal production"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    #npp$eCO2_conf[npp$term=="Mycorrhizal production"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=leaf_c_pool_ann)
    pool$aCO2_conf[pool$term=="Overstorey leaf"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    pool$eCO2_conf[pool$term=="Overstorey leaf"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    pool$aCO2_sd[pool$term=="Overstorey leaf"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    pool$eCO2_sd[pool$term=="Overstorey leaf"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=wood_c_pool_ann)
    pool$aCO2_conf[pool$term=="Overstorey wood"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    pool$eCO2_conf[pool$term=="Overstorey wood"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    pool$aCO2_sd[pool$term=="Overstorey wood"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    pool$eCO2_sd[pool$term=="Overstorey wood"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=understorey_aboveground_c_pool_ann)
    pool$aCO2_conf[pool$term=="Understorey above-ground"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    pool$eCO2_conf[pool$term=="Understorey above-ground"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    pool$aCO2_sd[pool$term=="Understorey above-ground"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    pool$eCO2_sd[pool$term=="Understorey above-ground"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=fineroot_c_pool_ann)
    pool$aCO2_conf[pool$term=="Fine Root"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    pool$eCO2_conf[pool$term=="Fine Root"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    pool$aCO2_sd[pool$term=="Fine Root"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    pool$eCO2_sd[pool$term=="Fine Root"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=coarse_root_c_pool_ann)
    pool$aCO2_conf[pool$term=="Coarse Root"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    pool$eCO2_conf[pool$term=="Coarse Root"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    pool$aCO2_sd[pool$term=="Coarse Root"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    pool$eCO2_sd[pool$term=="Coarse Root"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=leaflitter_pool_ann)
    pool$aCO2_conf[pool$term=="Litter"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    pool$eCO2_conf[pool$term=="Litter"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    pool$aCO2_sd[pool$term=="Litter"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    pool$eCO2_sd[pool$term=="Litter"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=microbial_c_pool_ann)
    pool$aCO2_conf[pool$term=="Microbial biomass"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    pool$eCO2_conf[pool$term=="Microbial biomass"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    pool$aCO2_sd[pool$term=="Microbial biomass"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    pool$eCO2_sd[pool$term=="Microbial biomass"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=soil_c_pool_ann)
    pool$aCO2_conf[pool$term=="Soil C"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    pool$eCO2_conf[pool$term=="Soil C"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    pool$aCO2_sd[pool$term=="Soil C"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    pool$eCO2_sd[pool$term=="Soil C"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=mycorrhizal_c_pool_ann)
    pool$aCO2_conf[pool$term=="Mycorrhizae"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    pool$eCO2_conf[pool$term=="Mycorrhizae"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    pool$aCO2_sd[pool$term=="Mycorrhizae"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    pool$eCO2_sd[pool$term=="Mycorrhizae"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_variable(myDF=insect_pool_ann)
    pool$aCO2_conf[pool$term=="Insects"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    pool$eCO2_conf[pool$term=="Insects"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    pool$aCO2_sd[pool$term=="Insects"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    pool$eCO2_sd[pool$term=="Insects"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_delta_pool(myDF=delta_leaf_c_pool_ann)
    delta_pool$aCO2_conf[delta_pool$term=="Overstorey leaf"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_conf[delta_pool$term=="Overstorey leaf"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    delta_pool$aCO2_sd[delta_pool$term=="Overstorey leaf"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_sd[delta_pool$term=="Overstorey leaf"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_delta_pool(myDF=delta_wood_c_pool_ann)
    delta_pool$aCO2_conf[delta_pool$term=="Overstorey wood"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_conf[delta_pool$term=="Overstorey wood"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    delta_pool$aCO2_sd[delta_pool$term=="Overstorey wood"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_sd[delta_pool$term=="Overstorey wood"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_delta_pool(myDF=delta_understorey_aboveground_c_pool_ann)
    delta_pool$aCO2_conf[delta_pool$term=="Understorey above-ground"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_conf[delta_pool$term=="Understorey above-ground"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    delta_pool$aCO2_sd[delta_pool$term=="Understorey above-ground"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_sd[delta_pool$term=="Understorey above-ground"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_delta_pool(myDF=delta_fineroot_c_pool_ann)
    delta_pool$aCO2_conf[delta_pool$term=="Fine Root"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_conf[delta_pool$term=="Fine Root"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    delta_pool$aCO2_sd[delta_pool$term=="Fine Root"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_sd[delta_pool$term=="Fine Root"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_delta_pool(myDF=delta_coarse_root_c_pool_ann)
    delta_pool$aCO2_conf[delta_pool$term=="Coarse Root"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_conf[delta_pool$term=="Coarse Root"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    delta_pool$aCO2_sd[delta_pool$term=="Coarse Root"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_sd[delta_pool$term=="Coarse Root"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_delta_pool(myDF=delta_leaflitter_pool_ann)
    delta_pool$aCO2_conf[delta_pool$term=="Litter"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_conf[delta_pool$term=="Litter"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    delta_pool$aCO2_sd[delta_pool$term=="Litter"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_sd[delta_pool$term=="Litter"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_delta_pool(myDF=delta_microbial_c_pool_ann)
    delta_pool$aCO2_conf[delta_pool$term=="Microbial biomass"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_conf[delta_pool$term=="Microbial biomass"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    delta_pool$aCO2_sd[delta_pool$term=="Microbial biomass"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_sd[delta_pool$term=="Microbial biomass"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_delta_pool(myDF=delta_soil_c_pool_ann)
    delta_pool$aCO2_conf[delta_pool$term=="Soil C"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_conf[delta_pool$term=="Soil C"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    delta_pool$aCO2_sd[delta_pool$term=="Soil C"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_sd[delta_pool$term=="Soil C"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_delta_pool(myDF=delta_mycorrhizal_c_pool_ann)
    delta_pool$aCO2_conf[delta_pool$term=="Mycorrhizae"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_conf[delta_pool$term=="Mycorrhizae"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    delta_pool$aCO2_sd[delta_pool$term=="Mycorrhizae"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_sd[delta_pool$term=="Mycorrhizae"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    tmpDF <- calc_conf_of_predicted_delta_pool(myDF=delta_insect_pool_ann)
    delta_pool$aCO2_conf[delta_pool$term=="Insects"] <- tmpDF$conf[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_conf[delta_pool$term=="Insects"] <- tmpDF$conf[tmpDF$Trt=="ele"] 
    delta_pool$aCO2_sd[delta_pool$term=="Insects"] <- tmpDF$sd[tmpDF$Trt=="amb"] 
    delta_pool$eCO2_sd[delta_pool$term=="Insects"] <- tmpDF$sd[tmpDF$Trt=="ele"] 
    
    ##### output tables
    return(list(inout = data.table(inout), 
                npp = data.table(npp), 
                pool = data.table(pool),
                delta_pool = data.table(delta_pool)))
}
