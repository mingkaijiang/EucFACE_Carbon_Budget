report_key_values_for_manuscript <- function() {
    ##### All values reported below are based on predicted CO2 effect
    
    
    ### subseting each method
    inoutDF <- as.data.frame(inDF$inout[,1:7])
    nppDF <- as.data.frame(inDF$npp[,1:7])
    
    ### prepare output df
    out <- data.frame(c("In-out", "NPP-Rh", "Pool"), NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("Method", "R1", "R2", "R3", "R4", "R5", "R6")
    
    ### calculate NEP based on each method
    for (i in c(2:7)) {
        out[out$Method=="In-out", i] <- (inoutDF[inoutDF$term == "GPP overstorey", i] + 
                                             inoutDF[inoutDF$term == "GPP understorey", i] -
                                             inoutDF[inoutDF$term == "CH4 efflux", i] -
                                             inoutDF[inoutDF$term == "Ra leaf", i] -
                                             inoutDF[inoutDF$term == "Ra stem", i] -
                                             inoutDF[inoutDF$term == "Ra understorey", i] -
                                             #inoutDF[inoutDF$term == "VOC", i] -
                                             inoutDF[inoutDF$term == "Rherbivore", i] -
                                             inoutDF[inoutDF$term == "DOC loss", i] -
                                             inoutDF[inoutDF$term == "Rsoil", i] -
                                             inoutDF[inoutDF$term == "Rgrowth", i]) 
        
        out[out$Method=="NPP-Rh", i] <- nppDF[nppDF$term == "Leaf NPP", i] +
            nppDF[nppDF$term == "Stem NPP", i] +
            nppDF[nppDF$term == "Fine Root NPP", i] +
            nppDF[nppDF$term == "Coarse Root NPP", i] +
            nppDF[nppDF$term == "Other NPP", i] +
            nppDF[nppDF$term == "Understorey NPP", i] +
            nppDF[nppDF$term == "Leaf consumption", i] -
            #nppDF[nppDF$term == "Mycorrhizal production", i] -
            nppDF[nppDF$term == "R hetero", i] 
        #nppDF[nppDF$term == "Flower production", i] 
    }
    
    ### Change in pools
    delta_soil_c <- make_yearly_delta_pool_function_ann(inDF=soil_c_pool_ann, var.col=9)
    delta_leaf_c <- make_yearly_delta_pool_function_ann(inDF=leaf_c_pool_ann, var.col=9)
    delta_wood_c <- make_yearly_delta_pool_function_ann(inDF=wood_c_pool_ann, var.col=9)
    delta_croot_c <- make_yearly_delta_pool_function_ann(inDF=coarse_root_c_pool_ann, var.col=9)
    delta_froot_c <- make_yearly_delta_pool_function_ann(inDF=fineroot_c_pool_ann, var.col=9)
    delta_ua_c <- make_yearly_delta_pool_function_ann(inDF=understorey_aboveground_c_pool_ann, var.col=9)
    delta_mic_c <- make_yearly_delta_pool_function_ann(inDF=microbial_c_pool_ann, var.col=9)
    delta_myc_c <- make_yearly_delta_pool_function_ann(inDF=mycorrhizal_c_pool_ann, var.col=9)
    delta_ins_c <- make_yearly_delta_pool_function_ann(inDF=insect_pool_ann, var.col=9)
    delta_lit_c <- make_yearly_delta_pool_function_ann(inDF=leaflitter_pool_ann, var.col=9)
    
    ### create df to store pools
    pool.list <- c("soilc", "leafc", "woodc", "crootc", "frootc", "uac",
                   "micc", "mycc", "litter", "cwd", "insect")
    poolDF <- data.frame(pool.list, NA,NA,NA,NA,NA,NA)
    colnames(poolDF) <- c("Term", "R1", "R2", "R3", "R4", "R5", "R6")
    
    ### A function to calculate means of each ring, return unit of g C m-2 yr-1
    calculate_variable_mean <- function(delta_pool) {
        temp <- data.frame(c(1:6), NA)
        colnames(temp) <- c("Ring", "Value")
        
        for (i in 1:6) {
            temp$Value[temp$Ring==i] <- with(delta_pool[delta_pool$Ring == i,],
                                             sum(daily_biomass_change*ndays)/sum(ndays)) * 365 
        }
        
        #out <- mean(temp$Value, na.rm=T)
        
        return(temp)
    }
    
    ### assign values
    #poolDF[poolDF$Term=="soilc",2:7] <- calculate_variable_mean(delta_soil_c)$Value
    #poolDF[poolDF$Term=="leafc",2:7] <- calculate_variable_mean(delta_leaf_c)$Value
    #poolDF[poolDF$Term=="woodc",2:7] <- calculate_variable_mean(delta_wood_c)$Value
    #poolDF[poolDF$Term=="crootc",2:7] <- calculate_variable_mean(delta_croot_c)$Value
    #poolDF[poolDF$Term=="frootc",2:7] <- calculate_variable_mean(delta_froot_c)$Value
    #poolDF[poolDF$Term=="uac",2:7] <- calculate_variable_mean(delta_ua_c)$Value
    #poolDF[poolDF$Term=="micc",2:7] <- calculate_variable_mean(delta_mic_c)$Value
    #poolDF[poolDF$Term=="mycc",2:7] <- calculate_variable_mean(delta_myc_c)$Value
    #poolDF[poolDF$Term=="insect",2:7] <- calculate_variable_mean(delta_ins_c)$Value
    #poolDF[poolDF$Term=="cwd",2:7] <- 0.0
    #poolDF[poolDF$Term=="litter",2:7] <- calculate_variable_mean(delta_lit_c)$Value
    
    poolDF[poolDF$Term=="soilc",2:7] <- summaryBy(delta~Ring,data=delta_soil_c,keep.names=T)$delta
    poolDF[poolDF$Term=="leafc",2:7] <- summaryBy(delta~Ring,data=delta_leaf_c,keep.names=T)$delta
    poolDF[poolDF$Term=="woodc",2:7] <- summaryBy(delta~Ring,data=delta_wood_c,keep.names=T)$delta
    poolDF[poolDF$Term=="crootc",2:7] <- summaryBy(delta~Ring,data=delta_croot_c,keep.names=T)$delta
    poolDF[poolDF$Term=="frootc",2:7] <- summaryBy(delta~Ring,data=delta_froot_c,keep.names=T)$delta
    poolDF[poolDF$Term=="uac",2:7] <- summaryBy(delta~Ring,data=delta_ua_c,keep.names=T)$delta
    poolDF[poolDF$Term=="micc",2:7] <- summaryBy(delta~Ring,data=delta_mic_c,keep.names=T)$delta
    poolDF[poolDF$Term=="mycc",2:7] <- summaryBy(delta~Ring,data=delta_myc_c,keep.names=T)$delta
    poolDF[poolDF$Term=="insect",2:7] <- summaryBy(delta~Ring,data=delta_ins_c,keep.names=T)$delta
    poolDF[poolDF$Term=="cwd",2:7] <- 0.0
    poolDF[poolDF$Term=="litter",2:7] <- summaryBy(delta~Ring,data=delta_lit_c,keep.names=T)$delta
    
    ### NEP change in pools
    for (i in c(2:7)) {
        out[out$Method=="Pool", i] <- sum(poolDF[,i])
    }
    
    # calculate means and sd
    out$aCO2 <- rowMeans(subset(out, select=c(R2, R3, R6)), na.rm=T)
    out$eCO2 <- rowMeans(subset(out, select=c(R1, R4, R5)), na.rm=T)
    
    require(sciplot)
    
    aC <- data.frame(out$R2, out$R3, out$R6)
    #aCo <- transform(aC, SD = apply(aC, 1, sd, na.rm=T))
    aCo <- transform(aC, SE = apply(aC, 1, se, na.rm=T))
    
    out$aCO2_se <- aCo$SE
    
    eC <- data.frame(out$R1, out$R4, out$R5)
    out$eCO2_se <- transform(eC, SE = apply(eC, 1, se, na.rm=T))$SE
    
    write.csv(out, "R_other/NEP_method_comparison.csv", row.names=F)
    
    
    ### increase and percent increase in gross ecosystem carbon uptake
    
    
    ### increase and percent increase in Rsoil
    
    ### increase and percent increase in Rwood
    
    ### increase and percent increase in Rua
    
    ### Increase in NPP
    
    ### increase in incremental change 
    
    ### NEP values
    nep <- read.csv("R_other/nep_summary.csv")
    
}