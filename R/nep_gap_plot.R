nep_gap_plot <- function(inDF) {
    
    ### subseting each method
    inoutDF <- as.data.frame(inDF$inout[, 1:7])
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
            nppDF[nppDF$term == "R hetero", i] 
            #nppDF[nppDF$term == "Mycorrhizal production", i] +-
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

    #source("R/stats/change_in_pool/make_change_in_pool_ann.R")
    ### Compute changes in pool variables
    #delta_soil_c <- make_change_in_pool_ann(mypool=soil_c_pool_ann)
    #delta_leaf_c <- make_change_in_pool_ann(mypool=leaf_c_pool_ann)
    #delta_wood_c <- make_change_in_pool_ann(mypool=wood_c_pool_ann)
    #delta_croot_c <- make_change_in_pool_ann(mypool=coarse_root_c_pool_ann)
    #delta_froot_c <- make_change_in_pool_ann(mypool=fineroot_c_pool_ann)
    #delta_ua_c <- make_change_in_pool_ann(mypool=understorey_aboveground_c_pool_ann)
    #delta_mic_c <- make_change_in_pool_ann(mypool=microbial_c_pool_ann)
    #delta_myc_c <- make_change_in_pool_ann(mypool=mycorrhizal_c_pool_ann)
    #delta_ins_c <- make_change_in_pool_ann(mypool=insect_pool_ann)
    #delta_lit_c <- make_change_in_pool_ann(mypool=leaflitter_pool_ann)

    
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
    
    aC <- data.frame(out$R2, out$R3, out$R6)
    aCo <- transform(aC, SD = apply(aC, 1, sd, na.rm=T))
    out$aCO2_sd <- aCo$SD
    
    eC <- data.frame(out$R1, out$R4, out$R5)
    out$eCO2_sd <- transform(eC, SD = apply(eC, 1, sd, na.rm=T))$SD
    
    write.csv(out, "R_other/NEP_method_comparison.csv", row.names=F)
    
    
    ### prepare plotDF
    plotDF <- data.frame(rep(c("In-out", "NPP-Rh", "Pool"), 2), NA, NA, NA)
    colnames(plotDF) <- c("Method", "NEP", "NEP_sd", "Trt")
    plotDF$Trt <- rep(c("aCO2", "eCO2"), each=3)
    
    plotDF$NEP[plotDF$Method=="In-out" & plotDF$Trt=="aCO2"] <- out$aCO2[out$Method=="In-out"]
    plotDF$NEP[plotDF$Method=="NPP-Rh" & plotDF$Trt=="aCO2"] <- out$aCO2[out$Method=="NPP-Rh"]
    plotDF$NEP[plotDF$Method=="Pool" & plotDF$Trt=="aCO2"] <- out$aCO2[out$Method=="Pool"]
    plotDF$NEP[plotDF$Method=="In-out" & plotDF$Trt=="eCO2"] <- out$eCO2[out$Method=="In-out"]
    plotDF$NEP[plotDF$Method=="NPP-Rh" & plotDF$Trt=="eCO2"] <- out$eCO2[out$Method=="NPP-Rh"]
    plotDF$NEP[plotDF$Method=="Pool" & plotDF$Trt=="eCO2"] <- out$eCO2[out$Method=="Pool"]
    
    plotDF$NEP_sd[plotDF$Method=="In-out" & plotDF$Trt=="aCO2"] <- out$aCO2_sd[out$Method=="In-out"]
    plotDF$NEP_sd[plotDF$Method=="NPP-Rh" & plotDF$Trt=="aCO2"] <- out$aCO2_sd[out$Method=="NPP-Rh"]
    plotDF$NEP_sd[plotDF$Method=="Pool" & plotDF$Trt=="aCO2"] <- out$aCO2_sd[out$Method=="Pool"]
    plotDF$NEP_sd[plotDF$Method=="In-out" & plotDF$Trt=="eCO2"] <- out$eCO2_sd[out$Method=="In-out"]
    plotDF$NEP_sd[plotDF$Method=="NPP-Rh" & plotDF$Trt=="eCO2"] <- out$eCO2_sd[out$Method=="NPP-Rh"]
    plotDF$NEP_sd[plotDF$Method=="Pool" & plotDF$Trt=="eCO2"] <- out$eCO2_sd[out$Method=="Pool"]
    
    plotDF$pos <- plotDF$NEP + plotDF$NEP_sd
    plotDF$neg <- plotDF$NEP - plotDF$NEP_sd
    
    ### make the bar plot
    p1 <- ggplot(plotDF,
                aes(Method, NEP)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        #geom_point(mapping=aes(x=Method, y=NEP, fill=Trt), 
        #           size=4, shape=21,position = position_dodge(0.9))+
        xlab("Method") + ylab(expression(paste("NEP (g C ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="bottom")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "grey", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete("",  
                         labels=c("In - Out",
                                  expression(paste("NPP - ", R[h])),
                                  expression(Delta*C[pools])))+
        theme(legend.justification=c(1,0), legend.position=c(0.9,0.05))
    
    #plot(p1)
    
    pdf("R_other/nep_gap.pdf", width=8, height=8)
    plot(p1)
    dev.off()
    
}