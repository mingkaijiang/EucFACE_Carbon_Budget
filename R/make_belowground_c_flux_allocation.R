#### To make EucFACE table by year
#### Ignore ring variability
make_belowground_c_flux_allocation <- function() {
    
    #### Define dataframe
    term <- c("FineRootNPP", "CoarseRootNPP","MycorrhizalProduction", "RaRoot")
    
    myDF <- data.frame(rep(term, 2), NA, NA, NA)
    colnames(myDF) <- c("term", "trt", "value", "ratio")
    myDF$trt <- rep(c("aC", "eC"), each = length(term))
    
    ### Define GPP dataframe
    term <- c("GPPoverstorey","GPPunderstorey", "GPPtotal")
    
    gppDF <- data.frame(rep(term, 2), NA, NA)
    colnames(gppDF) <- c("term", "trt", "value")
    gppDF$trt <- rep(c("aC", "eC"), each = length(term))
    
 
    ### fine root NPP
    myDF$value[myDF$term=="FineRootNPP" & myDF$trt=="aC"] <- with(fineroot_production_flux[fineroot_production_flux$Ring==2|fineroot_production_flux$Ring==3|fineroot_production_flux$Ring==6,],
                                                               sum(fineroot_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    
    myDF$value[myDF$term=="FineRootNPP" & myDF$trt=="eC"] <- with(fineroot_production_flux[fineroot_production_flux$Ring==1|fineroot_production_flux$Ring==4|fineroot_production_flux$Ring==5,],
                                                               sum(fineroot_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    

    ### Coarse root NPP
    myDF$value[myDF$term=="CoarseRootNPP" & myDF$trt=="aC"] <- with(coarse_root_production_flux_1[coarse_root_production_flux_1$Ring==2|coarse_root_production_flux_1$Ring==3|coarse_root_production_flux_1$Ring==6,],
                                                                    sum(coarse_root_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    
    myDF$value[myDF$term=="CoarseRootNPP" & myDF$trt=="eC"] <- with(coarse_root_production_flux_1[coarse_root_production_flux_1$Ring==1|coarse_root_production_flux_1$Ring==4|coarse_root_production_flux_1$Ring==5,],
                                                                    sum(coarse_root_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv

    ### Rh
    #myDF$value[myDF$term=="RHetero" & myDF$trt=="aC"] <- with(heterotrophic_respiration_flux[heterotrophic_respiration_flux$Ring==2|heterotrophic_respiration_flux$Ring==3|heterotrophic_respiration_flux$Ring==6,],
    #                                                              sum(heterotrophic_respiration_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    
    #myDF$value[myDF$term=="RHetero" & myDF$trt=="eC"] <- with(heterotrophic_respiration_flux[heterotrophic_respiration_flux$Ring==1|heterotrophic_respiration_flux$Ring==4|heterotrophic_respiration_flux$Ring==5,],
    #                                                              sum(heterotrophic_respiration_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    
    ### MycorrhizalProduction
    

    ### Ra root
    myDF$value[myDF$term=="RaRoot" & myDF$trt=="aC"] <- with(root_respiration_flux[root_respiration_flux$Ring==2|root_respiration_flux$Ring==3|root_respiration_flux$Ring==6,],
                                                              sum(root_respiration_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    
    myDF$value[myDF$term=="RaRoot" & myDF$trt=="eC"] <- with(root_respiration_flux[root_respiration_flux$Ring==1|root_respiration_flux$Ring==4|root_respiration_flux$Ring==5,],
                                                              sum(root_respiration_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 

    ### GPP overstorey
    gppDF$value[gppDF$term=="GPPoverstorey" & gppDF$trt=="aC"] <- with(overstorey_gpp_flux[overstorey_gpp_flux$Ring==2|overstorey_gpp_flux$Ring==3|overstorey_gpp_flux$Ring==6,],
                                                                    mean(GPP, na.rm=T)) 
    
    gppDF$value[gppDF$term=="GPPoverstorey" & gppDF$trt=="eC"] <- with(overstorey_gpp_flux[overstorey_gpp_flux$Ring==1|overstorey_gpp_flux$Ring==4|overstorey_gpp_flux$Ring==5,],
                                                                    mean(GPP, na.rm=T))
    
    ### understorey GPP
    gppDF$value[gppDF$term=="GPPunderstorey" & gppDF$trt=="aC"] <- with(understorey_gpp_flux[understorey_gpp_flux$Ring==2|understorey_gpp_flux$Ring==3|understorey_gpp_flux$Ring==6,],
                                                                     mean(GPP, na.rm=T)) 
    
    gppDF$value[gppDF$term=="GPPunderstorey" & gppDF$trt=="eC"] <- with(understorey_gpp_flux[understorey_gpp_flux$Ring==1|understorey_gpp_flux$Ring==4|understorey_gpp_flux$Ring==5,],
                                                                     mean(GPP, na.rm=T))
    
    ### GPP total
    gppDF$value[gppDF$term=="GPPtotal" & gppDF$trt=="aC"] <- gppDF$value[gppDF$term=="GPPoverstorey" & gppDF$trt=="aC"] + gppDF$value[gppDF$term=="GPPunderstorey" & gppDF$trt=="aC"] 
    gppDF$value[gppDF$term=="GPPtotal" & gppDF$trt=="eC"] <- gppDF$value[gppDF$term=="GPPoverstorey" & gppDF$trt=="eC"] + gppDF$value[gppDF$term=="GPPunderstorey" & gppDF$trt=="eC"]
    
    ### Calculate ratio
    myDF$ratio[myDF$term=="FineRootNPP" & myDF$trt=="aC"] <- myDF$value[myDF$term=="FineRootNPP" & myDF$trt=="aC"]/gppDF$value[gppDF$term=="GPPtotal" & gppDF$trt=="aC"] * 100
    myDF$ratio[myDF$term=="FineRootNPP" & myDF$trt=="eC"] <- myDF$value[myDF$term=="FineRootNPP" & myDF$trt=="eC"]/gppDF$value[gppDF$term=="GPPtotal" & gppDF$trt=="eC"] * 100
    
    #myDF$ratio[myDF$term=="RHetero" & myDF$trt=="aC"] <- myDF$value[myDF$term=="RHetero" & myDF$trt=="aC"]/gppDF$value[gppDF$term=="GPPtotal" & gppDF$trt=="aC"] * 100
    #myDF$ratio[myDF$term=="RHetero" & myDF$trt=="eC"] <- myDF$value[myDF$term=="RHetero" & myDF$trt=="eC"]/gppDF$value[gppDF$term=="GPPtotal" & gppDF$trt=="eC"] * 100
    
    myDF$ratio[myDF$term=="MycorrhizalProduction" & myDF$trt=="aC"] <- myDF$value[myDF$term=="MycorrhizalProduction" & myDF$trt=="aC"]/gppDF$value[gppDF$term=="GPPtotal" & gppDF$trt=="aC"] * 100
    myDF$ratio[myDF$term=="MycorrhizalProduction" & myDF$trt=="eC"] <- myDF$value[myDF$term=="MycorrhizalProduction" & myDF$trt=="eC"]/gppDF$value[gppDF$term=="GPPtotal" & gppDF$trt=="eC"] * 100
    
    myDF$ratio[myDF$term=="RaRoot" & myDF$trt=="aC"] <- myDF$value[myDF$term=="RaRoot" & myDF$trt=="aC"]/gppDF$value[gppDF$term=="GPPtotal" & gppDF$trt=="aC"] * 100
    myDF$ratio[myDF$term=="RaRoot" & myDF$trt=="eC"] <- myDF$value[myDF$term=="RaRoot" & myDF$trt=="eC"]/gppDF$value[gppDF$term=="GPPtotal" & gppDF$trt=="eC"] * 100
    
    myDF$ratio[myDF$term=="CoarseRootNPP" & myDF$trt=="aC"] <- myDF$value[myDF$term=="CoarseRootNPP" & myDF$trt=="aC"]/gppDF$value[gppDF$term=="GPPtotal" & gppDF$trt=="aC"] * 100
    myDF$ratio[myDF$term=="CoarseRootNPP" & myDF$trt=="eC"] <- myDF$value[myDF$term=="CoarseRootNPP" & myDF$trt=="eC"]/gppDF$value[gppDF$term=="GPPtotal" & gppDF$trt=="eC"] * 100
    
    ### make the bar plot
    p2 <- ggplot(myDF,
                 aes(trt, ratio)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("Treatment") + ylab("TBCA (%)") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="right",
              legend.text.align=0)
    
    plot(p2)
    
    pdf("R_other/tbca_allocation_comparison.pdf")
    plot(p2)
    dev.off()
    
    
}
