#### To make overstorey  NPP allocation ratio 
#### Look at aCO2 and eCO2
#### Assume a fraction of fineroot is overstorey
make_npp_allocation_ratio <- function() {

    #### Define dataframe
    term <- c("LeafNPP", "StemNPP", "FineRootNPP", "CoarseRootNPP",
              "TwigNPP", "SeedNPP", "BarkNPP", 
              "UnderstoreyNPP", "LeafConsumption",
              "MycorrhizalNPP", "FlowerNPP")
    
    npp <- data.frame(rep(term, 2), NA, NA, NA)
    colnames(npp) <- c("term", "trt", "value", "ratio")
    npp$trt <- rep(c("aC", "eC"), each = length(term))
    
    ### leaf NPP
    npp$value[npp$term=="LeafNPP" & npp$trt=="aC"] <- with(leaflitter_flux[leaflitter_flux$Ring==2|leaflitter_flux$Ring==3|leaflitter_flux$Ring==6,],
                                                           sum(leaf_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv
    
    npp$value[npp$term=="LeafNPP" & npp$trt=="eC"] <- with(leaflitter_flux[leaflitter_flux$Ring==1|leaflitter_flux$Ring==4|leaflitter_flux$Ring==5,],
                                                           sum(leaf_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv
    ### twig NPP
    npp$value[npp$term=="TwigNPP" & npp$trt=="aC"] <- with(leaflitter_flux[leaflitter_flux$Ring==2|leaflitter_flux$Ring==3|leaflitter_flux$Ring==6,],
                                                           sum(twig_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv
    
    npp$value[npp$term=="TwigNPP" & npp$trt=="eC"] <- with(leaflitter_flux[leaflitter_flux$Ring==1|leaflitter_flux$Ring==4|leaflitter_flux$Ring==5,],
                                                           sum(twig_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv
    
    ### bark NPP
    npp$value[npp$term=="BarkNPP" & npp$trt=="aC"] <- with(leaflitter_flux[leaflitter_flux$Ring==2|leaflitter_flux$Ring==3|leaflitter_flux$Ring==6,],
                                                           sum(bark_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv
    
    npp$value[npp$term=="BarkNPP" & npp$trt=="eC"] <- with(leaflitter_flux[leaflitter_flux$Ring==1|leaflitter_flux$Ring==4|leaflitter_flux$Ring==5,],
                                                           sum(bark_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv
    
    ### seed NPP
    npp$value[npp$term=="SeedNPP" & npp$trt=="aC"] <- with(leaflitter_flux[leaflitter_flux$Ring==2|leaflitter_flux$Ring==3|leaflitter_flux$Ring==6,],
                                                           sum(seed_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv

    npp$value[npp$term=="SeedNPP" & npp$trt=="eC"] <- with(leaflitter_flux[leaflitter_flux$Ring==1|leaflitter_flux$Ring==4|leaflitter_flux$Ring==5,],
                                                           sum(seed_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv
    
    ### Wood NPP
    npp$value[npp$term=="StemNPP" & npp$trt=="aC"] <- with(wood_production_flux[wood_production_flux$Ring==2|wood_production_flux$Ring==3|wood_production_flux$Ring==6,],
                                                           sum(wood_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv

    npp$value[npp$term=="StemNPP" & npp$trt=="eC"] <- with(wood_production_flux[wood_production_flux$Ring==1|wood_production_flux$Ring==4|wood_production_flux$Ring==5,],
                                                           sum(wood_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv

    ### fine root NPP
    npp$value[npp$term=="FineRootNPP" & npp$trt=="aC"] <- with(fineroot_production_flux[fineroot_production_flux$Ring==2|fineroot_production_flux$Ring==3|fineroot_production_flux$Ring==6,],
                                                           sum(fineroot_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    
    npp$value[npp$term=="FineRootNPP" & npp$trt=="eC"] <- with(fineroot_production_flux[fineroot_production_flux$Ring==1|fineroot_production_flux$Ring==4|fineroot_production_flux$Ring==5,],
                                                           sum(fineroot_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    

    ### Coarse root NPP
    npp$value[npp$term=="CoarseRootNPP" & npp$trt=="aC"] <- with(coarse_root_production_flux_1[coarse_root_production_flux_1$Ring==2|coarse_root_production_flux_1$Ring==3|coarse_root_production_flux_1$Ring==6,],
                                                               sum(coarse_root_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    
    npp$value[npp$term=="CoarseRootNPP" & npp$trt=="eC"] <- with(coarse_root_production_flux_1[coarse_root_production_flux_1$Ring==1|coarse_root_production_flux_1$Ring==4|coarse_root_production_flux_1$Ring==5,],
                                                               sum(coarse_root_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv

    ### leaf consumption
    npp$value[npp$term=="LeafConsumption" & npp$trt=="aC"] <- with(herbivory_leaf_consumption_flux[herbivory_leaf_consumption_flux$Ring==2|herbivory_leaf_consumption_flux$Ring==3|herbivory_leaf_consumption_flux$Ring==6,],
                                                                 sum(herbivory_leaf_consumption_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    
    npp$value[npp$term=="LeafConsumption" & npp$trt=="eC"] <- with(herbivory_leaf_consumption_flux[herbivory_leaf_consumption_flux$Ring==1|herbivory_leaf_consumption_flux$Ring==4|herbivory_leaf_consumption_flux$Ring==5,],
                                                                 sum(herbivory_leaf_consumption_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv

    ### Understorey aboveground production
    npp$value[npp$term=="UnderstoreyNPP" & npp$trt=="aC"] <- with(understorey_aboveground_production_flux[understorey_aboveground_production_flux$Ring==2|understorey_aboveground_production_flux$Ring==3|understorey_aboveground_production_flux$Ring==6,],
                                                                   sum(understorey_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv 
    
    npp$value[npp$term=="UnderstoreyNPP" & npp$trt=="eC"] <- with(understorey_aboveground_production_flux[understorey_aboveground_production_flux$Ring==1|understorey_aboveground_production_flux$Ring==4|understorey_aboveground_production_flux$Ring==5,],
                                                                   sum(understorey_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T)) * conv
    
    ### MycorrhizalProduction
    
    
    ### FlowerProduction
    
    
    ### Calculate total NPP per treatment
    ac.tot <- sum(npp$value[npp$trt=="aC"], na.rm=T)
    ec.tot <- sum(npp$value[npp$trt=="eC"], na.rm=T)
    
    ### Assign ratio
    npp$ratio[npp$trt=="aC"] <- npp$value[npp$trt=="aC"] / ac.tot
    npp$ratio[npp$trt=="eC"] <- npp$value[npp$trt=="eC"] / ec.tot
    
    ### Make plots
    p1 <- ggplot(npp,
                 aes(trt, value)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("Treatment") + ylab(expression(paste("NPP g C ", m^-2, yr^-1))) +
        scale_x_discrete(labels=c(expression(aCO[2]), expression(eCO[2])))+
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
    
    p2 <- ggplot(npp,
                 aes(trt, ratio)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("Treatment") + ylab("Allocation fraction") +
        scale_x_discrete(labels=c(expression(aCO[2]), expression(eCO[2])))+
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
    
}
