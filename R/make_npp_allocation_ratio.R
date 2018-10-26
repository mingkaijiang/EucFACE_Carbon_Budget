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
    npp$value[npp$term=="LeafNPP" & npp$trt=="aC"] <- with(leaflitter_flux_ann[leaflitter_flux_ann$Ring==2|leaflitter_flux_ann$Ring==3|leaflitter_flux_ann$Ring==6,],
                                                           mean(predicted))
    
    npp$value[npp$term=="LeafNPP" & npp$trt=="eC"] <- with(leaflitter_flux_ann[leaflitter_flux_ann$Ring==1|leaflitter_flux_ann$Ring==4|leaflitter_flux_ann$Ring==5,],
                                                           mean(predicted))
    ### twig NPP
    npp$value[npp$term=="TwigNPP" & npp$trt=="aC"] <- with(twiglitter_flux_ann[twiglitter_flux_ann$Ring==2|twiglitter_flux_ann$Ring==3|twiglitter_flux_ann$Ring==6,],
                                                           mean(predicted))
    
    npp$value[npp$term=="TwigNPP" & npp$trt=="eC"] <- with(twiglitter_flux_ann[twiglitter_flux_ann$Ring==1|twiglitter_flux_ann$Ring==4|twiglitter_flux_ann$Ring==5,],
                                                           mean(predicted))
    
    ### bark NPP
    npp$value[npp$term=="BarkNPP" & npp$trt=="aC"] <- with(barklitter_flux_ann[barklitter_flux_ann$Ring==2|barklitter_flux_ann$Ring==3|barklitter_flux_ann$Ring==6,],
                                                           mean(predicted))
    
    npp$value[npp$term=="BarkNPP" & npp$trt=="eC"] <- with(barklitter_flux_ann[barklitter_flux_ann$Ring==1|barklitter_flux_ann$Ring==4|barklitter_flux_ann$Ring==5,],
                                                           mean(predicted))
    
    ### seed NPP
    npp$value[npp$term=="SeedNPP" & npp$trt=="aC"] <- with(seedlitter_flux_ann[seedlitter_flux_ann$Ring==2|seedlitter_flux_ann$Ring==3|seedlitter_flux_ann$Ring==6,],
                                                           mean(predicted))

    npp$value[npp$term=="SeedNPP" & npp$trt=="eC"] <- with(seedlitter_flux_ann[seedlitter_flux_ann$Ring==1|seedlitter_flux_ann$Ring==4|seedlitter_flux_ann$Ring==5,],
                                                           mean(predicted))
    
    ### Wood NPP
    npp$value[npp$term=="StemNPP" & npp$trt=="aC"] <- with(wood_production_flux_ann[wood_production_flux_ann$Ring==2|wood_production_flux_ann$Ring==3|wood_production_flux_ann$Ring==6,],
                                                           mean(predicted))

    npp$value[npp$term=="StemNPP" & npp$trt=="eC"] <- with(wood_production_flux_ann[wood_production_flux_ann$Ring==1|wood_production_flux_ann$Ring==4|wood_production_flux_ann$Ring==5,],
                                                           mean(predicted))

    ### fine root NPP
    npp$value[npp$term=="FineRootNPP" & npp$trt=="aC"] <- with(fineroot_production_flux_ann[fineroot_production_flux_ann$Ring==2|fineroot_production_flux_ann$Ring==3|fineroot_production_flux_ann$Ring==6,],
                                                               mean(predicted))
    
    npp$value[npp$term=="FineRootNPP" & npp$trt=="eC"] <- with(fineroot_production_flux_ann[fineroot_production_flux_ann$Ring==1|fineroot_production_flux_ann$Ring==4|fineroot_production_flux_ann$Ring==5,],
                                                               mean(predicted))
    

    ### Coarse root NPP
    npp$value[npp$term=="CoarseRootNPP" & npp$trt=="aC"] <- with(coarse_root_production_flux_ann[coarse_root_production_flux_ann$Ring==2|coarse_root_production_flux_ann$Ring==3|coarse_root_production_flux_ann$Ring==6,],
                                                                 mean(predicted))
    
    npp$value[npp$term=="CoarseRootNPP" & npp$trt=="eC"] <- with(coarse_root_production_flux_ann[coarse_root_production_flux_ann$Ring==1|coarse_root_production_flux_ann$Ring==4|coarse_root_production_flux_ann$Ring==5,],
                                                                 mean(predicted))

    ### leaf consumption
    npp$value[npp$term=="LeafConsumption" & npp$trt=="aC"] <- with(herbivory_leaf_consumption_flux_ann[herbivory_leaf_consumption_flux_ann$Ring==2|herbivory_leaf_consumption_flux_ann$Ring==3|herbivory_leaf_consumption_flux_ann$Ring==6,],
                                                                   mean(predicted))
    
    npp$value[npp$term=="LeafConsumption" & npp$trt=="eC"] <- with(herbivory_leaf_consumption_flux_ann[herbivory_leaf_consumption_flux_ann$Ring==1|herbivory_leaf_consumption_flux_ann$Ring==4|herbivory_leaf_consumption_flux_ann$Ring==5,],
                                                                   mean(predicted))

    ### Understorey aboveground production
    npp$value[npp$term=="UnderstoreyNPP" & npp$trt=="aC"] <- with(understorey_aboveground_production_flux_ann[understorey_aboveground_production_flux_ann$Ring==2|understorey_aboveground_production_flux_ann$Ring==3|understorey_aboveground_production_flux_ann$Ring==6,],
                                                                  mean(predicted))
    
    npp$value[npp$term=="UnderstoreyNPP" & npp$trt=="eC"] <- with(understorey_aboveground_production_flux_ann[understorey_aboveground_production_flux_ann$Ring==1|understorey_aboveground_production_flux_ann$Ring==4|understorey_aboveground_production_flux_ann$Ring==5,],
                                                                  mean(predicted))
    
    ### MycorrhizalProduction
    
    
    ### Calculate total NPP per treatment
    ac.tot <- sum(npp$value[npp$trt=="aC"], na.rm=T)
    ec.tot <- sum(npp$value[npp$trt=="eC"], na.rm=T)
    
    ### Assign ratio
    npp$ratio[npp$trt=="aC"] <- npp$value[npp$trt=="aC"] / ac.tot
    npp$ratio[npp$trt=="eC"] <- npp$value[npp$trt=="eC"] / ec.tot
    
    npp$term <- factor(npp$term, levels=unique(npp$term))
    
    
    require(viridis)
    col.list1 <- viridis(9)
    
    ### Prepare variable labels
    var.labs1 <- c(expression(NPP[leaf]), expression(NPP[wood]),
                   expression(NPP[froot]), expression(NPP[croot]),
                   expression(NPP[twig]), expression(NPP[seed]),
                   expression(NPP[bark]), expression(NPP[ua]),
                   expression(NPP[hb]))
    
    ## Calculate change in allocation
    delta1 <- (npp[npp$trt=="eC","value"]-npp[npp$trt=="aC","value"])
    #delta2 <- (npp[npp$trt=="eC","value"]-npp[npp$trt=="aC","value"])/npp[npp$trt=="aC","value"]*100
    
    delta2 <- (npp[npp$trt=="eC","ratio"]-npp[npp$trt=="aC","ratio"])*100
    deltaDF <- data.frame(npp$term[npp$trt=="aC"], delta1, delta2)
    colnames(deltaDF) <- c("term", "delta.abs", "delta.pct")
    deltaDF <- deltaDF[complete.cases(deltaDF$delta.abs),]
    deltaDF$term <- factor(deltaDF$term, levels=unique(deltaDF$term))
    
    ### Make plots
    p1 <- ggplot(npp,
                 aes(trt, value)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("Treatment") + ylab(expression(paste("NPP (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(aCO[2]), expression(eCO[2])))+
        theme_linedraw() +
        scale_fill_manual(name="NPP", 
                          values = col.list1,
                          labels=var.labs1) +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none",
              legend.text.align=0)
    
    p2 <- ggplot(npp,
                 aes(trt, ratio*100)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("Treatment") + ylab("NPP allocation fraction (%)") +
        scale_x_discrete(labels=c(expression(aCO[2]), expression(eCO[2])))+
        theme_linedraw() +
        scale_fill_manual(name="NPP", 
                          values = col.list1,
                          labels=var.labs1) +
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
    
    p3 <- ggplot(deltaDF,
                 aes(term, delta.abs)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("Variable") + ylab(expression(paste("Change in allocation (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("","","","","","","","",""))+
        theme_linedraw() +
        scale_fill_manual(name="NPP", 
                          values = col.list1,
                          labels=var.labs1) +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none",
              legend.text.align=0)
    
    p4 <- ggplot(deltaDF,
                 aes(term, delta.pct)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("Variable") + ylab("Change in allocation (%)") +
        scale_x_discrete(labels=c("","","","","","","","",""))+
        theme_linedraw() +
        scale_fill_manual(name="NPP", 
                          values = col.list1,
                          labels=var.labs1) +
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

    require(grid)
    require(cowplot)
    
    pdf("output/NPP_allocation.pdf", width=10,height=8)
    plot_grid(p1, p2, p3, p4, labels="AUTO", ncol=2, align="v", axis="l",
              rel_widths=c(1,1.2, 1, 1.2))
    dev.off()
    
    
    
    
    ### subset only 2015, but ring-specific values
    npp <- data.frame(rep(term, 6), NA, NA, NA)
    colnames(npp) <- c("term", "ring", "value", "ratio")
    npp$ring <- rep(c(1:6), each = length(term))
    
    for (i in 1:6) {
        ### leaf NPP
        npp$value[npp$term=="LeafNPP" & npp$ring==i] <- with(leaflitter_flux_ann[leaflitter_flux_ann$Ring==i&leaflitter_flux_ann$Yr=="2015",],
                                                             mean(predicted))
        
        ### twig NPP
        npp$value[npp$term=="TwigNPP" & npp$ring==i] <- with(twiglitter_flux_ann[twiglitter_flux_ann$Ring==i&twiglitter_flux_ann$Yr=="2015",],
                                                             mean(predicted))
        
        ### bark NPP
        npp$value[npp$term=="BarkNPP" & npp$ring==i] <- with(barklitter_flux_ann[barklitter_flux_ann$Ring==i&barklitter_flux_ann$Yr=="2015",],
                                                             mean(predicted))
        
        ### seed NPP
        npp$value[npp$term=="SeedNPP" & npp$ring==i] <- with(seedlitter_flux_ann[seedlitter_flux_ann$Ring==i&seedlitter_flux_ann$Yr=="2015",],
                                                             mean(predicted))
        
        ### Wood NPP
        npp$value[npp$term=="StemNPP" & npp$ring==i] <- with(wood_production_flux_ann[wood_production_flux_ann$Ring==i&wood_production_flux_ann$Yr=="2015",],
                                                             mean(predicted))
        
        ### fine root NPP
        npp$value[npp$term=="FineRootNPP" & npp$ring==i] <- with(fineroot_production_flux_ann[fineroot_production_flux_ann$Ring==i&fineroot_production_flux_ann$Yr=="2015",],
                                                                 mean(predicted))
        
        ### Coarse root NPP
        npp$value[npp$term=="CoarseRootNPP" & npp$ring==i] <- with(coarse_root_production_flux_ann[coarse_root_production_flux_ann$Ring==i&coarse_root_production_flux_ann$Yr=="2015",],
                                                                   mean(predicted))
        
        ### leaf consumption
        npp$value[npp$term=="LeafConsumption" & npp$ring==i] <- with(herbivory_leaf_consumption_flux_ann[herbivory_leaf_consumption_flux_ann$Ring==i&herbivory_leaf_consumption_flux_ann$Yr=="2015",],
                                                                     mean(predicted))
        
        ### Understorey aboveground production
        npp$value[npp$term=="UnderstoreyNPP" & npp$ring==i] <- with(understorey_aboveground_production_flux_ann[understorey_aboveground_production_flux_ann$Ring==i&understorey_aboveground_production_flux_ann$Yr=="2015",],
                                                                    mean(predicted))   
    }
    

    ### Calculate total NPP per treatment
    r1.tot <- sum(npp$value[npp$ring=="1"], na.rm=T)
    r2.tot <- sum(npp$value[npp$ring=="2"], na.rm=T)
    r3.tot <- sum(npp$value[npp$ring=="3"], na.rm=T)
    r4.tot <- sum(npp$value[npp$ring=="4"], na.rm=T)
    r5.tot <- sum(npp$value[npp$ring=="5"], na.rm=T)
    r6.tot <- sum(npp$value[npp$ring=="6"], na.rm=T)
    
    ### Assign ratio
    npp$ratio[npp$ring=="1"] <- npp$value[npp$ring=="1"] / r1.tot
    npp$ratio[npp$ring=="2"] <- npp$value[npp$ring=="2"] / r2.tot
    npp$ratio[npp$ring=="3"] <- npp$value[npp$ring=="3"] / r3.tot
    npp$ratio[npp$ring=="4"] <- npp$value[npp$ring=="4"] / r4.tot
    npp$ratio[npp$ring=="5"] <- npp$value[npp$ring=="5"] / r5.tot
    npp$ratio[npp$ring=="6"] <- npp$value[npp$ring=="6"] / r6.tot
    
    npp$term <- factor(npp$term, levels=unique(npp$term))
    
    ## Calculate change in allocation
    delta1 <- (npp[npp$trt=="eC","value"]-npp[npp$trt=="aC","value"])
    delta2 <- (npp[npp$trt=="eC","ratio"]-npp[npp$trt=="aC","ratio"])*100
    deltaDF <- data.frame(npp$term[npp$trt=="aC"], delta1, delta2)
    colnames(deltaDF) <- c("term", "delta.abs", "delta.pct")
    deltaDF <- deltaDF[complete.cases(deltaDF$delta.abs),]
    deltaDF$term <- factor(deltaDF$term, levels=unique(deltaDF$term))
    
    ### Make plots
    p1 <- ggplot(npp,
                 aes(trt, value)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("Treatment") + ylab(expression(paste("NPP (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(aCO[2]), expression(eCO[2])))+
        theme_linedraw() +
        scale_fill_manual(name="NPP", 
                          values = col.list1,
                          labels=var.labs1) +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none",
              legend.text.align=0)
  
    p2 <- ggplot(deltaDF,
                 aes(term, delta.pct)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("Variable") + ylab("Change in allocation (%)") +
        scale_x_discrete(labels=c("","","","","","","","",""))+
        theme_linedraw() +
        scale_fill_manual(name="NPP", 
                          values = col.list1,
                          labels=var.labs1) +
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
    
}

