make_eCO2_effect_on_GPP_plot_with_covariate_predicted_alternative_plot_color_based_on_ring_level_data <- function(inDF) {
    
    ######### Plot abs, no interactions, and change in pools
    ### read in the csv file to plot the treatment effect and confidence interval
    myDF <- rbind(inDF$inout, inDF$npp, inDF$pool, inDF$delta_pool)
    
    myDF$diff_mean <- myDF$eCO2 - myDF$aCO2
    myDF$diff_sd <- sqrt((myDF$aCO2_sd^2)/3 + (myDF$eCO2_sd^2)/3)
    
    myDF <- myDF[complete.cases(myDF$diff_mean),]
    
    myDF$term <- c("over_gpp", "understorey_gpp", "ch4",
                   "over_leaf_respiration", "wood_respiration", "root_respiration",
                   "understorey_respiration","voc", "herbivory_respiration",
                   "doc", "soil_respiration", "growth_respiration",
                   "leaf_prod", "wood_prod", "fineroot_prod",
                   "coarseroot_prod", "other_prod", "understorey_prod", 
                   "understorey_lit", "frass_prod", "herb_consump", #"mycorrhizal_prod",
                    "hetero_respiration", "over_leaf", "wood", "und_aboveground",
                   "fineroot", "coarseroot", "litter", "cwd", "microbe",
                   "soil", "mycorrhizae", "insects", 
                   "delta_leaf_c", "delta_wood_c", "delta_understorey_c",
                   "delta_fineroot_c", "delta_coarseroot_c", 
                   "delta_litter_c","delta_microbial_c","delta_soil_c",
                   "delta_mycorrhizal_c", "delta_insect_c")
    
    #### exclude all pools
    myDF$Category <- c(rep("gpp", 2),  
                       rep("resp", 10), 
                       rep("prod", 6),
                       rep("litter", 2), 
                       rep("prod", 1), 
                       rep("resp", 1), 
                       rep("pool", 11), 
                       rep("change_in_pool", 10))  
    
    ### Drop redundant pools and fluxes
    myDF <- subset(myDF, term != c("understorey_lit"))
    
    ### Drop CWD - confidence interval too large
    myDF <- subset(myDF, term != c("cwd"))
    
    myDF <- myDF[,c("term", "diff_mean", "diff_sd", "Category")]
    colnames(myDF) <- c("Variable", "effect_size", "sd", "Category")
    myDF$conf_low <- myDF$effect_size - myDF$sd
    myDF$conf_high <- myDF$effect_size + myDF$sd
    
    ### Subset GPP, NPP, change in pools, and out fluxes
    plotDF1 <- subset(myDF, Variable %in% c("over_gpp", "understorey_gpp", "ch4",
                                            "leaf_prod", "other_prod", "wood_prod", "fineroot_prod",
                                            "coarseroot_prod", "understorey_prod", "herb_consump", 
                                            "over_leaf_respiration", "wood_respiration", "root_respiration",
                                            "understorey_respiration", "hetero_respiration","doc", "voc", "growth_respiration",
                                            "delta_leaf_c", "delta_wood_c", "delta_fineroot_c", 
                                            "delta_coarseroot_c", "delta_understorey_c", 
                                            "delta_litter_c","delta_soil_c","delta_microbial_c",
                                            "delta_mycorrhizal_c", "delta_litter_c", "delta_insect_c"))
    
    ### Add plot category
    plotDF1$plot.cat[plotDF1$Category=="gpp"] <- "Influxes"
    plotDF1$plot.cat[plotDF1$Category=="change_in_pool"] <- "Change_in_pools"
    plotDF1$plot.cat[plotDF1$Category=="resp"] <- "Outfluxes"
    plotDF1$plot.cat[plotDF1$Category=="prod"] <- "NPP"
    plotDF1$plot.cat[plotDF1$Variable=="ch4"] <- "Influxes"
    
    plotDF1 <- plotDF1[,c("Variable", "effect_size", "conf_low", "conf_high", "plot.cat", "sd")]
    
    plotDF1$plot.cat[plotDF1$Variable%in%c("root_respiration", "understorey_respiration", "growth_respiration",
                                           "over_leaf_respiration", "wood_respiration", "voc")] <- "Ra"
    plotDF1$plot.cat[plotDF1$Variable=="herb_consump"] <- "NPP"
    
    
    ### Calculate totals of each plot.cat
    plotDF2 <- summaryBy(effect_size+conf_low+conf_high~plot.cat, data=plotDF1, FUN=sum, keep.names=T, na.rm=T)
    names(plotDF2)[1] <- "Variable"
    
    ### the confidence above is not correct, update these numbers
    plotDF2[plotDF2$Variable == "Change_in_pools", "sd"] <- sqrt((plotDF1[plotDF1$Variable=="delta_leaf_c","sd"]^2 + 
                    plotDF1[plotDF1$Variable=="delta_wood_c","sd"]^2 +
                    plotDF1[plotDF1$Variable=="delta_understorey_c","sd"]^2 +
                    plotDF1[plotDF1$Variable=="delta_fineroot_c","sd"]^2 + 
                    plotDF1[plotDF1$Variable=="delta_coarseroot_c","sd"]^2 +
                    plotDF1[plotDF1$Variable=="delta_litter_c","sd"]^2 +
                    plotDF1[plotDF1$Variable=="delta_microbial_c","sd"]^2 +
                    plotDF1[plotDF1$Variable=="delta_soil_c","sd"]^2 +
                    plotDF1[plotDF1$Variable=="delta_mycorrhizal_c","sd"]^2 +
                    plotDF1[plotDF1$Variable=="delta_insect_c","sd"]^2)/10)
    
    plotDF2[plotDF2$Variable == "Influxes", "sd"] <- sqrt((plotDF1[plotDF1$Variable=="over_gpp","sd"]^2 + 
                                                                     plotDF1[plotDF1$Variable=="understorey_gpp","sd"]^2 +
                                                                     plotDF1[plotDF1$Variable=="ch4","sd"]^2)/3)
    
    plotDF2[plotDF2$Variable == "NPP", "sd"] <- sqrt((plotDF1[plotDF1$Variable=="leaf_prod","sd"]^2 + 
                                                                     plotDF1[plotDF1$Variable=="wood_prod","sd"]^2 +
                                                                     plotDF1[plotDF1$Variable=="fineroot_prod","sd"]^2 +
                                                                     plotDF1[plotDF1$Variable=="coarseroot_prod","sd"]^2 + 
                                                                     plotDF1[plotDF1$Variable=="other_prod","sd"]^2 +
                                                                     plotDF1[plotDF1$Variable=="understorey_prod","sd"]^2 +
                                                                     plotDF1[plotDF1$Variable=="herb_consump","sd"]^2)/7)
    
    plotDF2[plotDF2$Variable == "Outfluxes", "sd"] <- sqrt((plotDF1[plotDF1$Variable=="doc","sd"]^2 + 
                                                                     plotDF1[plotDF1$Variable=="hetero_respiration","sd"]^2)/2)
    
    plotDF2[plotDF2$Variable == "Ra", "sd"] <- sqrt((plotDF1[plotDF1$Variable=="over_leaf_respiration","sd"]^2 + 
                                                                     plotDF1[plotDF1$Variable=="wood_respiration","sd"]^2 +
                                                                     plotDF1[plotDF1$Variable=="root_respiration","sd"]^2 +
                                                                     plotDF1[plotDF1$Variable=="understorey_respiration","sd"]^2 + 
                                                                     plotDF1[plotDF1$Variable=="voc","sd"]^2 +
                                                                     plotDF1[plotDF1$Variable=="growth_respiration","sd"]^2)/6)
    
    
    ### Total of Ra and Rh and the conf. interval
    tot_out <- data.frame("total_outflux", 
                          plotDF2$effect_size[plotDF2$Variable=="Outfluxes"]+plotDF2$effect_size[plotDF2$Variable=="Ra"],
                          plotDF2$conf_low[plotDF2$Variable=="Outfluxes"]+plotDF2$conf_low[plotDF2$Variable=="Ra"],
                          plotDF2$conf_high[plotDF2$Variable=="Outfluxes"]+plotDF2$conf_high[plotDF2$Variable=="Ra"],
                          0)
    colnames(tot_out) <- c("Variable", "effect_size", "conf_low", "conf_high", "sd")
    plotDF2 <- rbind(plotDF2, tot_out) 
    
    plotDF2[plotDF2$Variable == "total_outflux", "sd"] <- sqrt((plotDF1[plotDF1$Variable=="over_leaf_respiration","sd"]^2 + 
                                                                   plotDF1[plotDF1$Variable=="wood_respiration","sd"]^2 +
                                                                   plotDF1[plotDF1$Variable=="root_respiration","sd"]^2 +
                                                                   plotDF1[plotDF1$Variable=="understorey_respiration","sd"]^2 + 
                                                                   plotDF1[plotDF1$Variable=="voc","sd"]^2 +
                                                                   plotDF1[plotDF1$Variable=="growth_respiration","sd"]^2+
                                                                   plotDF1[plotDF1$Variable=="doc","sd"]^2 + 
                                                                   plotDF1[plotDF1$Variable=="hetero_respiration","sd"]^2)/8)
    
    
    plotDF2$plot.cat <- c("total_change_in_pool", "total_influx", "total_npp", "total_rh", "total_ra", "total_outflux")
    
    ### Combine the plots
    plotDF3 <- plotDF1[,c("Variable", "effect_size", "conf_low", "conf_high", "sd", "plot.cat")]
    plotDF <- rbind(plotDF3, plotDF2[c(1,3,5,6),])
    
    ### Assign plot cat 2
    plotDF$plot.cat2[plotDF$plot.cat=="Influxes"] <- "A"
    plotDF$plot.cat2[plotDF$plot.cat=="total_npp"] <- "B"
    plotDF$plot.cat2[plotDF$plot.cat=="total_ra"] <- "B"
    plotDF$plot.cat2[plotDF$plot.cat=="total_outflux"] <- "C"
    plotDF$plot.cat2[plotDF$plot.cat=="total_change_in_pool"] <- "C"
    plotDF$plot.cat2[plotDF$plot.cat=="NPP"] <- "D"
    plotDF$plot.cat2[plotDF$plot.cat=="Ra"] <- "E"
    plotDF$plot.cat2[plotDF$plot.cat=="Outfluxes"] <- "E"
    plotDF$plot.cat2[plotDF$plot.cat=="Change_in_pools"] <- "F"
    
    ### exclude variables with averaged CO2 effect < 1 g m-2 yr-1
    # plotDF <- subset(plotDF, effect_size >= 1)
    
    ### Order plot DF
    plotDF <- plotDF[order(plotDF$plot.cat2),]
    
    ### Plot variable ordering
    plotDF$var.order <- 1:length(plotDF$plot.cat2)
    
    ### confidence interval for each plot.cat2 categories
    ### Need to separately calculate confidence interval using sums of each category
    confDF <- summaryBy(effect_size+sd~plot.cat2, keep.names=T, na.rm=T, data=plotDF, FUN=sum)
    
    ## update A influxes
    confDF$sd[confDF$plot.cat2 == "A"] <- plotDF2$sd[plotDF2$Variable=="Influxes"]
    confDF$conf_low[confDF$plot.cat2 == "A"] <- plotDF2$conf_low[plotDF2$Variable=="Influxes"]
    confDF$conf_high[confDF$plot.cat2 == "A"] <- plotDF2$conf_high[plotDF2$Variable=="Influxes"]
    
    ## update D NPP
    confDF$sd[confDF$plot.cat2 == "D"] <- plotDF2$sd[plotDF2$Variable=="NPP"]
    confDF$conf_low[confDF$plot.cat2 == "D"] <- plotDF2$conf_low[plotDF2$Variable=="NPP"]
    confDF$conf_high[confDF$plot.cat2 == "D"] <- plotDF2$conf_high[plotDF2$Variable=="NPP"]
    
    ## update E total outfluxes
    confDF$sd[confDF$plot.cat2 == "E"] <- plotDF2$sd[plotDF2$Variable=="total_outflux"]
    confDF$conf_low[confDF$plot.cat2 == "E"] <- plotDF2$conf_low[plotDF2$Variable=="total_outflux"]
    confDF$conf_high[confDF$plot.cat2 == "E"] <- plotDF2$conf_high[plotDF2$Variable=="total_outflux"]
    
    ## update F change in pools
    confDF$sd[confDF$plot.cat2 == "F"] <- plotDF2$sd[plotDF2$Variable=="Change_in_pools"]
    confDF$conf_low[confDF$plot.cat2 == "F"] <- plotDF2$conf_low[plotDF2$Variable=="Change_in_pools"]
    confDF$conf_high[confDF$plot.cat2 == "F"] <- plotDF2$conf_high[plotDF2$Variable=="Change_in_pools"]
    
    ## update B NPP + Ra
    confDF$sd[confDF$plot.cat2 == "B"] <- sqrt((plotDF1[plotDF1$Variable=="leaf_prod","sd"]^2 + 
                                                   plotDF1[plotDF1$Variable=="wood_prod","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="fineroot_prod","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="coarseroot_prod","sd"]^2 + 
                                                   plotDF1[plotDF1$Variable=="other_prod","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="understorey_prod","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="herb_consump","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="over_leaf_respiration","sd"]^2 + 
                                                   plotDF1[plotDF1$Variable=="wood_respiration","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="root_respiration","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="understorey_respiration","sd"]^2 + 
                                                   plotDF1[plotDF1$Variable=="voc","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="growth_respiration","sd"]^2)/13)
    
    confDF <- as.data.frame(confDF)
    
    confDF[confDF$plot.cat2 == "B", "conf_low"]  <- as.numeric(confDF[confDF$plot.cat2 == "B", "effect_size"]) -
        as.numeric(confDF[confDF$plot.cat2 == "B", "sd"])
    confDF[confDF$plot.cat2 == "B", "conf_high"]  <- as.numeric(confDF[confDF$plot.cat2 == "B", "effect_size"]) +
        as.numeric(confDF[confDF$plot.cat2 == "B", "sd"])
    
    ## update C change in pool + total outfluxes
    confDF$sd[confDF$plot.cat2 == "C"] <- sqrt((plotDF1[plotDF1$Variable=="delta_leaf_c","sd"]^2 + 
                                                   plotDF1[plotDF1$Variable=="delta_wood_c","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="delta_understorey_c","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="delta_fineroot_c","sd"]^2 + 
                                                   plotDF1[plotDF1$Variable=="delta_coarseroot_c","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="delta_litter_c","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="delta_microbial_c","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="delta_soil_c","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="delta_mycorrhizal_c","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="delta_insect_c","sd"]^2+
                                                   plotDF1[plotDF1$Variable=="over_leaf_respiration","sd"]^2 + 
                                                   plotDF1[plotDF1$Variable=="wood_respiration","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="root_respiration","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="understorey_respiration","sd"]^2 + 
                                                   plotDF1[plotDF1$Variable=="voc","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="growth_respiration","sd"]^2 +
                                                   plotDF1[plotDF1$Variable=="doc","sd"]^2 + 
                                                   plotDF1[plotDF1$Variable=="hetero_respiration","sd"]^2)/18)
    
    
    confDF[confDF$plot.cat2 == "C", "conf_low"]  <- as.numeric(confDF[confDF$plot.cat2 == "C", "effect_size"]) -
        as.numeric(confDF[confDF$plot.cat2 == "C", "sd"])
    confDF[confDF$plot.cat2 == "C", "conf_high"]  <- as.numeric(confDF[confDF$plot.cat2 == "C", "effect_size"]) +
        as.numeric(confDF[confDF$plot.cat2 == "C", "sd"])
    
    soil_resp_mean <- myDF[myDF$Variable=="soil_respiration", "effect_size"]
    soil_resp_conf_low <- myDF[myDF$Variable=="soil_respiration", "effect_size"]-myDF[myDF$Variable=="soil_respiration", "sd"] 
    soil_resp_conf_high  <- myDF[myDF$Variable=="soil_respiration", "effect_size"]+myDF[myDF$Variable=="soil_respiration", "sd"] 
    
    
    y.lab1 <- c("ch4"=expression(CH[4]),                          # 1
                "over_gpp"=expression(GPP[o]),                    # 2
                "understorey_gpp"=expression(GPP[u]),             # 3
                "Change_in_pools"=expression(Delta*C[pools]),
                "NPP"="NPP",                                      # 4
                "total_outflux"="R",                                  # 5
                "Ra"=expression(R[a]),                            # 6
                "herb_consump"=expression(NPP[insect]),           # 7
                "leaf_prod"=expression(NPP[leaf]),                # 8
                "other_prod"=expression(NPP[other]),                # 9
                "wood_prod"=expression(NPP[stem]),                # 12
                "fineroot_prod"=expression(NPP[froot]),           # 13
                "coarseroot_prod"=expression(NPP[croot]),         # 14
                "understorey_prod"=expression(NPP[ua]),           # 15
                "root_respiration"=expression(R[root]),           # 16
                "understorey_respiration"=expression(R[ua]),      # 17
                "growth_respiration"=expression(R[growth]),
                "doc"=expression(DOC),                            # 18
                "voc"=expression(VOC),
                "hetero_respiration"=expression(R[rh]),            # 19
                "over_leaf_respiration"=expression(R[leaf]),      # 20
                "wood_respiration"=expression(R[stem]),           # 21
                "delta_soil_c"=expression(Delta*C[soil]),         # 22
                "delta_leaf_c"=expression(Delta*C[leaf]),         # 23
                "delta_wood_c"=expression(Delta*C[stem]),         # 24
                "delta_fineroot_c"=expression(Delta*C[froot]),    # 25
                "delta_coarseroot_c"=expression(Delta*C[croot]),  # 26
                "delta_understorey_c"=expression(Delta*C[ua]),    # 27
                "delta_litter_c"=expression(Delta*C[lit]))     # 28
    
    
    ## gpp
    colfunc.inf <- colorRampPalette(c("black", "grey"))
    A.col.list <- colfunc.inf(3)
    
    # 2nd column bar - NPP + Ra 
    colfunc.inf <- colorRampPalette(c("darkgreen", "darkred"))
    B.col.list <- colfunc.inf(2)
    
    # 2nd column bar - Change in pools + all R
    colfunc.inf <- colorRampPalette(c("red", "blue"))
    C.col.list <- colfunc.inf(2)
    
    ## NPP
    colfunc.npp <- colorRampPalette(c("darkgreen", "yellow"))
    D.col.list <- colfunc.npp(7)
    
    ## R
    colfunc.R <- colorRampPalette(c("darkred", "pink"))
    E.col.list <- colfunc.R(7)
    
    ### Change in pools
    colfunc.delta <- colorRampPalette(c("darkblue", "cyan"))
    F.col.list <- colfunc.delta(10)
    
    ### Combine all color list
    col.list1 <- c("ch4"=A.col.list[1], 
                   "over_gpp"=A.col.list[2],                    
                   "understorey_gpp"=A.col.list[3], 
                   "Change_in_pools"=C.col.list[1],   
                   "NPP"=B.col.list[1],   
                   "total_outflux"=C.col.list[2],
                   "Ra"=B.col.list[2],      
                   
                   "herb_consump"=D.col.list[1],           
                   "leaf_prod"=D.col.list[2],                
                   "other_prod"=D.col.list[3],                
                   "wood_prod"=D.col.list[4],               
                   "fineroot_prod"=D.col.list[5],           
                   "coarseroot_prod"=D.col.list[6],         
                   "understorey_prod"=D.col.list[7],
                   
                   "over_leaf_respiration"=E.col.list[1],      
                   "wood_respiration"=E.col.list[2],           
                   "root_respiration"=E.col.list[3],           
                   "understorey_respiration"=E.col.list[4],      
                   "hetero_respiration"=E.col.list[5],            
                   "doc"=E.col.list[6],     
                   "voc"=E.col.list[7], 
                   "growth_respiration"=E.col.list[8],
                   
                   "delta_leaf_c"=F.col.list[1],         
                   "delta_wood_c"=F.col.list[2],        
                   "delta_fineroot_c"=F.col.list[3],    
                   "delta_coarseroot_c"=F.col.list[4],  
                   "delta_understorey_c"=F.col.list[5],    
                   "delta_soil_c"=F.col.list[6],         
                   "delta_litter_c"=F.col.list[7],
                   "delta_microbial_c"=F.col.list[8],
                   "delta_mycorrhizal_c"=F.col.list[9],
                   "delta_insect_c"=F.col.list[10])                         
    
    #### Plotting
    p1 <- ggplot(plotDF,
                 aes(plot.cat2, effect_size)) +   
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("Influxes", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste(Delta*C[pools], "+R")),
                                  "NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF$Variable,
                          values = col.list1,
                          labels=y.lab1) +
        theme_linedraw() +
        ylim(-500,1000)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)
    
    p2 <- ggplot(plotDF,
                 aes(plot.cat2, effect_size)) +   
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        geom_errorbar(data=confDF, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), width=0.2, size=1, color="grey") + 
        geom_point(data=confDF, mapping=aes(x=plot.cat2, y=effect_size), size=4, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("Influxes", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste(Delta*C[pools], "+R")),
                                  "NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF$Variable,
                          values = col.list1,
                          labels=y.lab1) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)
    
    
    ### Simplify the plot by eleminating fluxes that has CO2 effect < 1 g m-2 yr-1
    plotDF2 <- subset(plotDF, abs(effect_size) >= 1)
    
    ### Order plot DF
    plotDF2 <- plotDF2[order(plotDF2$plot.cat2),]
    
    ### Plot variable ordering
    plotDF2$var.order <- 1:length(plotDF2$plot.cat2)
    
    library(viridis)
    
    ## gpp
    colfunc.inf <- colorRampPalette(c( "grey21","dimgrey"))
    A.col.list <- colfunc.inf(2)
    
    # 2nd column bar - NPP + Ra 
    colfunc.inf <- colorRampPalette(c("green4", "orange"))
    B.col.list <- colfunc.inf(2)
    
    # 3rd column bar - Change in pools + all R
    colfunc.inf <- colorRampPalette(c("blue", "orangered"))
    C.col.list <- colfunc.inf(2)
    
    ## NPP
    #colfunc.npp <- colorRampPalette(c("seagreen", "green", "yellowgreen"))
    #D.col.list <- colfunc.npp(9)
    D.col.list <- c("darkgreen", "seagreen", "chartreuse3", "yellowgreen", "chartreuse1", "springgreen3")#, "springgreen1")
    
    ## R
    colfunc.R <- colorRampPalette(c("darkred", "pink"))
    E.col.list <- colfunc.R(5)
    
    ### Change in pools
    colfunc.delta <- colorRampPalette(c("darkblue", "cyan"))
    F.col.list <- colfunc.delta(6)
    
    v.list <- viridis(23)
    #v.list <- rainbow(26)
    
    ### Combine all color list
    col.list2 <- c("over_gpp"=A.col.list[1],                    
                   "understorey_gpp"=A.col.list[2], 
                   "NPP"=B.col.list[1],   
                   "Ra"=B.col.list[2],      
                   "Change_in_pools"=C.col.list[1],   
                   "total_outflux"=C.col.list[2],
                   "leaf_prod"=D.col.list[1],                
                   "wood_prod"=D.col.list[2],               
                   "fineroot_prod"=D.col.list[3],           
                   "other_prod"=D.col.list[4],                
                   "understorey_prod"=D.col.list[5], 
                   "herb_consump"=D.col.list[6],           
                   "over_leaf_respiration"=E.col.list[4],      
                   "wood_respiration"=E.col.list[5],           
                   "root_respiration"=E.col.list[1],           
                   "understorey_respiration"=E.col.list[2],      
                   "hetero_respiration"=E.col.list[3],    
                   "delta_leaf_c"=F.col.list[1],  
                   "delta_wood_c"=F.col.list[2],        
                   "delta_fineroot_c"=F.col.list[3],    
                   "delta_litter_c"=F.col.list[4],
                   "delta_microbial_c"=F.col.list[5],
                   "delta_soil_c"=F.col.list[6])     
        
    
    # y label
    y.lab2 <- c("over_gpp"=expression(GPP[o]),                    # 1
                "understorey_gpp"=expression(GPP[u]),             # 2
                "NPP"="NPP",                                      # 3
                "Ra"=expression(R[a]),                            # 5
                "Change_in_pools"=expression(Delta*C[pools]),
                "total_outflux"="R",                              # 4
                "leaf_prod"=expression(NPP[leaf]),                # 7
                "wood_prod"=expression(NPP[stem]),                # 11
                "fineroot_prod"=expression(NPP[froot]),           # 12
                "other_prod"=expression(NPP[other]),                # 8
                "understorey_prod"=expression(NPP[ua]),           # 14
                "herb_consump"=expression(NPP[insect]),           # 6
                "over_leaf_respiration"=expression(R[leaf]),      # 18
                "wood_respiration"=expression(R[stem]),           # 19
                "root_respiration"=expression(R[root]),           # 15
                "understorey_respiration"=expression(R[ua]),      # 16
                "hetero_respiration"=expression(R[rh]),            # 17
                "delta_leaf_c"=expression(Delta*C[leaf]),
                "delta_wood_c"=expression(Delta*C[stem]),         # 21
                "delta_fineroot_c"=expression(Delta*C[froot]),    # 22
                "delta_litter_c"=expression(Delta*C[lit]),
                "delta_microbial_c"=expression(Delta*C[micr]),
                "delta_soil_c"=expression(Delta*C[soil]))     # 25
    
    
    
    p3 <- ggplot(plotDF2,
                 aes(plot.cat2, effect_size)) +  
        geom_hline(yintercept=0)+
        #geom_hline(yintercept=150, linetype="dashed", color="grey")+
        #geom_hline(yintercept=-50, linetype="dashed", color="grey")+
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        geom_errorbar(data=confDF, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
                      width=0.1, size=1, color="grey") + 
        geom_point(data=confDF, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("GPP", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste("R+",Delta*C[pools])),
                                  "NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$Variable,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        geom_vline(xintercept = 3.5, linetype="dashed", color="black")+
        #geom_vline(xintercept = 4.5, linetype="dashed", color="black")+
        #geom_vline(xintercept = 5.5, linetype="dashed", color="black")+
        
        scale_y_continuous(limits=c(-150, 500), 
                           breaks=c(-150, -75, 0, 75, 150, 300, 500),
                           labels=c(-150, -75, 0, 75, 150, 300, 500))+
        #geom_text(aes(label=Variable), position=position_stack(), stat="identity", size=3, parse=T)
        guides(fill=guide_legend(ncol=6))#+
        #ylim(-400,800)+
        #scale_y_continuous(breaks=c(-400,0,400,800), limits=c(-400, 800))
    
    #plot(p3)
    
    ### Plotting
    pdf("Output/eco2_effect_on_gpp_and_subsequent_fluxes_pools_with_covariate_bootstrapped.pdf", width=8, height=6)
    #plot(p1)
    plot(p3)
    #plot(p2)
    dev.off()
    
    
    
}
