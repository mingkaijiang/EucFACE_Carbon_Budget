make_eCO2_effect_on_GPP_plot_with_covariate <- function() {
    ######### Plot abs, no interactions, and change in pools
    ### read in the csv file to plot the treatment effect and confidence interval
    myDF1 <- read.csv("R_other/treatment_statistics_abs_no_interaction.csv")
    myDF2 <- read.csv("R_other/treatment_statistics_abs_change_in_pool_no_interaction.csv")
    myDF <- rbind(myDF1, myDF2)
    
    #### assign color scheme according to treatment p-value
    myDF$trt_sig[myDF$Trt_Pr > 0.1] <- "non-sig"
    myDF$trt_sig[myDF$Trt_Pr <= 0.1] <- "sig"
    
    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
    
    myDF <- myDF[complete.cases(myDF$effect_size),]
    
    #### exclude all pools
    myDF$Category <- c(rep("other", 2), # 1 - 2
                       rep("pool", 14), # 3 - 16
                       rep("resp", 2), # 17 - 18
                       rep("prod", 1), # 19
                       rep("resp", 2), # 20 - 21
                       rep("prod", 1), # 22
                       rep("resp", 2), # 23 - 24
                       rep("prod", 9), # 25 - 33
                       rep("resp", 1), # 34
                       rep("gpp", 1),  # 35
                       rep("resp", 2), # 36 - 37
                       rep("gpp", 1),  # 38
                       rep("change_in_pool", 13))   # 39 - 51
    
    ### Drop redundant pools and fluxes
    myDF <- subset(myDF, Variable != c("delta_understorey_c_2"))
    
    ### Drop CWD - confidence interval too large
    myDF <- subset(myDF, Variable != c("delta_cwd_c"))
    
    ### Subset GPP, NPP, change in pools, and out fluxes
    plotDF1 <- subset(myDF, Variable %in% c("over_gpp", "understorey_gpp", "ch4",
                                            "leaf_prod", "twig_prod", "bark_prod",
                                            "seed_prod", "wood_prod", "fineroot_prod",
                                            "coarseroot_prod", "understorey_prod", "herb_consump",
                                            "over_leaf_respiration", "wood_respiration", "root_respiration",
                                            "understorey_respiration", "hetero_respiration","doc",
                                            "delta_leaf_c", "delta_wood_c", "delta_fineroot_c", 
                                            "delta_coarseroot_c", "delta_understorey_c", 
                                            "delta_litter_c","delta_soil_c"))

    ### Add plot category
    plotDF1$plot.cat[plotDF1$Category=="gpp"] <- "Influxes"
    plotDF1$plot.cat[plotDF1$Category=="change_in_pool"] <- "Change_in_pools"
    plotDF1$plot.cat[plotDF1$Category=="resp"] <- "Outfluxes"
    plotDF1$plot.cat[plotDF1$Category=="prod"] <- "NPP"
    plotDF1$plot.cat[plotDF1$Variable=="ch4"] <- "Influxes"
    
    plotDF1 <- plotDF1[,c("Variable", "effect_size", "conf_low", "conf_high", "plot.cat")]
    
    plotDF1$plot.cat[plotDF1$Variable%in%c("root_respiration", "understorey_respiration",
                                           "over_leaf_respiration", "wood_respiration")] <- "Ra"
    plotDF1$plot.cat[plotDF1$Variable=="herb_consump"] <- "NPP"
    
    
    ### Calculate totals of each plot.cat
    plotDF2 <- summaryBy(effect_size+conf_low+conf_high~plot.cat, data=plotDF1, FUN=sum, keep.names=T, na.rm=T)
    names(plotDF2)[1] <- "Variable"
    plotDF2$plot.cat <- c("total_change_in_pool", "total_influx", "total_npp", "total_outflux", "total_ra")
    
    ### Combine the plots
    plotDF <- rbind(plotDF1, plotDF2[c(3,4,5),])
    
    ### Assign plot cat 2
    plotDF$plot.cat2[plotDF$plot.cat=="Influxes"] <- "A"
    plotDF$plot.cat2[plotDF$plot.cat=="total_npp"] <- "B"
    plotDF$plot.cat2[plotDF$plot.cat=="total_ra"] <- "B"
    plotDF$plot.cat2[plotDF$plot.cat=="total_outflux"] <- "B"
    plotDF$plot.cat2[plotDF$plot.cat=="NPP"] <- "C"
    plotDF$plot.cat2[plotDF$plot.cat=="Ra"] <- "D"
    plotDF$plot.cat2[plotDF$plot.cat=="Outfluxes"] <- "D"
    plotDF$plot.cat2[plotDF$plot.cat=="Change_in_pools"] <- "E"

    ### exclude variables with averaged CO2 effect < 1 g m-2 yr-1
    # plotDF <- subset(plotDF, effect_size >= 1)
    
    ### Order plot DF
    plotDF <- plotDF[order(plotDF$plot.cat2),]
    
    ### Plot variable ordering
    plotDF$var.order <- 1:length(plotDF$plot.cat2)
    
    ### confidence interval for each plot.cat2 categories
    ### Need to separately calculate confidence interval using sums of each category
    confDF <- summaryBy(effect_size~plot.cat2, keep.names=T, na.rm=T, data=plotDF, FUN=sum)
    plotDF$conf_low_radius <- with(plotDF, effect_size - conf_low)
    plotDF$conf_high_radius <- with(plotDF, conf_high - effect_size)
    plotDF$conf_low_radius_sq <- (plotDF$conf_low_radius)^2
    plotDF$conf_high_radius_sq <- (plotDF$conf_high_radius)^2
    
    confDF2 <- summaryBy(conf_high_radius_sq~plot.cat2, keep.names=T, na.rm=T, data=plotDF, FUN=sum)
    confDF3 <- summaryBy(conf_low_radius_sq~plot.cat2, keep.names=T, na.rm=T, data=plotDF, FUN=sum)
    
    confDF2$conf_high_radius <- sqrt(confDF2$conf_high_radius_sq)
    confDF3$conf_low_radius <- sqrt(confDF3$conf_low_radius_sq)
    
    confDF$conf_low <- confDF$effect_size - confDF3$conf_low_radius
    confDF$conf_high <- confDF$effect_size + confDF2$conf_high_radius
    
    y.lab1 <- c("ch4"=expression(CH[4]),                          # 1
                "over_gpp"=expression(GPP[o]),                    # 2
                "understorey_gpp"=expression(GPP[u]),             # 3
                "NPP"="NPP",                                      # 4
                "Outfluxes"=expression(R[other]),                 # 5
                "Ra"=expression(R[a]),                            # 6
                "herb_consump"=expression(NPP[insect]),           # 7
                "leaf_prod"=expression(NPP[leaf]),                # 8
                "twig_prod"=expression(NPP[twig]),                # 9
                "bark_prod"=expression(NPP[bark]),                # 10
                "seed_prod"=expression(NPP[seed]),                # 11
                "wood_prod"=expression(NPP[wood]),                # 12
                "fineroot_prod"=expression(NPP[froot]),           # 13
                "coarseroot_prod"=expression(NPP[croot]),         # 14
                "understorey_prod"=expression(NPP[ua]),           # 15
                "root_respiration"=expression(R[root]),           # 16
                "understorey_respiration"=expression(R[ua]),      # 17
                "doc"=expression(R[doc]),                         # 18
                "hetero_respiration"=expression(R[h]),            # 19
                "over_leaf_respiration"=expression(R[leaf]),      # 20
                "wood_respiration"=expression(R[wood]),           # 21
                "delta_soil_c"=expression(Delta*C[soil]),         # 22
                "delta_leaf_c"=expression(Delta*C[leaf]),         # 23
                "delta_wood_c"=expression(Delta*C[wood]),         # 24
                "delta_fineroot_c"=expression(Delta*C[froot]),    # 25
                "delta_coarseroot_c"=expression(Delta*C[croot]),  # 26
                "delta_understorey_c"=expression(Delta*C[ua]),    # 27
                "delta_litter_c"=expression(Delta*C[litter]))     # 28
          
    
    ## gpp
    colfunc.inf <- colorRampPalette(c("black", "grey"))
    A.col.list <- colfunc.inf(3)
    
    # 2nd column bar - NPP + Ra + Rother
    colfunc.inf <- colorRampPalette(c("darkgreen", "darkred", "red"))
    B.col.list <- colfunc.inf(3)
    
    ## NPP
    colfunc.npp <- colorRampPalette(c("darkgreen", "yellow"))
    C.col.list <- colfunc.npp(9)
    
    ## R
    colfunc.R <- colorRampPalette(c("darkred", "pink"))
    D.col.list <- colfunc.R(6)
    
    ### Change in pools
    colfunc.delta <- colorRampPalette(c("darkblue", "cyan"))
    E.col.list <- colfunc.delta(7)
    
    ### Combine all color list
    col.list1 <- c("ch4"=A.col.list[1], 
                   "over_gpp"=A.col.list[2],                    
                   "understorey_gpp"=A.col.list[3],             
                   "NPP"=B.col.list[1],   
                   "Outfluxes"=B.col.list[2],
                   "Ra"=B.col.list[3],      
                   "herb_consump"=C.col.list[1],           
                   "leaf_prod"=C.col.list[2],                
                   "twig_prod"=C.col.list[3],                
                   "bark_prod"=C.col.list[4],                
                   "seed_prod"=C.col.list[5],                
                   "wood_prod"=C.col.list[6],               
                   "fineroot_prod"=C.col.list[7],           
                   "coarseroot_prod"=C.col.list[8],         
                   "understorey_prod"=C.col.list[9],           
                   "over_leaf_respiration"=D.col.list[1],      
                   "wood_respiration"=D.col.list[2],           
                   "root_respiration"=D.col.list[3],           
                   "understorey_respiration"=D.col.list[4],      
                   "hetero_respiration"=D.col.list[5],            
                   "doc"=D.col.list[6],                        
                   "delta_leaf_c"=E.col.list[1],         
                   "delta_wood_c"=E.col.list[2],        
                   "delta_fineroot_c"=E.col.list[3],    
                   "delta_coarseroot_c"=E.col.list[4],  
                   "delta_understorey_c"=E.col.list[5],    
                   "delta_soil_c"=E.col.list[6],         
                   "delta_litter_c"=E.col.list[7])                         
    
    #### Plotting
    p1 <- ggplot(plotDF,
                 aes(plot.cat2, effect_size)) +   
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("Influxes", 
                                  "NPP+R",
                                  "NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF$Variable,
                          values = col.list1,
                          labels=y.lab1) +
        theme_linedraw() +
        ylim(-50,150)+
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
                                  "NPP+R",
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
    
    # y label
    y.lab2 <- c("over_gpp"=expression(GPP[o]),                    # 1
                "understorey_gpp"=expression(GPP[u]),             # 2
                "NPP"="NPP",                                      # 3
                "Outfluxes"=expression(R[other]),                 # 4
                "Ra"=expression(R[a]),                            # 5
                "herb_consump"=expression(NPP[insect]),           # 6
                "leaf_prod"=expression(NPP[leaf]),                # 7
                "twig_prod"=expression(NPP[twig]),                # 8
                "bark_prod"=expression(NPP[bark]),                # 9
                "seed_prod"=expression(NPP[seed]),                # 10
                "wood_prod"=expression(NPP[wood]),                # 11
                "fineroot_prod"=expression(NPP[froot]),           # 12
                "understorey_prod"=expression(NPP[ua]),           # 13
                "root_respiration"=expression(R[root]),           # 14
                "understorey_respiration"=expression(R[ua]),      # 15
                "hetero_respiration"=expression(R[h]),            # 16
                "over_leaf_respiration"=expression(R[leaf]),      # 17
                "wood_respiration"=expression(R[wood]),           # 18
                "delta_soil_c"=expression(Delta*C[soil]),         # 19
                "delta_wood_c"=expression(Delta*C[wood]),         # 20
                "delta_fineroot_c"=expression(Delta*C[froot]))     # 21
    
    library(viridis)
    
    ## gpp
    colfunc.inf <- colorRampPalette(c("orange", "yellow"))
    A.col.list <- colfunc.inf(2)
    
    # 2nd column bar - NPP + Ra + Rother
    colfunc.inf <- colorRampPalette(c("green", "darkred", "red"))
    B.col.list <- colfunc.inf(3)
    
    ## NPP
    colfunc.npp <- colorRampPalette(c("darkgreen", "green"))
    C.col.list <- viridis(8) 
    
    ## R
    colfunc.R <- colorRampPalette(c("darkred", "pink"))
    D.col.list <- colfunc.R(5)
    
    ### Change in pools
    colfunc.delta <- colorRampPalette(c("darkblue", "cyan"))
    E.col.list <- colfunc.delta(3)
    
    v.list <- viridis(21)
    
    ### Combine all color list
    col.list2 <- c("over_gpp"=A.col.list[1],                    
                   "understorey_gpp"=A.col.list[2],             
                   "NPP"=B.col.list[1],   
                   "Outfluxes"=B.col.list[2],
                   "Ra"=B.col.list[3],      
                   "herb_consump"=C.col.list[1],           
                   "leaf_prod"=C.col.list[2],                
                   "twig_prod"=C.col.list[3],                
                   "bark_prod"=C.col.list[4],                
                   "seed_prod"=C.col.list[5],                
                   "wood_prod"=C.col.list[6],               
                   "fineroot_prod"=C.col.list[7],           
                   "understorey_prod"=C.col.list[8], 
                   "root_respiration"=D.col.list[1],           
                   "understorey_respiration"=D.col.list[2],      
                   "hetero_respiration"=D.col.list[3],            
                   "over_leaf_respiration"=D.col.list[4],      
                   "wood_respiration"=D.col.list[5],           
                   "delta_soil_c"=E.col.list[1],         
                   "delta_wood_c"=E.col.list[2],        
                   "delta_fineroot_c"=E.col.list[3])      
    
    col.list2 <- c("over_gpp"=v.list[1],                    
                   "understorey_gpp"=v.list[2],             
                   "NPP"=v.list[3],   
                   "Outfluxes"=v.list[4],
                   "Ra"=v.list[5],      
                   "herb_consump"=v.list[6],           
                   "leaf_prod"=v.list[7],                
                   "twig_prod"=v.list[8],                
                   "bark_prod"=v.list[9],                
                   "seed_prod"=v.list[10],                
                   "wood_prod"=v.list[11],               
                   "fineroot_prod"=v.list[12],           
                   "understorey_prod"=v.list[13], 
                   "root_respiration"=v.list[14],           
                   "understorey_respiration"=v.list[15],      
                   "hetero_respiration"=v.list[16],            
                   "over_leaf_respiration"=v.list[17],      
                   "wood_respiration"=v.list[18],           
                   "delta_soil_c"=v.list[19],         
                   "delta_wood_c"=v.list[20],        
                   "delta_fineroot_c"=v.list[21])        

    for (i in 1:5) {
        if (confDF[i, "conf_low"] >= -50) {
            confDF[i, "conf_low_sc"] <- confDF[i, "conf_low"]
        } else {
            confDF[i, "conf_low_sc"] <- -50 + (confDF[i, "conf_low"] * 0.125)
        }
        
        if (confDF[i, "conf_high"] <= 150) {
            confDF[i, "conf_high_sc"] <- confDF[i, "conf_high"]
        } else {
            confDF[i, "conf_high_sc"] <- 150 + ((confDF[i, "conf_high"] - 150) * 0.1)
        }

    }

    p3 <- ggplot(plotDF2,
                 aes(plot.cat2, effect_size)) +  
        geom_hline(yintercept=0)+
        geom_hline(yintercept=150, linetype="dashed", color="grey")+
        geom_hline(yintercept=-50, linetype="dashed", color="grey")+
        geom_errorbar(data=confDF, mapping=aes(x=plot.cat2, ymin=conf_low_sc, ymax=conf_high_sc), 
                      width=0.1, size=1, color="grey") + 
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        geom_point(data=confDF, mapping=aes(x=plot.cat2, y=effect_size), size=4, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("Influxes", 
                                  "NPP+R",
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
        scale_y_continuous(limits=c(-100, 200), 
                           breaks=c(-100, -75, -50, -25, 0, 50, 100, 150, 175, 200),
                           labels=c(-400, -200, -50, -25, 0, 50, 100, 150, 400, 650))+
        #geom_text(aes(label=Variable), position=position_stack(), stat="identity", size=3, parse=T)
        guides(fill=guide_legend(ncol=5))
        
    plot(p3)

    ### Plotting
    pdf("R_other/eco2_effect_on_gpp_and_subsequent_fluxes_pools2.pdf", width=8, height=6)
    #plot(p1)
    plot(p3)
    #plot(p2)
    dev.off()
    
    
    
 }
