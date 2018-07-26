make_eCO2_effect_on_GPP_plot2 <- function() {
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
                       rep("change_in_pool", 14))   # 39 - 52
    
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
    
    ### Calculate totals of each plot.cat
    plotDF2 <- summaryBy(effect_size+conf_low+conf_high~plot.cat, data=plotDF1, FUN=sum, keep.names=T, na.rm=T)
    names(plotDF2)[1] <- "Variable"
    plotDF2$plot.cat <- c("total_change_in_pool", "total_influx", "total_npp", "total_outflux")
    
    ### Combine the plots
    plotDF <- rbind(plotDF1, plotDF2[c(1,3,4),])
    
    ### Assign plot cat 2
    plotDF$plot.cat2[plotDF$plot.cat=="Influxes"] <- "A"
    plotDF$plot.cat2[plotDF$plot.cat=="total_change_in_pool"] <- "B"
    plotDF$plot.cat2[plotDF$plot.cat=="total_npp"] <- "B"
    plotDF$plot.cat2[plotDF$plot.cat=="total_outflux"] <- "B"
    plotDF$plot.cat2[plotDF$plot.cat=="NPP"] <- "C"
    plotDF$plot.cat2[plotDF$plot.cat=="Change_in_pools"] <- "D"
    plotDF$plot.cat2[plotDF$plot.cat=="Outfluxes"] <- "E"
    
    ### Order plot DF
    plotDF <- plotDF[order(plotDF$plot.cat2),]
    
    ### Plot variable ordering
    plotDF$var.order <- 1:length(plotDF$plot.cat2)
    
    ### confidence interval for each plot.cat2 categories
    ### Need to separately calculate confidence interval using sums of each category
    confDF <- summaryBy(conf_high~plot.cat2, keep.names=T, na.rm=T, data=plotDF, FUN=max)
    confDF2 <- summaryBy(conf_low~plot.cat2, keep.names=T, na.rm=T, data=plotDF, FUN=min)
    confDF3 <- summaryBy(effect_size~plot.cat2, keep.names=T, na.rm=T, data=plotDF, FUN=sum)
    
    confDF$conf_low <- confDF2$conf_low
    confDF$effect_size <- confDF3$effect_size
    
    
    y.lab1 <- c("over_gpp"=expression(GPP[o]),                    # 1
                "understorey_gpp"=expression(GPP[u]),             # 2
                "ch4"=expression(CH[4]),                          # 3
                "leaf_prod"=expression(NPP[leaf]),                # 4
                "twig_prod"=expression(NPP[twig]),                # 5
                "bark_prod"=expression(NPP[bark]),                # 6
                "seed_prod"=expression(NPP[seed]),                # 7
                "wood_prod"=expression(NPP[wood]),                # 8
                "fineroot_prod"=expression(NPP[froot]),           # 9
                "coarseroot_prod"=expression(NPP[croot]),         # 10
                "understorey_prod"=expression(NPP[ua]),           # 11
                "over_leaf_respiration"=expression(R[leaf]),      # 12
                "wood_respiration"=expression(R[wood]),           # 13
                "root_respiration"=expression(R[root]),           # 14
                "understorey_respiration"=expression(R[ua]),      # 15
                "herb_consump"=expression(NPP[insect]),           # 16
                "hetero_respiration"=expression(R[h]),            # 17
                "doc"=expression(R[doc]),                         # 18
                "delta_leaf_c"=expression(Delta*C[leaf]),         # 19
                "delta_wood_c"=expression(Delta*C[wood]),         # 20
                "delta_fineroot_c"=expression(Delta*C[froot]),    # 21
                "delta_coarseroot_c"=expression(Delta*C[croot]),  # 22
                "delta_understorey_c"=expression(Delta*C[ua]),    # 23
                "delta_soil_c"=expression(Delta*C[soil]),         # 24
                "delta_litter_c"=expression(Delta*C[litter]),     # 25
                "Change_in_pools"=expression(Delta*C[pools]),     # 26 
                "NPP"="NPP",                                      # 27
                "Outfluxes"="Outfluxes")                          # 28
    
    ### Color labels
    #library(RColorBrewer)
    #cl <- colors(distinct=T)
    #set.seed(1888)
    #col.list1 <- sample(cl, 28)
    
    ## gpp
    colfunc.inf <- colorRampPalette(c("black", "grey"))
    I.col.list <- colfunc.inf(3)
    
    ## respiration
    colfunc.R <- colorRampPalette(c("darkred", "pink"))
    R.col.list <- colfunc.R(8)
    
    ## NPP
    colfunc.npp <- colorRampPalette(c("darkgreen", "yellow"))
    N.col.list <- colfunc.npp(9)
    
    ### Change in pools
    colfunc.delta <- colorRampPalette(c("darkblue", "cyan"))
    D.col.list <- colfunc.delta(8)
    
    ### Combine all color list
    col.list1 <- c("over_gpp"=I.col.list[1],                    
                   "understorey_gpp"=I.col.list[2],             
                   "ch4"=I.col.list[3], 
                   "NPP"=N.col.list[1],                                      
                   "Change_in_pools"=D.col.list[1],     
                   "Outfluxes"=R.col.list[1],
                   "leaf_prod"=N.col.list[2],                
                   "twig_prod"=N.col.list[3],                
                   "bark_prod"=N.col.list[4],                
                   "seed_prod"=N.col.list[5],                
                   "wood_prod"=N.col.list[6],               
                   "fineroot_prod"=N.col.list[7],           
                   "coarseroot_prod"=N.col.list[8],         
                   "understorey_prod"=N.col.list[9],           
                   "over_leaf_respiration"=R.col.list[2],      
                   "wood_respiration"=R.col.list[3],           
                   "root_respiration"=R.col.list[4],           
                   "understorey_respiration"=R.col.list[5],      
                   "herb_consump"=R.col.list[6],           
                   "hetero_respiration"=R.col.list[7],            
                   "doc"=R.col.list[8],                        
                   "delta_leaf_c"=D.col.list[2],         
                   "delta_wood_c"=D.col.list[3],        
                   "delta_fineroot_c"=D.col.list[4],    
                   "delta_coarseroot_c"=D.col.list[5],  
                   "delta_understorey_c"=D.col.list[6],    
                   "delta_soil_c"=D.col.list[7],         
                   "delta_litter_c"=D.col.list[8])                         
    
    #### Plotting
    p1 <- ggplot(plotDF,
                 aes(plot.cat2, effect_size)) +   
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        #geom_errorbar(data=confDF, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), width=0.2, size=1, color="grey") + 
        #geom_point(data=confDF, mapping=aes(x=plot.cat2, y=effect_size), size=4, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("Influxes", 
                                  expression(paste("NPP+R+", Delta*C[pools])),
                                  "NPP",
                                  expression(Delta*C[pools]),
                                  "R"))+
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
              panel.grid.major=element_line(color="grey"),
              legend.position="bottom",
              legend.text.align=0)+
        geom_hline(yintercept=0)
    
    p2 <- ggplot(plotDF,
                 aes(plot.cat2, effect_size)) +   
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        geom_errorbar(data=confDF, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), width=0.2, size=1, color="grey") + 
        geom_point(data=confDF, mapping=aes(x=plot.cat2, y=effect_size), size=4, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("Influxes", 
                                  expression(paste("NPP+R+", Delta*C[pools])),
                                  "NPP",
                                  expression(Delta*C[pools]),
                                  "R"))+
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
              panel.grid.major=element_line(color="grey"),
              legend.position="bottom",
              legend.text.align=0)+
        geom_hline(yintercept=0)


    ### Plotting
    pdf("R_other/eco2_effect_on_gpp_and_subsequent_fluxes_pools2.pdf", width=8, height=6)
    plot(p1)
    plot(p2)
    dev.off()
    
    
    
 }