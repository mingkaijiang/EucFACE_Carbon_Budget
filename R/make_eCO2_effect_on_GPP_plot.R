make_eCO2_effect_on_GPP_plot <- function() {
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
                "delta_litter_c"=expression(Delta*C[litter]))     # 25
    
    ### Plot variable ordering
    plotDF1$var.order <- c(14, 15, 16, 18, 3, 
                           4, 5, 6, 7, 8, 9, 10, 11,
                           17, 1, 12, 13, 2, 24, 19,
                           20, 21, 22, 23, 25)
    
    ### Order plot DF
    plotDF1 <- plotDF1[order(plotDF1$var.order),]
    
    ### make it a simple df
    plotDF1 <- plotDF1[,c("Variable", "effect_size", "conf_low", "conf_high", "plot.cat", "var.order")]
    
    ### assign plot category labels
    plotDF1$plot.cat2 <- c(rep("A", 3), rep("B", 22))
    
    ### Color labels
    library(RColorBrewer)
    cl <- colors(distinct=T)
    set.seed(1888)
    col.list1 <- sample(cl, 25)
    
    #### Plotting
    p1 <- ggplot(plotDF1,
                 aes(plot.cat2, effect_size)) +   
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        #geom_segment(data=plotDF1, aes(x=cat, xend=cat, y=conf_low, yend=conf_high), 
        #             colour="grey")+
        xlab("Method") + ylab("") +
        scale_x_discrete(labels=c("Influxes", "Others"))+
        scale_fill_manual(name="", 
                          breaks = plotDF1$Variable,
                          values = col.list1,
                          labels=y.lab1) +
        theme_linedraw() +
        ylim(-100,150)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="bottom",
              legend.text.align=0)
    
    ### Plot DF 2, look at the sum of these fluxes and pools
    plotDF2 <- summaryBy(effect_size+conf_low+conf_high~plot.cat, data=plotDF1, FUN=sum, keep.names=T, na.rm=T)
    
    names(plotDF2)[1] <- "Category"
    plotDF2$plot.cat <- c("Calc", "Influx", "Calc", "Calc")
    
    ### Set y labs
    y.lab2 <- c("Change_in_pools"=expression(Delta*C[pools]),
                "Influxes"="Influxes",
                "NPP"="NPP",
                "Outfluxes"="Outfluxes")
    
    col.list2 <- hsv(seq(0,1 - 1/4,length.out = 4),0.8,1)
    
    plotDF2$plot.order <- c(2,1,2,2)
    
    
    ## Plotting
    p2 <- ggplot(plotDF2,
                 aes(reorder(plot.cat, plot.order), effect_size)) +   
        geom_bar(stat = "identity", aes(fill=Category),
                 position="stack") +
        xlab("Method")+ ylab(expression(paste(CO[2], " treatment effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("Influxes", "Others"))+
        scale_fill_manual(name="", 
                          values = col.list2,
                          labels=y.lab2) +
        ylim(-100,150)+
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
              legend.text.align=0)
    
    
    ### Plot changes in plant pool
    plotDF3 <- subset(myDF, Variable %in% c("leaf_prod", "twig_prod", "bark_prod",
                                            "seed_prod", "wood_prod", "fineroot_prod",
                                            "coarseroot_prod", "understorey_prod", "herb_consump",
                                            "over_leaf_respiration", "wood_respiration", "root_respiration",
                                            "understorey_respiration", 
                                            "delta_leaf_c", "delta_wood_c", "delta_fineroot_c", 
                                            "delta_coarseroot_c", "delta_understorey_c"))
    plotDF3$plot.cat[plotDF3$Category=="change_in_pool"] <- "Change_in_pools"
    plotDF3$plot.cat[plotDF3$Category=="resp"] <- "Respirations"
    plotDF3$plot.cat[plotDF3$Category=="prod"] <- "NPP"

    tmpDF <- subset(myDF, Variable %in% c("leaf_prod", "twig_prod", "bark_prod",
                                          "seed_prod", "fineroot_prod",
                                          "understorey_prod"))
    tmpDF$plot.cat[tmpDF$Category=="prod"] <- "Litterfalls"
    
    plotDF3 <- rbind(plotDF3, tmpDF)
    
    ### Summarize
    plotDF4 <- summaryBy(effect_size+conf_low+conf_high~plot.cat, FUN=sum, data=plotDF3, keep.names=T, na.rm=T)
    
    names(plotDF4)[1] <- "Category"
    plotDF4$plot.cat <- c("Delta", "Other", "Other", "Other")
    
    ### Set y labs
    y.lab3 <- c("Change_in_pools"=expression(Delta*C[plant]),
                "Litterfalls"="Litterfalls",
                "NPP"="NPP",
                "Respirations"="respirations")
    
    col.list3 <- hsv(seq(0,1 - 1/4,length.out = 4),0.8,1)
    
    plotDF4$plot.order <- c(1,2,2,2)
    
    ## Plotting
    p3 <- ggplot(plotDF4,
                 aes(reorder(plot.cat, plot.order), effect_size)) +   
        geom_bar(stat = "identity", aes(fill=Category),
                 position="stack") +
        xlab("Method")+ ylab(expression(paste(CO[2], " treatment effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(Delta*C[plant]), "In & Out"))+
        scale_fill_manual(name="", 
                          values = col.list3,
                          labels=y.lab3) +
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
              legend.text.align=0)
    
    
    ##### Plot changes in soil + litter pools
    plotDF5 <- subset(myDF, Variable %in% c("leaf_prod", "twig_prod", "bark_prod",
                                            "seed_prod", "fineroot_prod",
                                            "understorey_prod", "delta_soil_c", "delta_litter_c",
                                            "hetero_respiration", "doc", "ch4"))
    
    plotDF5$plot.cat[plotDF5$Category=="change_in_pool"] <- "Change_in_pools"
    plotDF5$plot.cat[plotDF5$Category=="resp"] <- "Outfluxes"
    plotDF5$plot.cat[plotDF5$Category=="prod"] <- "Influxes"
    plotDF5$plot.cat[plotDF5$Variable=="ch4"] <- "Influxes"
    plotDF5$plot.cat[plotDF5$Variable=="doc"] <- "Outfluxes"
    
    ### Summarize
    plotDF6 <- summaryBy(effect_size+conf_low+conf_high~plot.cat, FUN=sum, data=plotDF5, keep.names=T, na.rm=T)
    
    names(plotDF6)[1] <- "Category"
    plotDF6$plot.cat <- c("Delta", "Other", "Other")
    
    ### Set y labs
    y.lab4 <- c("Change_in_pools"=expression(Delta*C[soil]),
                "Influxes"="Influxes",
                "Outfluxes"="Outfluxes")
    
    col.list4 <- hsv(seq(0,1 - 1/3,length.out = 3),0.8,1)
    
    plotDF6$plot.order <- c(1,2,2)
    
    ## Plotting
    p4 <- ggplot(plotDF6,
                 aes(reorder(plot.cat, plot.order), effect_size)) +   
        geom_bar(stat = "identity", aes(fill=Category),
                 position="stack") +
        xlab("Method")+ ylab("") +
        scale_x_discrete(labels=c(expression(Delta*C[soil]), "In & Out"))+
        scale_fill_manual(name="", 
                          values = col.list4,
                          labels=y.lab4) +
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
              legend.text.align=0)
    
    
    ### Plotting
    pdf("R_other/eco2_effect_on_gpp_and_subsequent_fluxes_pools.pdf", width=12, height=12)
    require(cowplot)    
    plot_grid(p2, p1, p3, p4,
              labels="auto", ncol=2, align="h", axis = "l",
              rel_widths=c(0.7,0.7, 0.7, 0.7))
    dev.off()
    
    
    
 }