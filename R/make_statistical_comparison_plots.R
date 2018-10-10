make_statistical_comparison_plots <- function() {
#    ######### 1. Plot ratios, no interactions
#    ### read in the csv file to plot the treatment effect and confidence interval
#    myDF <- read.csv("R_other/treatment_statistics_ratio_no_interaction.csv")
#    
#    #### assign color scheme according to treatment p-value
#    myDF$trt_sig[myDF$Trt_Pr > 0.15] <- "non-sig"
#    myDF$trt_sig[myDF$Trt_Pr <= 0.15] <- "sig"
#    
#    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
#    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
#    
#    #myDF$eff_factor[myDF$effect_size >1] <- "above"
#    #myDF$eff_factor[myDF$effect_size <1] <- "below"
#    
#    myDF <- myDF[complete.cases(myDF$effect_size),]
#    
#    #### Prepare x labels
#    x.labs <- c("LAI", "SLA", expression(C[soil]), expression(C[leaf]), expression(C[wood]),
#                expression(C[froot]),expression(C[croot]),expression(C[ua]),expression(C[ua2]),
#                expression(C[ua_live]),expression(C[ua_dead]),expression(C[micr]),expression(C[myco]),
#                expression(C[lit]),expression(C[ins]),#expression(C[cwd]),
#                expression(R[root]),
#                expression(R[ua]),expression(L[frass]),expression(NPP[hb]),expression(R[hb]),
#                expression(NPP[lerp]),expression(R[soil]),expression(L[doc]),expression(NPP[leaf]),
#                expression(NPP[twig]),expression(NPP[bark]),expression(NPP[seed]),expression(NPP[wood]),
#                expression(NPP[froot]),expression(NPP[croot]),expression(NPP[ua]),expression(R[rh]),
#                expression(GPP[o]), expression(R[leaf]), expression(R[wood]), expression(GPP[u]))
#    
#    #### Plotting
#    p1 <- ggplot(myDF)+ 
#        geom_point(aes(x=Variable, y=effect_size, shape=factor(date_sig)), stat='identity', size=4)+
#        geom_segment(aes(x=Variable, y=conf_low, xend=Variable, yend=conf_high, color=factor(trt_sig)))+
#        scale_color_manual(name="Treatment significance", 
#                           labels = c("Non-sig", "Sig (P<0.15)"), 
#                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
#        scale_shape_manual(name="Date Significance", 
#                           labels = c("Non-sig", "Sig (P<0.05)"), 
#                           values = c(1, 2))+ 
#        ylim(0, 4)+coord_flip()+
#        labs(x="Variable", y="eC/aC ratio")+
#        theme_linedraw()+
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_text(size=14), 
#              axis.text.x = element_text(size=12),
#              axis.text.y=element_text(size=12),
#              axis.title.y=element_text(size=14),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_blank(),
#              legend.position="right")+
#        geom_hline(yintercept = 1.0)+
#        scale_x_discrete(limits=myDF$Variable,labels=x.labs)
#    
#    pdf("R_other/treatment_effect_ratio_no_interaction.pdf", width=6, height=8)
#    plot(p1)
#    dev.off()
#    
#    
#    ######### 2. Plot ratios, with interactions
#    ### read in the csv file to plot the treatment effect and confidence interval
#    myDF <- read.csv("R_other/treatment_statistics_ratio_interaction.csv")
#    
#    #### assign color scheme according to treatment p-value
#    myDF$trt_sig[myDF$Trt_Pr > 0.15] <- "non-sig"
#    myDF$trt_sig[myDF$Trt_Pr <= 0.15] <- "sig"
#    
#    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
#    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
#    
#    myDF$date_trt_sig[myDF$Trt_Date_Pr > 0.05] <- "non-sig"
#    myDF$date_trt_sig[myDF$Trt_Date_Pr <= 0.05] <- "sig"
#    
#    myDF <- myDF[complete.cases(myDF$effect_size),]
#    
#    #### Prepare x labels
#    x.labs <- c("LAI", "SLA", expression(C[soil]), expression(C[leaf]), expression(C[wood]),
#                expression(C[froot]),expression(C[croot]),expression(C[ua]),expression(C[ua2]),
#                expression(C[ua_live]),expression(C[ua_dead]),expression(C[micr]),expression(C[myco]),
#                expression(C[lit]),expression(C[ins]),#expression(C[cwd]),
#                expression(R[root]),
#                expression(R[ua]),expression(L[frass]),expression(NPP[hb]),expression(R[hb]),
#                expression(NPP[lerp]),expression(R[soil]),expression(L[doc]),expression(NPP[leaf]),
#                expression(NPP[twig]),expression(NPP[bark]),expression(NPP[seed]),expression(NPP[wood]),
#                expression(NPP[froot]),expression(NPP[croot]),expression(NPP[ua]),expression(R[rh]),
#                expression(GPP[o]), expression(R[leaf]), expression(R[wood]), expression(GPP[u]))
#    
#    #### Plotting
#    p2 <- ggplot(myDF)+ 
#        geom_segment(aes(x=Variable, y=conf_low, xend=Variable, yend=conf_high, color=factor(trt_sig)))+
#        geom_point(aes(x=Variable, y=effect_size, shape=factor(date_sig), fill=factor(date_trt_sig)), 
#                   stat='identity', size=4)+
#        scale_color_manual(name="Treatment significance", 
#                           labels = c("Non-sig", "Sig (P<0.15)"), 
#                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
#        scale_shape_manual(name="Date Significance", 
#                           labels = c("Non-sig", "Sig (P<0.05)"), 
#                           values = c(21, 24))+ 
#        scale_fill_manual(name="Interaction significance", 
#                           labels = c("Non-sig", "Sig (P<0.05)"), 
#                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+
#        ylim(0, 7)+coord_flip()+
#        labs(x="Variable", y="eC/aC ratio")+
#        theme_linedraw()+
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_text(size=14), 
#              axis.text.x = element_text(size=12),
#              axis.text.y=element_text(size=12),
#              axis.title.y=element_text(size=14),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_blank(),
#              legend.position="right")+
#        geom_hline(yintercept = 1.0)+
#        scale_x_discrete(limits=myDF$Variable,labels=x.labs)+
#        guides(fill = guide_legend(override.aes = list(color = c("#f8766d", "#00ba38"))), 
#               size = guide_legend(override.aes = list(shape = c(21, 21))))
#    
#    pdf("R_other/treatment_effect_ratio_interaction.pdf", width=6, height=8)
#    plot(p2)
#    dev.off()
#    
#    ######### 3. Plot abs, no interactions
#    ### read in the csv file to plot the treatment effect and confidence interval
#    myDF <- read.csv("R_other/treatment_statistics_abs_no_interaction.csv")
#    
#    #### assign color scheme according to treatment p-value
#    myDF$trt_sig[myDF$Trt_Pr > 0.1] <- "non-sig"
#    myDF$trt_sig[myDF$Trt_Pr <= 0.1] <- "sig"
#    
#    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
#    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
#    
#    myDF <- myDF[complete.cases(myDF$effect_size),]
#    
#    ### Ignore a few pool variables that vary too large
#    myDF <- subset(myDF, conf_low >= - 200)
#    
#    
#    #### Prepare x labels
#    x.labs <- c("LAI", "SLA", expression(C[leaf]),
#                expression(C[froot]),expression(C[croot]),expression(C[ua]),expression(C[ua2]),
#                expression(C[ua_live]),expression(C[ua_dead]),expression(C[micr]),expression(C[myco]),
#                expression(C[lit]),expression(C[ins]),#expression(C[cwd]),
#                expression(R[root]),
#                expression(R[ua]),expression(L[frass]),expression(NPP[hb]),expression(R[hb]),
#                expression(NPP[lerp]),expression(R[soil]),expression(L[doc]),expression(NPP[leaf]),
#                expression(NPP[twig]),expression(NPP[bark]),expression(NPP[seed]),expression(NPP[wood]),
#                expression(NPP[froot]),expression(NPP[croot]),expression(NPP[ua]),expression(R[rh]),
#                expression(GPP[o]), expression(R[leaf]), expression(R[wood]), expression(GPP[u]))
#    
#    #### Plotting
#    p3 <- ggplot(myDF)+ 
#        geom_point(aes(x=Variable, y=effect_size, shape=factor(date_sig)), stat='identity', size=4)+
#        geom_segment(aes(x=Variable, y=conf_low, xend=Variable, yend=conf_high, color=factor(trt_sig)))+
#        scale_color_manual(name="Treatment significance", 
#                           labels = c("Non-sig", "Sig (P<0.1)"), 
#                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
#        scale_shape_manual(name="Date Significance", 
#                           labels = c("Non-sig", "Sig (P<0.05)"), 
#                           values = c(1, 2))+ 
#        ylim(-200, 400)+coord_flip()+
#        labs(x="Variable", y="eC minus aC")+
#        theme_linedraw()+
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_text(size=14), 
#              axis.text.x = element_text(size=12),
#              axis.text.y=element_text(size=12),
#              axis.title.y=element_text(size=14),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_blank(),
#              legend.position="right")+
#        geom_hline(yintercept = 1.0)+
#        scale_x_discrete(limits=myDF$Variable,labels=x.labs)
#    
#    pdf("R_other/treatment_effect_abs_no_interaction.pdf", width=6, height=8)
#    plot(p3)
#    dev.off()
#    
#    ######### 4. Plot abs, with interactions
#    ### read in the csv file to plot the treatment effect and confidence interval
#    myDF <- read.csv("R_other/treatment_statistics_abs_interaction.csv")
#    
#    #### assign color scheme according to treatment p-value
#    myDF$trt_sig[myDF$Trt_Pr > 0.15] <- "non-sig"
#    myDF$trt_sig[myDF$Trt_Pr <= 0.15] <- "sig"
#    
#    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
#    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
#    
#    myDF$date_trt_sig[myDF$Trt_Date_Pr > 0.05] <- "non-sig"
#    myDF$date_trt_sig[myDF$Trt_Date_Pr <= 0.05] <- "sig"
#    
#    myDF <- myDF[complete.cases(myDF$effect_size),]
#    
#    ### Ignore a few pool variables that vary too large
#    myDF <- subset(myDF, conf_low >= - 200)
#    
#    #### Prepare x labels
#    x.labs <- c("LAI", "SLA", expression(C[leaf]),
#                expression(C[froot]),expression(C[croot]),expression(C[ua]),expression(C[ua2]),
#                expression(C[ua_live]),expression(C[ua_dead]),expression(C[micr]),expression(C[myco]),
#                expression(C[lit]),expression(C[ins]),#expression(C[cwd]),
#                expression(R[root]),
#                expression(R[ua]),expression(L[frass]),expression(NPP[hb]),expression(R[hb]),
#                expression(NPP[lerp]),expression(R[soil]),expression(L[doc]),expression(NPP[leaf]),
#                expression(NPP[twig]),expression(NPP[bark]),expression(NPP[seed]),expression(NPP[wood]),
#                expression(NPP[froot]),expression(NPP[croot]),expression(NPP[ua]),expression(R[rh]),
#                expression(GPP[o]), expression(R[leaf]), expression(R[wood]), expression(GPP[u]))
#    
#    #### Plotting
#    p4 <- ggplot(myDF)+ 
#        geom_segment(aes(x=Variable, y=conf_low, xend=Variable, yend=conf_high, color=factor(trt_sig)))+
#        geom_point(aes(x=Variable, y=effect_size, shape=factor(date_sig), fill=factor(date_trt_sig)), 
#                   stat='identity', size=4)+
#        scale_color_manual(name="Treatment significance", 
#                           labels = c("Non-sig", "Sig (P<0.15)"), 
#                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
#        scale_shape_manual(name="Date Significance", 
#                           labels = c("Non-sig", "Sig (P<0.05)"), 
#                           values = c(21, 24))+ 
#        scale_fill_manual(name="Interaction significance", 
#                          labels = c("Non-sig", "Sig (P<0.05)"), 
#                          values = c("non-sig"="#f8766d","sig"="#00ba38"))+
#        ylim(-200, 400)+coord_flip()+
#        labs(x="Variable", y="eC minus aC")+
#        theme_linedraw()+
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_text(size=14), 
#              axis.text.x = element_text(size=12),
#              axis.text.y=element_text(size=12),
#              axis.title.y=element_text(size=14),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_blank(),
#              legend.position="right")+
#        geom_hline(yintercept = 1.0)+
#        scale_x_discrete(limits=myDF$Variable,labels=x.labs)+
#        guides(fill = guide_legend(override.aes = list(color = c("#f8766d", "#00ba38"))), 
#               size = guide_legend(override.aes = list(shape = c(21, 21))))
#    
#    pdf("R_other/treatment_effect_abs_interaction.pdf", width=6, height=8)
#    plot(p4)
#    dev.off()
#    
#    
#    ######### 5. Plot abs, no interactions, and change in pools
#    ### read in the csv file to plot the treatment effect and confidence interval
#    myDF1 <- read.csv("R_other/treatment_statistics_abs_no_interaction.csv")
#    myDF2 <- read.csv("R_other/treatment_statistics_abs_change_in_pool_no_interaction.csv")
#    myDF <- rbind(myDF1, myDF2)
#    
#    #### assign color scheme according to treatment p-value
#    myDF$trt_sig[myDF$Trt_Pr > 0.1] <- "non-sig"
#    myDF$trt_sig[myDF$Trt_Pr <= 0.1] <- "sig"
#    
#    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
#    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
#    
#    myDF <- myDF[complete.cases(myDF$effect_size),]
#    
#    #### exclude all pools
#    myDF$Category <- c(rep("other", 2), # 1 - 2
#                       rep("pool", 14), # 3 - 16
#                       rep("resp", 2), # 17 - 18
#                       rep("prod", 1), # 19
#                       rep("resp", 2), # 20 - 21
#                       rep("prod", 1), # 22
#                       rep("resp", 2), # 23 - 24
#                       rep("prod", 9), # 25 - 33
#                       rep("resp", 1), # 34
#                       rep("gpp", 1),  # 35
#                       rep("resp", 2), # 36 - 37
#                       rep("gpp", 1),  # 38
#                       rep("change_in_pool", 13))   # 39 - 52
#    
#    ### Drop redundant pools and fluxes
#    myDF <- subset(myDF, Variable != c("delta_understorey_c_2"))
#    
#    ### Drop CWD - confidence interval too large
#    myDF <- subset(myDF, Variable != c("delta_cwd_c"))
#    
#    plotDF1 <- subset(myDF, Category == "change_in_pool")
#    plotDF2 <- subset(myDF, Category == "resp")
#    plotDF3 <- subset(myDF, Category == "prod")
#    plotDF4 <- subset(myDF, Category == "gpp")
#    
#    ### transform the data so that we could have break point on x axis
#    #Function to transform data to y positions
#    trans <- function(x) {
#        if (x < 0) {
#            pmax(x,-40) + 0.25*pmin(x+40,0)
#        } else {
#            pmin(x,40) + 0.25*pmax(x-40,0)
#        }
#    }
#    
#    xticks.brk <- xticks <- c(-150, -100, -40, -20, 0, 20, 40, 100, 200, 300)
#    
#    #Transform the data onto the display scale
#    for (i in 1:length(plotDF1$Variable)) {
#        plotDF1$effect_size_t[i] <- trans(plotDF1$effect_size[i])
#        plotDF1$conf_low_t[i] <- trans(plotDF1$conf_low[i])
#        plotDF1$conf_high_t[i] <- trans(plotDF1$conf_high[i])
#    }   
#    
#    for (i in 1:length(plotDF2$Variable)) {
#        plotDF2$effect_size_t[i] <- trans(plotDF2$effect_size[i])
#        plotDF2$conf_low_t[i] <- trans(plotDF2$conf_low[i])
#        plotDF2$conf_high_t[i] <- trans(plotDF2$conf_high[i])
#    }  
#    
#    for (i in 1:length(plotDF3$Variable)) {
#        plotDF3$effect_size_t[i] <- trans(plotDF3$effect_size[i])
#        plotDF3$conf_low_t[i] <- trans(plotDF3$conf_low[i])
#        plotDF3$conf_high_t[i] <- trans(plotDF3$conf_high[i])
#    }  
#    
#    for (i in 1:length(plotDF4$Variable)) {
#        plotDF4$effect_size_t[i] <- trans(plotDF4$effect_size[i])
#        plotDF4$conf_low_t[i] <- trans(plotDF4$conf_low[i])
#        plotDF4$conf_high_t[i] <- trans(plotDF4$conf_high[i])
#    }  
#    
#    for (i in 1:length(xticks)) {
#        xticks.brk[i] <- trans(xticks[i])
#    }
#    
#    y.lab1 <- c("delta_soil_c"=expression(Delta*C[soil]),
#                "delta_leaf_c"=expression(Delta*C[leaf]),
#                "delta_wood_c"=expression(Delta*C[wood]),
#                "delta_fineroot_c"=expression(Delta*C[froot]),
#                "delta_coarseroot_c"=expression(Delta*C[croot]),
#                "delta_understorey_c"=expression(Delta*C[ua]),
#                "delta_understorey_c_live"=expression(Delta*C[ua_live]),
#                "delta_understorey_c_dead"=expression(Delta*C[ua_dead]),
#                "delta_microbial_c"=expression(Delta*C[micr]),
#                "delta_mycorrhizal_c"=expression(Delta*C[myco]),
#                "delta_litter_c"=expression(Delta*C[lit]),
#                "delta_insect_c"=expression(Delta*C[ins]))
#    
#    y.lab2 <- c("wood_respiration"=expression(R[wood]),
#                "root_respiration"=expression(R[root]),
#                "understorey_respiration"=expression(R[ua]),
#                "herb_consump"=expression(NPP[hb]),
#                "herb_respiration"=expression(R[hb]),
#                "soil_respiration"=expression(R[soil]),
#                "doc"=expression(L[doc]),
#                "hetero_respiration"=expression(R[rh]),
#                "over_leaf_respiration"=expression(R[leaf]))
#    
#    y.lab3 <- c("frass_prod"=expression(P[frass]),
#                "lerp_prod"=expression(P[lerp]),
#                "leaf_prod"=expression(NPP[leaf]),
#                "twig_prod"=expression(NPP[twig]),
#                "bark_prod"=expression(NPP[bark]),
#                "seed_prod"=expression(NPP[seed]),
#                "wood_prod"=expression(NPP[wood]),
#                "fineroot_prod"=expression(NPP[froot]),
#                "coarseroot_prod"=expression(NPP[croot]),
#                "understorey_prod"=expression(NPP[ua]),
#                "ch4"=expression(U[CH[4]]))
#    
#    y.lab4 <- c("over_gpp"=expression(GPP[o]),
#                "understorey_gpp"=expression(GPP[u]))
#    
#    
#    ### add conditional color to data frames
#    plotDF1$col.con <- ifelse(plotDF1$effect_size_t<0, "red3", "blue2")
#    plotDF2$col.con <- ifelse(plotDF2$effect_size_t<0, "red3", "blue2")
#    plotDF3$col.con <- ifelse(plotDF3$effect_size_t<0, "red3", "blue2")
#    plotDF4$col.con <- ifelse(plotDF4$effect_size_t<0, "red3", "blue2")
#    
#    #### Plotting
#    p8 <- ggplot(plotDF4)+ 
#        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
#                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
#                     size=6, color="grey")+
#        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
#                   stat='identity', size=4, shape=19, color=plotDF4$col.con)+
#        labs(x="eC minus aC", y="")+
#        theme_linedraw()+
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_blank(), 
#              axis.text.x = element_blank(),
#              axis.text.y=element_text(size=12),
#              axis.title.y=element_text(size=14),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_blank(),
#              legend.position="none")+
#        geom_vline(xintercept = 0.0)+
#        geom_vline(xintercept = -40, linetype="dashed", color="grey")+
#        geom_vline(xintercept = -55, linetype="dashed", color="grey")+
#        geom_vline(xintercept = 40, linetype="dashed", color="grey")+
#        geom_vline(xintercept = 55, linetype="dashed", color="grey")+
#        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
#        scale_y_discrete(labels=y.lab4)
#    
#    
#    p5 <- ggplot(plotDF1)+ 
#        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
#                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
#                     size=6, color="grey")+
#        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
#                   stat='identity', size=4, shape=19, color=plotDF1$col.con)+
#        #scale_color_manual(name="Treatment significance", 
#        #                   labels = c("Non-sig", "Sig (P<0.1)"), 
#        #                   values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
#        #scale_shape_manual(name="Date Significance", 
#        #                   labels = c("Non-sig", "Sig (P<0.05)"), 
#        #                   values = c(1, 2))+ 
#        labs(x="eC minus aC", y="")+
#        theme_linedraw()+
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_blank(), 
#              axis.text.x = element_blank(),
#              axis.text.y=element_text(size=12),
#              axis.title.y=element_text(size=14),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_blank(),
#              legend.position="none")+
#        geom_vline(xintercept = 0.0)+
#        geom_vline(xintercept = -40, linetype="dashed", color="grey")+
#        geom_vline(xintercept = -55, linetype="dashed", color="grey")+
#        geom_vline(xintercept = 40, linetype="dashed", color="grey")+
#        geom_vline(xintercept = 55, linetype="dashed", color="grey")+
#        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
#        scale_y_discrete(labels=y.lab1)
#    
#    p6 <- ggplot(plotDF3)+ 
#        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
#                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
#                     size=6, color="grey")+
#        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
#                   stat='identity', size=4, shape=19, color=plotDF3$col.con)+
#        labs(x="eC minus aC", y="")+
#        theme_linedraw()+
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_blank(), 
#              axis.text.x = element_blank(),
#              axis.text.y=element_text(size=12),
#              axis.title.y=element_text(size=14),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_blank(),
#              legend.position="none")+
#        geom_vline(xintercept = 0.0)+
#        geom_vline(xintercept = -40, linetype="dashed", color="grey")+
#        geom_vline(xintercept = -55, linetype="dashed", color="grey")+
#        geom_vline(xintercept = 40, linetype="dashed", color="grey")+
#        geom_vline(xintercept = 55, linetype="dashed", color="grey")+
#        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
#        scale_y_discrete(labels=y.lab3)
#    
#    p7 <- ggplot(plotDF2)+ 
#        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
#                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
#                     size=6, color="grey")+
#        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
#                   stat='identity', size=4, shape=19, color=plotDF2$col.con)+
#        scale_color_manual(name="Treatment significance", 
#                           labels = c("Non-sig", "Sig (P<0.1)"), 
#                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
#        labs(x=expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")")), y="")+
#        theme_linedraw()+
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_text(size=14), 
#              axis.text.x = element_text(size=12),
#              axis.text.y=element_text(size=12),
#              axis.title.y=element_text(size=14),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_blank(),
#              legend.position="bottom")+
#        geom_vline(xintercept = 0.0)+
#        geom_vline(xintercept = -40, linetype="dashed", color="grey")+
#        geom_vline(xintercept = -55, linetype="dashed", color="grey")+
#        geom_vline(xintercept = 40, linetype="dashed", color="grey")+
#        geom_vline(xintercept = 55, linetype="dashed", color="grey")+
#        #geom_rect(aes(xmin=-50, xmax=-40, ymin=0, ymax=18), fill="white")+
#        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
#        scale_y_discrete(labels=y.lab2)
#    
#    #grid.labs <- c("(a)", "(b)", "(c)")
#    
#    #require(grid)
#    
#    ## plot 
#    #pdf("R_other/treatment_effect_abs_no_interaction_change_in_pool.pdf", width=8, height=10)
#    #grid.newpage()
#    #grid.draw(rbind(ggplotGrob(p8), ggplotGrob(p5), ggplotGrob(p6), 
#    #                ggplotGrob(p7), size="last"))
#    #grid.text(grid.labs,x = 0.95, y = c(0.95, 0.63, 0.32),
#    #          gp=gpar(fontsize=16, col="black", fontface="bold"))
#    #dev.off()
#
#    pdf("R_other/treatment_effect_abs_no_interaction_change_in_pool.pdf", width=8, height=12)
#    require(cowplot)    
#    plot_grid(p8, p5, 
#              p6, p7, 
#              labels="AUTO", ncol=1, align="v", axis = "l",
#              rel_heights=c(0.3,1,1,0.9))
#    dev.off()
#    
#    
#    ######### 6. Plot ratio, no interactions, and no change in pools
#    ### read in the csv file to plot the treatment effect and confidence interval
#    myDF <- read.csv("R_other/treatment_statistics_ratio_no_interaction.csv")
#    
#    #### assign color scheme according to treatment p-value
#    myDF$trt_sig[myDF$Trt_Pr > 0.1] <- "non-sig"
#    myDF$trt_sig[myDF$Trt_Pr <= 0.1] <- "sig"
#    
#    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
#    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
#    
#    myDF <- myDF[complete.cases(myDF$effect_size),]
#    
#    #### exclude all pools
#    myDF$Category <- c(rep("other", 2), # 1 - 2
#                       rep("pool", 14), # 3 - 16
#                       rep("resp", 2), # 17 - 18
#                       rep("prod", 1), # 19
#                       rep("resp", 2), # 20 - 21
#                       rep("prod", 1), # 22
#                       rep("resp", 2), # 23 - 24
#                       rep("prod", 8), # 25 - 33
#                       rep("resp", 1), # 34
#                       rep("gpp", 1),  # 35
#                       rep("resp", 2), # 36 - 37
#                       rep("gpp", 1))   # 38
#    
#    plotDF2 <- subset(myDF, Category == "resp")
#    plotDF3 <- subset(myDF, Category == "prod")
#    plotDF4 <- subset(myDF, Category == "gpp")
#    
#    ### transform the data so that we could have break point on x axis
#    #Function to transform data to y positions
#    trans <- function(x) {
#        if (x > 1.2) {
#            pmin(x,1.2) + 0.15*pmax(x-1.2,0)
#        } else {
#            x
#        }
#    }
#    
#    xticks.brk <- xticks <- c(0.4, 0.6, 0.8, 0.9, 1, 1.1, 1.2, 
#                              2, 3, 4)
#    
#    #Transform the data onto the display scale
#    for (i in 1:length(plotDF2$Variable)) {
#        plotDF2$effect_size_t[i] <- trans(plotDF2$effect_size[i])
#        plotDF2$conf_low_t[i] <- trans(plotDF2$conf_low[i])
#        plotDF2$conf_high_t[i] <- trans(plotDF2$conf_high[i])
#    }  
#    
#    for (i in 1:length(plotDF3$Variable)) {
#        plotDF3$effect_size_t[i] <- trans(plotDF3$effect_size[i])
#        plotDF3$conf_low_t[i] <- trans(plotDF3$conf_low[i])
#        plotDF3$conf_high_t[i] <- trans(plotDF3$conf_high[i])
#    }  
#    
#    for (i in 1:length(plotDF4$Variable)) {
#        plotDF4$effect_size_t[i] <- trans(plotDF4$effect_size[i])
#        plotDF4$conf_low_t[i] <- trans(plotDF4$conf_low[i])
#        plotDF4$conf_high_t[i] <- trans(plotDF4$conf_high[i])
#    }  
#    
#    for (i in 1:length(xticks)) {
#        xticks.brk[i] <- trans(xticks[i])
#    }
#    
#    y.lab2 <- c("wood_respiration"=expression(R[wood]),
#                "root_respiration"=expression(R[root]),
#                "understorey_respiration"=expression(R[ua]),
#                "herb_consump"=expression(NPP[hb]),
#                "herb_respiration"=expression(R[hb]),
#                "soil_respiration"=expression(R[soil]),
#                "doc"=expression(L[doc]),
#                "hetero_respiration"=expression(R[rh]),
#                "over_leaf_respiration"=expression(R[leaf]))
#    
#    y.lab3 <- c("frass_prod"=expression(P[frass]),
#                "lerp_prod"=expression(P[lerp]),
#                "leaf_prod"=expression(NPP[leaf]),
#                "twig_prod"=expression(NPP[twig]),
#                "bark_prod"=expression(NPP[bark]),
#                "seed_prod"=expression(NPP[seed]),
#                "wood_prod"=expression(NPP[wood]),
#                "fineroot_prod"=expression(NPP[froot]),
#                "coarseroot_prod"=expression(NPP[croot]),
#                "understorey_prod"=expression(NPP[ua]))
#    
#    y.lab4 <- c("over_gpp"=expression(GPP[o]),
#                "understorey_gpp"=expression(GPP[u]))
#    
#    
#    ### add conditional color to data frames
#    plotDF2$col.con <- ifelse(plotDF2$effect_size<1, "red3", "blue2")
#    plotDF3$col.con <- ifelse(plotDF3$effect_size<1, "red3", "blue2")
#    plotDF4$col.con <- ifelse(plotDF4$effect_size<1, "red3", "blue2")
#    
#    #### Plotting
#    p9 <- ggplot(plotDF4)+ 
#        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
#                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
#                     size=6, color="grey")+
#        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
#                   stat='identity', size=4, shape=19, color=plotDF4$col.con)+
#        labs(x="eC minus aC", y="")+
#        theme_linedraw()+
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_blank(), 
#              axis.text.x = element_blank(),
#              axis.text.y=element_text(size=12),
#              axis.title.y=element_text(size=14),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_blank(),
#              legend.position="none")+
#        geom_vline(xintercept = 1.0)+
#        scale_y_discrete(labels=y.lab4)+
#        geom_vline(xintercept = 1.2, linetype="dashed", color="grey")+
#        geom_vline(xintercept = 3, linetype="dashed", color="grey")+
#        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)   
#    
#    
#    p10 <- ggplot(plotDF3)+ 
#        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
#                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
#                     size=6, color="grey")+
#        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
#                   stat='identity', size=4, shape=19, color=plotDF3$col.con)+
#        labs(x="eC minus aC", y="")+
#        theme_linedraw()+
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_blank(), 
#              axis.text.x = element_blank(),
#              axis.text.y=element_text(size=12),
#              axis.title.y=element_text(size=14),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_blank(),
#              legend.position="none")+
#        geom_vline(xintercept = 1.0)+
#        scale_y_discrete(labels=y.lab3)+
#        geom_vline(xintercept = 1.2, linetype="dashed", color="grey")+
#        geom_vline(xintercept = 3, linetype="dashed", color="grey")+
#        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)    
#    
#    p11 <- ggplot(plotDF2)+ 
#        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
#                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
#                     size=6, color="grey")+
#        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
#                   stat='identity', size=4, shape=19, color=plotDF2$col.con)+
#        labs(x=expression(paste(CO[2], " effect (%)")), y="")+
#        theme_linedraw()+
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_text(size=14), 
#              axis.text.x = element_text(size=12),
#              axis.text.y=element_text(size=12),
#              axis.title.y=element_text(size=14),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_blank(),
#              legend.position="bottom")+
#        geom_vline(xintercept = 1.0)+
#        scale_y_discrete(labels=y.lab2)+        
#        geom_vline(xintercept = 1.2, linetype="dashed", color="grey")+
#        geom_vline(xintercept = 3, linetype="dashed", color="grey")+
#        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)  
#
#    
#    
#    pdf("R_other/treatment_effect_ratio_no_interaction_change_in_pool.pdf", width=8, height=10)
#    require(cowplot)    
#    plot_grid(p9, p10, 
#              p11, 
#              labels="AUTO", ncol=1, align="v", axis = "l",
#              rel_heights=c(0.2,0.8,0.8))
#    dev.off()
#    
#    
#    
    ######### 7. Plot abs, no interactions, and change in pools, with covariates
    ### read in the csv file to plot the treatment effect and confidence interval
    myDF <- read.csv("R_other/treatment_statistics_abs_no_interaction_with_covariate.csv")

    #### assign color scheme according to treatment p-value
    myDF$trt_sig[myDF$Trt_Pr > 0.1] <- "non-sig"
    myDF$trt_sig[myDF$Trt_Pr <= 0.1] <- "sig"
    
    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
    
    myDF <- myDF[complete.cases(myDF$effect_size),]
    
    #### exclude all pools
    myDF$Category <- c(rep("pool", 11), # 1 - 11
                       rep("resp", 2), # 12 - 13
                       rep("prod", 1), # 14
                       rep("resp", 2), # 15 - 16
                       rep("prod", 1), # 17
                       rep("resp", 2), # 18 - 19
                       rep("prod", 9), # 20 - 28
                       rep("resp", 1), # 29
                       rep("prod",1),  # 30
                       rep("gpp", 1),  # 31
                       rep("resp", 2), # 32 - 33
                       #rep("prod",1),  # 33
                       rep("gpp", 1),  # 34
                       rep("change_in_pool", 11))   # 35 - 45
    
    ### Drop redundant pools and fluxes
    myDF <- subset(myDF, Variable != c("delta_understorey_c_2"))
    myDF <- subset(myDF, Variable != c("understorey_lit"))
    
    ### Drop CWD - confidence interval too large
    myDF <- subset(myDF, Variable != c("delta_cwd_c"))
    
    plotDF1 <- subset(myDF, Category == "change_in_pool")
    plotDF2 <- subset(myDF, Category == "resp")
    plotDF3 <- subset(myDF, Category == "prod")
    plotDF4 <- subset(myDF, Category == "gpp")
    
    ### transform the data so that we could have break point on x axis
    #Function to transform data to y positions
    trans <- function(x) {
        if (x < 0) {
            pmax(x,-50) + 0.2*pmin(x+50,0)
        } else {
            pmin(x,50) + 0.2*pmax(x-50,0)
        }
    }
    
    xticks.brk <- xticks <- c(-400, -200, -50, -25, 0, 25, 50, 150, 300, 600)
    
    #Transform the data onto the display scale
    for (i in 1:length(plotDF1$Variable)) {
        plotDF1$effect_size_t[i] <- trans(plotDF1$effect_size[i])
        plotDF1$conf_low_t[i] <- trans(plotDF1$conf_low[i])
        plotDF1$conf_high_t[i] <- trans(plotDF1$conf_high[i])
    }   
    
    for (i in 1:length(plotDF2$Variable)) {
        plotDF2$effect_size_t[i] <- trans(plotDF2$effect_size[i])
        plotDF2$conf_low_t[i] <- trans(plotDF2$conf_low[i])
        plotDF2$conf_high_t[i] <- trans(plotDF2$conf_high[i])
    }  
    
    for (i in 1:length(plotDF3$Variable)) {
        plotDF3$effect_size_t[i] <- trans(plotDF3$effect_size[i])
        plotDF3$conf_low_t[i] <- trans(plotDF3$conf_low[i])
        plotDF3$conf_high_t[i] <- trans(plotDF3$conf_high[i])
    }  
    
    for (i in 1:length(plotDF4$Variable)) {
        plotDF4$effect_size_t[i] <- trans(plotDF4$effect_size[i])
        plotDF4$conf_low_t[i] <- trans(plotDF4$conf_low[i])
        plotDF4$conf_high_t[i] <- trans(plotDF4$conf_high[i])
    }  
    
    for (i in 1:length(xticks)) {
        xticks.brk[i] <- trans(xticks[i])
    }
    
    y.lab1 <- c("delta_soil_c"=expression(Delta*C[soil]),
                "delta_leaf_c"=expression(Delta*C[leaf]),
                "delta_wood_c"=expression(Delta*C[wood]),
                "delta_fineroot_c"=expression(Delta*C[froot]),
                "delta_coarseroot_c"=expression(Delta*C[croot]),
                "delta_understorey_c"=expression(Delta*C[ua]),
                "delta_understorey_c_live"=expression(Delta*C[ua_live]),
                "delta_understorey_c_dead"=expression(Delta*C[ua_dead]),
                "delta_microbial_c"=expression(Delta*C[micr]),
                "delta_mycorrhizal_c"=expression(Delta*C[myco]),
                "delta_litter_c"=expression(Delta*C[lit]),
                "delta_insect_c"=expression(Delta*C[ins]))
    
    y.lab2 <- c("wood_respiration"=expression(R[wood]),
                "root_respiration"=expression(R[root]),
                "understorey_respiration"=expression(R[ua]),
                "herb_consump"=expression(NPP[hb]),
                "herb_respiration"=expression(R[hb]),
                "soil_respiration"=expression(R[soil]),
                "doc"=expression(L[doc]),
                "hetero_respiration"=expression(R[rh]),
                "over_leaf_respiration"=expression(R[leaf]))
    
    y.lab3 <- c("frass_prod"=expression(P[frass]),
                "lerp_prod"=expression(P[lerp]),
                "leaf_prod"=expression(NPP[leaf]),
                "twig_prod"=expression(NPP[twig]),
                "bark_prod"=expression(NPP[bark]),
                "seed_prod"=expression(NPP[seed]),
                "wood_prod"=expression(NPP[wood]),
                "fineroot_prod"=expression(NPP[froot]),
                "coarseroot_prod"=expression(NPP[croot]),
                "understorey_prod"=expression(NPP[ua]),
                #"mycorrhizal_prod"=expression(NPP[myc]),
                "ch4"=expression(U[CH[4]]))
    
    y.lab4 <- c("over_gpp"=expression(GPP[o]),
                "understorey_gpp"=expression(GPP[u]))
    
    
    ### add conditional color to data frames
    plotDF1$col.con <- ifelse(plotDF1$effect_size_t<0, "red3", "blue2")
    plotDF2$col.con <- ifelse(plotDF2$effect_size_t<0, "red3", "blue2")
    plotDF3$col.con <- ifelse(plotDF3$effect_size_t<0, "red3", "blue2")
    plotDF4$col.con <- ifelse(plotDF4$effect_size_t<0, "red3", "blue2")
    
    #### Plotting
    p8 <- ggplot(plotDF4)+ 
        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
                   stat='identity', size=4, shape=19, color=plotDF4$col.con)+
        labs(x="eC minus aC", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        geom_vline(xintercept = 0.0)+
        geom_vline(xintercept = -50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = -55, linetype="dashed", color="grey")+
        geom_vline(xintercept = 50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = 55, linetype="dashed", color="grey")+
        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
        scale_y_discrete(labels=y.lab4)
    
    
    p5 <- ggplot(plotDF1)+ 
        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
                   stat='identity', size=4, shape=19, color=plotDF1$col.con)+
        #scale_color_manual(name="Treatment significance", 
        #                   labels = c("Non-sig", "Sig (P<0.1)"), 
        #                   values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
        #scale_shape_manual(name="Date Significance", 
        #                   labels = c("Non-sig", "Sig (P<0.05)"), 
        #                   values = c(1, 2))+ 
        labs(x="eC minus aC", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        geom_vline(xintercept = 0.0)+
        geom_vline(xintercept = -50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = -55, linetype="dashed", color="grey")+
        geom_vline(xintercept = 50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = 55, linetype="dashed", color="grey")+
        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
        scale_y_discrete(labels=y.lab1)
    
    p6 <- ggplot(plotDF3)+ 
        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
                   stat='identity', size=4, shape=19, color=plotDF3$col.con)+
        labs(x="eC minus aC", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        geom_vline(xintercept = 0.0)+
        geom_vline(xintercept = -50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = -55, linetype="dashed", color="grey")+
        geom_vline(xintercept = 50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = 55, linetype="dashed", color="grey")+
        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
        scale_y_discrete(labels=y.lab3)
    
    p7 <- ggplot(plotDF2)+ 
        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
                   stat='identity', size=4, shape=19, color=plotDF2$col.con)+
        scale_color_manual(name="Treatment significance", 
                           labels = c("Non-sig", "Sig (P<0.1)"), 
                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
        labs(x=expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom")+
        geom_vline(xintercept = 0.0)+
        geom_vline(xintercept = -50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = -55, linetype="dashed", color="grey")+
        geom_vline(xintercept = 50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = 55, linetype="dashed", color="grey")+
        #geom_rect(aes(xmin=-50, xmax=-40, ymin=0, ymax=18), fill="white")+
        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
        scale_y_discrete(labels=y.lab2)
    
    #grid.labs <- c("(a)", "(b)", "(c)")
    
    #require(grid)
    
    ## plot 
    #pdf("R_other/treatment_effect_abs_no_interaction_change_in_pool.pdf", width=8, height=10)
    #grid.newpage()
    #grid.draw(rbind(ggplotGrob(p8), ggplotGrob(p5), ggplotGrob(p6), 
    #                ggplotGrob(p7), size="last"))
    #grid.text(grid.labs,x = 0.95, y = c(0.95, 0.63, 0.32),
    #          gp=gpar(fontsize=16, col="black", fontface="bold"))
    #dev.off()
    
    pdf("R_other/treatment_effect_abs_no_interaction_change_in_pool_with_covariate.pdf", width=8, height=12)
    require(cowplot)    
    plot_grid(p8, p5, 
              p6, p7, 
              labels="AUTO", ncol=1, align="v", axis = "l",
              rel_heights=c(0.3,1,1,0.9))
    dev.off()
    
}
