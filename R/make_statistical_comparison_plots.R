make_statistical_comparison_plots <- function() {
    ######### 1. Plot ratios, no interactions
    ### read in the csv file to plot the treatment effect and confidence interval
    myDF <- read.csv("R_other/treatment_statistics_ratio_no_interaction.csv")
    
    #### assign color scheme according to treatment p-value
    myDF$trt_sig[myDF$Trt_Pr > 0.15] <- "non-sig"
    myDF$trt_sig[myDF$Trt_Pr <= 0.15] <- "sig"
    
    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
    
    #myDF$eff_factor[myDF$effect_size >1] <- "above"
    #myDF$eff_factor[myDF$effect_size <1] <- "below"
    
    myDF <- myDF[complete.cases(myDF$effect_size),]
    
    #### Prepare x labels
    x.labs <- c("LAI", "SLA", expression(C[soil]), expression(C[leaf]), expression(C[wood]),
                expression(C[froot]),expression(C[croot]),expression(C[ua]),expression(C[ua2]),
                expression(C[ua_live]),expression(C[ua_dead]),expression(C[micr]),expression(C[myco]),
                expression(C[lit]),expression(C[ins]),expression(C[cwd]),expression(R[root]),
                expression(R[ua]),expression(L[frass]),expression(NPP[hb]),expression(R[hb]),
                expression(NPP[lerp]),expression(R[soil]),expression(L[doc]),expression(NPP[leaf]),
                expression(NPP[twig]),expression(NPP[bark]),expression(NPP[seed]),expression(NPP[wood]),
                expression(NPP[froot]),expression(NPP[croot]),expression(NPP[ua]),expression(R[rh]))
    
    #### Plotting
    p1 <- ggplot(myDF)+ 
        geom_point(aes(x=Variable, y=effect_size, shape=factor(date_sig)), stat='identity', size=4)+
        geom_segment(aes(x=Variable, y=conf_low, xend=Variable, yend=conf_high, color=factor(trt_sig)))+
        scale_color_manual(name="Treatment significance", 
                           labels = c("Non-sig", "Sig (P<0.15)"), 
                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
        scale_shape_manual(name="Date Significance", 
                           labels = c("Non-sig", "Sig (P<0.05)"), 
                           values = c(1, 2))+ 
        ylim(0, 4)+coord_flip()+
        labs(x="Variable", y="eC/aC ratio")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        geom_hline(yintercept = 1.0)+
        scale_x_discrete(limits=myDF$Variable,labels=x.labs)
    
    pdf("R_other/treatment_effect_ratio_no_interaction.pdf", width=6, height=8)
    plot(p1)
    dev.off()
    
    
    ######### 2. Plot ratios, with interactions
    ### read in the csv file to plot the treatment effect and confidence interval
    myDF <- read.csv("R_other/treatment_statistics_ratio_interaction.csv")
    
    #### assign color scheme according to treatment p-value
    myDF$trt_sig[myDF$Trt_Pr > 0.15] <- "non-sig"
    myDF$trt_sig[myDF$Trt_Pr <= 0.15] <- "sig"
    
    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
    
    myDF$date_trt_sig[myDF$Trt_Date_Pr > 0.05] <- "non-sig"
    myDF$date_trt_sig[myDF$Trt_Date_Pr <= 0.05] <- "sig"
    
    myDF <- myDF[complete.cases(myDF$effect_size),]
    
    #### Prepare x labels
    x.labs <- c("LAI", "SLA", expression(C[soil]), expression(C[leaf]), expression(C[wood]),
                expression(C[froot]),expression(C[croot]),expression(C[ua]),expression(C[ua2]),
                expression(C[ua_live]),expression(C[ua_dead]),expression(C[micr]),expression(C[myco]),
                expression(C[lit]),expression(C[ins]),expression(C[cwd]),expression(R[root]),
                expression(R[ua]),expression(L[frass]),expression(NPP[hb]),expression(R[hb]),
                expression(NPP[lerp]),expression(R[soil]),expression(L[doc]),expression(NPP[leaf]),
                expression(NPP[twig]),expression(NPP[bark]),expression(NPP[seed]),expression(NPP[wood]),
                expression(NPP[froot]),expression(NPP[croot]),expression(NPP[ua]),expression(R[rh]))
    
    #### Plotting
    p2 <- ggplot(myDF)+ 
        geom_segment(aes(x=Variable, y=conf_low, xend=Variable, yend=conf_high, color=factor(trt_sig)))+
        geom_point(aes(x=Variable, y=effect_size, shape=factor(date_sig), fill=factor(date_trt_sig)), 
                   stat='identity', size=4)+
        scale_color_manual(name="Treatment significance", 
                           labels = c("Non-sig", "Sig (P<0.15)"), 
                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
        scale_shape_manual(name="Date Significance", 
                           labels = c("Non-sig", "Sig (P<0.05)"), 
                           values = c(21, 24))+ 
        scale_fill_manual(name="Interaction significance", 
                           labels = c("Non-sig", "Sig (P<0.05)"), 
                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+
        ylim(0, 7)+coord_flip()+
        labs(x="Variable", y="eC/aC ratio")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        geom_hline(yintercept = 1.0)+
        scale_x_discrete(limits=myDF$Variable,labels=x.labs)+
        guides(fill = guide_legend(override.aes = list(color = c("#f8766d", "#00ba38"))), 
               size = guide_legend(override.aes = list(shape = c(21, 21))))
    
    pdf("R_other/treatment_effect_ratio_interaction.pdf", width=6, height=8)
    plot(p2)
    dev.off()
    
    ######### 3. Plot abs, no interactions
    ### read in the csv file to plot the treatment effect and confidence interval
    myDF <- read.csv("R_other/treatment_statistics_abs_no_interaction.csv")
    
    #### assign color scheme according to treatment p-value
    myDF$trt_sig[myDF$Trt_Pr > 0.1] <- "non-sig"
    myDF$trt_sig[myDF$Trt_Pr <= 0.1] <- "sig"
    
    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
    
    myDF <- myDF[complete.cases(myDF$effect_size),]
    
    ### Ignore a few pool variables that vary too large
    myDF <- subset(myDF, conf_low >= - 200)
    
    
    #### Prepare x labels
    x.labs <- c("LAI", "SLA", expression(C[leaf]),
                expression(C[froot]),expression(C[croot]),expression(C[ua]),expression(C[ua2]),
                expression(C[ua_live]),expression(C[ua_dead]),expression(C[micr]),expression(C[myco]),
                expression(C[lit]),expression(C[ins]),expression(C[cwd]),expression(R[root]),
                expression(R[ua]),expression(L[frass]),expression(NPP[hb]),expression(R[hb]),
                expression(NPP[lerp]),expression(R[soil]),expression(L[doc]),expression(NPP[leaf]),
                expression(NPP[twig]),expression(NPP[bark]),expression(NPP[seed]),expression(NPP[wood]),
                expression(NPP[froot]),expression(NPP[croot]),expression(NPP[ua]),expression(R[rh]))
    
    #### Plotting
    p3 <- ggplot(myDF)+ 
        geom_point(aes(x=Variable, y=effect_size, shape=factor(date_sig)), stat='identity', size=4)+
        geom_segment(aes(x=Variable, y=conf_low, xend=Variable, yend=conf_high, color=factor(trt_sig)))+
        scale_color_manual(name="Treatment significance", 
                           labels = c("Non-sig", "Sig (P<0.1)"), 
                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
        scale_shape_manual(name="Date Significance", 
                           labels = c("Non-sig", "Sig (P<0.05)"), 
                           values = c(1, 2))+ 
        ylim(-200, 400)+coord_flip()+
        labs(x="Variable", y="eC minus aC")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        geom_hline(yintercept = 1.0)+
        scale_x_discrete(limits=myDF$Variable,labels=x.labs)
    
    pdf("R_other/treatment_effect_abs_no_interaction.pdf", width=6, height=8)
    plot(p3)
    dev.off()
    
    ######### 4. Plot abs, with interactions
    ### read in the csv file to plot the treatment effect and confidence interval
    myDF <- read.csv("R_other/treatment_statistics_abs_interaction.csv")
    
    #### assign color scheme according to treatment p-value
    myDF$trt_sig[myDF$Trt_Pr > 0.15] <- "non-sig"
    myDF$trt_sig[myDF$Trt_Pr <= 0.15] <- "sig"
    
    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
    
    myDF$date_trt_sig[myDF$Trt_Date_Pr > 0.05] <- "non-sig"
    myDF$date_trt_sig[myDF$Trt_Date_Pr <= 0.05] <- "sig"
    
    myDF <- myDF[complete.cases(myDF$effect_size),]
    
    ### Ignore a few pool variables that vary too large
    myDF <- subset(myDF, conf_low >= - 200)
    
    #### Prepare x labels
    x.labs <- c("LAI", "SLA", expression(C[leaf]),
                expression(C[froot]),expression(C[croot]),expression(C[ua]),expression(C[ua2]),
                expression(C[ua_live]),expression(C[ua_dead]),expression(C[micr]),expression(C[myco]),
                expression(C[lit]),expression(C[ins]),expression(C[cwd]),expression(R[root]),
                expression(R[ua]),expression(L[frass]),expression(NPP[hb]),expression(R[hb]),
                expression(NPP[lerp]),expression(R[soil]),expression(L[doc]),expression(NPP[leaf]),
                expression(NPP[twig]),expression(NPP[bark]),expression(NPP[seed]),expression(NPP[wood]),
                expression(NPP[froot]),expression(NPP[croot]),expression(NPP[ua]),expression(R[rh]))
    
    #### Plotting
    p4 <- ggplot(myDF)+ 
        geom_segment(aes(x=Variable, y=conf_low, xend=Variable, yend=conf_high, color=factor(trt_sig)))+
        geom_point(aes(x=Variable, y=effect_size, shape=factor(date_sig), fill=factor(date_trt_sig)), 
                   stat='identity', size=4)+
        scale_color_manual(name="Treatment significance", 
                           labels = c("Non-sig", "Sig (P<0.15)"), 
                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
        scale_shape_manual(name="Date Significance", 
                           labels = c("Non-sig", "Sig (P<0.05)"), 
                           values = c(21, 24))+ 
        scale_fill_manual(name="Interaction significance", 
                          labels = c("Non-sig", "Sig (P<0.05)"), 
                          values = c("non-sig"="#f8766d","sig"="#00ba38"))+
        ylim(-200, 400)+coord_flip()+
        labs(x="Variable", y="eC minus aC")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right")+
        geom_hline(yintercept = 1.0)+
        scale_x_discrete(limits=myDF$Variable,labels=x.labs)+
        guides(fill = guide_legend(override.aes = list(color = c("#f8766d", "#00ba38"))), 
               size = guide_legend(override.aes = list(shape = c(21, 21))))
    
    pdf("R_other/treatment_effect_abs_interaction.pdf", width=6, height=8)
    plot(p4)
    dev.off()
    
    
    ######### 5. Plot abs, no interactions, and change in pools
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
                       rep("change_in_pool", 14))   # 35 - 46
    
    ### Drop redundant pools and fluxes
    myDF <- subset(myDF, Variable != c("delta_understorey_c_2"))
    
    ### Drop CWD - confidence interval too large
    myDF <- subset(myDF, Variable != c("delta_cwd_c"))
    
    plotDF1 <- subset(myDF, Category == "change_in_pool")
    plotDF2 <- subset(myDF, Category == "resp")
    plotDF3 <- subset(myDF, Category == "prod")
    
    ### transform the data so that we could have break point on x axis
    #Function to transform data to y positions
    trans <- function(x) {
        if (x < 0) {
            pmax(x,-40) + 0.25*pmin(x+40,0)
        } else {
            pmin(x,40) + 0.25*pmax(x-40,0)
        }
    }
    
    xticks.brk <- xticks <- c(-150, -100, -40, -20, 0, 20, 40, 100, 200, 300)
    
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
    
    for (i in 1:length(xticks)) {
        xticks.brk[i] <- trans(xticks[i])
    }
    
    y.lab1 <- c("delta_soil_c"=expression(delta*C[soil]),
                "delta_leaf_c"=expression(delta*C[leaf]),
                "delta_wood_c"=expression(delta*C[wood]),
                "delta_fineroot_c"=expression(delta*C[froot]),
                "delta_coarseroot_c"=expression(delta*C[croot]),
                "delta_understorey_c"=expression(delta*C[ua]),
                "delta_understorey_c_live"=expression(delta*C[ua_live]),
                "delta_understorey_c_dead"=expression(delta*C[ua_dead]),
                "delta_microbial_c"=expression(delta*C[micr]),
                "delta_mycorrhizal_c"=expression(delta*C[myco]),
                "delta_litter_c"=expression(delta*C[lit]),
                "delta_insect_c"=expression(delta*C[ins]))
    
    y.lab2 <- c("root_respiration"=expression(R[root]),
                "understorey_respiration"=expression(R[ua]),
                "herb_consump"=expression(NPP[hb]),
                "herb_respiration"=expression(R[hb]),
                "soil_respiration"=expression(R[soil]),
                "doc"=expression(L[doc]),
                "hetero_respiration"=expression(R[rh]))
    
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
                "ch4"=expression(U[CH[4]]))
    
    #### Plotting
    p5 <- ggplot(plotDF1)+ 
        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
                   stat='identity', size=4, shape=19, color="red")+
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
        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
        scale_y_discrete(labels=y.lab1)
    
    p6 <- ggplot(plotDF3)+ 
        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
                   stat='identity', size=4, shape=19, color="red")+
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
        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
        scale_y_discrete(labels=y.lab3)
    
    p7 <- ggplot(plotDF2)+ 
        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
                     size=10, color="grey")+
        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
                   stat='identity', size=4, shape=19, color="red")+
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
        #geom_rect(aes(xmin=-50, xmax=-40, ymin=0, ymax=18), fill="white")+
        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
        scale_y_discrete(labels=y.lab2)
    
    grid.labs <- c("(a)", "(b)", "(c)")
    
    require(grid)
    
    ## plot 
    pdf("R_other/treatment_effect_abs_no_interaction_change_in_pool.pdf", width=8, height=10)
    grid.newpage()
    grid.draw(rbind(ggplotGrob(p5), ggplotGrob(p6), 
                    ggplotGrob(p7), size="last"))
    grid.text(grid.labs,x = 0.95, y = c(0.95, 0.63, 0.32),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
}