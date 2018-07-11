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
                expression(C[ua_live]),expression(C[ua_dead]),expression(C[micr]),expression(R[root]),
                expression(R[ua]),expression(L[frass]),expression(NPP[hb]),expression(R[hb]),
                expression(NPP[lerp]),expression(R[soil]),expression(L[doc]),expression(NPP[leaf]),
                expression(NPP[twig]),expression(NPP[bark]),expression(NPP[seed]),expression(NPP[wood]),
                expression(NPP[froot]),expression(NPP[croot]),expression(NPP[ua]),expression(R[rh]))
    
    #### Plotting
    p1 <- ggplot(myDF)+ 
        geom_point(aes(x=Variable, y=effect_size, shape=factor(date_sig)), stat='identity', size=2)+
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
                expression(C[ua_live]),expression(C[ua_dead]),expression(C[micr]),expression(R[root]),
                expression(R[ua]),expression(L[frass]),expression(NPP[hb]),expression(R[hb]),
                expression(NPP[lerp]),expression(R[soil]),expression(L[doc]),expression(NPP[leaf]),
                expression(NPP[twig]),expression(NPP[bark]),expression(NPP[seed]),expression(NPP[wood]),
                expression(NPP[froot]),expression(NPP[croot]),expression(NPP[ua]),expression(R[rh]))
    
    #### Plotting
    p2 <- ggplot(myDF)+ 
        geom_point(aes(x=Variable, y=effect_size, shape=factor(date_sig)), stat='identity', size=2)+
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
    
    pdf("R_other/treatment_effect_ratio_interaction.pdf", width=6, height=8)
    plot(p2)
    dev.off()
    
    
}