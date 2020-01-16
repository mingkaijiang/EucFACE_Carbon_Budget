make_delta_C_pool_plot <- function(inDF) {
    
    ### subseting each method
    deltaDF <- as.data.frame(inDF$delta_pool)

    deltaDF$term <- c("delta_leaf_c", "delta_wood_c", "delta_understorey_c",
                   "delta_fineroot_c", "delta_intermediate_root_c", "delta_coarse_root_c",
                   "delta_litter_c","delta_microbial_c","delta_soil_c",
                   "delta_mycorrhizal_c", "delta_insect_c")
    
    plotDF1 <- deltaDF[,c("term", "aCO2", "aCO2_sd")]
    plotDF2 <- deltaDF[,c("term", "eCO2", "eCO2_sd")]
    plotDF1$Treatment <- "aCO2"
    plotDF2$Treatment <- "eCO2"
    
    colnames(plotDF1) <- colnames(plotDF2) <- c("term", "value", "sd", "Treatment")
    
    plotDF <- rbind(plotDF1, plotDF2)
    
    
    plotDF$pos <- plotDF$value + plotDF$sd
    plotDF$neg <- plotDF$value - plotDF$sd
    
    
    ### scale resizing
    #Function to transform data to y positions
    trans <- function(x) {
        if (x < 0) {
            pmax(x,-50) + 0.2*pmin(x+50,0)
        } else {
            pmin(x,50) + 0.2*pmax(x-50,0)
        }
    }
    
    yticks.brk <- yticks <- c(-300,-50, -25, 0, 25, 50, 150)
    
    #Transform the data onto the display scale
    for (i in 1:length(plotDF$term)) {
        plotDF$value_t[i] <- trans(plotDF$value[i])
        plotDF$pos_t[i] <- trans(plotDF$pos[i])
        plotDF$neg_t[i] <- trans(plotDF$neg[i])
    }   
    
    for (i in 1:length(yticks)) {
        yticks.brk[i] <- trans(yticks[i])
    }
    
    ### set up labels
    x.lab1 <- c("delta_soil_c"=expression(Delta*C[soil]),
                "delta_leaf_c"=expression(Delta*C[ol]),
                "delta_wood_c"=expression(Delta*C[stem]),
                "delta_fineroot_c"=expression(Delta*C[froot]),
                "delta_intermediate_root_c"=expression(Delta*C[iroot]),
                "delta_coarse_root_c"=expression(Delta*C[croot]),
                "delta_understorey_c"=expression(Delta*C[ua]),
                "delta_understorey_c_live"=expression(Delta*C[ua_live]),
                "delta_understorey_c_dead"=expression(Delta*C[ua_dead]),
                "delta_microbial_c"=expression(Delta*C[micr]),
                "delta_mycorrhizal_c"=expression(Delta*C[myco]),
                "delta_litter_c"=expression(Delta*C[lit]),
                "delta_insect_c"=expression(Delta*C[ins]))
    
    ### make the bar plot
    p1 <- ggplot(plotDF, aes(x=term, y=value_t))+
        geom_bar(stat = "identity", aes(fill=Treatment), position="dodge") +
        geom_errorbar(aes(ymax=pos_t, ymin=neg_t, color=factor(Treatment)), 
                      position = position_dodge(0.9), width=0.25, size=0.2) +
        geom_hline(yintercept = 0, color="black")+
        xlab("") + ylab(expression(Delta * C[pool] * " (g C " * m^-2 * " " * yr^-1 * ")"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=6, family="Helvetica"), 
              axis.text.x = element_text(size=6, family="Helvetica"),
              axis.text.y=element_text(size=7, family="Helvetica"),
              axis.title.y=element_text(size=7, family="Helvetica"),
              legend.text=element_text(size=7, family="Helvetica"),
              legend.title=element_text(size=7, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0,
              legend.direction="vertical")+
        scale_color_manual(name="Treatment", values = c("aCO2" = "black", "eCO2" = "black"),
                           labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_fill_manual(name="Treatment", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        geom_hline(yintercept = -50, linetype="dashed", color="grey")+
        geom_hline(yintercept = 50, linetype="dashed", color="grey")+
        scale_y_continuous(limits=c(min(yticks.brk), max(yticks.brk)), breaks=yticks.brk, labels=yticks)+
        scale_x_discrete(labels=y.lab1)+
        guides(fill = guide_legend(ncol=2))
    
    #plot(p1)
    
    ggsave(filename = "output/ED_Figure_6.jpg", 
           plot = p1,
           width = 89, 
           height = 69,
           units = "mm",
           dpi = 300)
    
    
    
 }
