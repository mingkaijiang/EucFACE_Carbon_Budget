gpp_and_rsoil_gap_plot <- function(inDF) {
    
    ### subseting NPP DF
    ### aCO2
    temDF <- inDF$npp[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6")]
    
    ### only include plant NPP
    nppDF <- rbind(temDF[temDF$term == "Leaf NPP",],temDF[temDF$term == "Stem NPP",],
                   temDF[temDF$term == "Fine Root NPP",],temDF[temDF$term == "Coarse Root NPP",],
                   temDF[temDF$term == "Other NPP",],temDF[temDF$term == "Understorey NPP",],
                   temDF[temDF$term == "Leaf consumption",])
    
    ### subsetting inout df
    temDF <- inDF$inout[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6")]
    
    ### only include GPP
    gppDF <- rbind(temDF[temDF$term == "GPP overstorey",],temDF[temDF$term == "GPP understorey",])
    
    ### only include respiration
    raDF <- rbind(temDF[temDF$term == "Ra leaf",],temDF[temDF$term == "Ra stem",],
                  temDF[temDF$term == "Ra root",],temDF[temDF$term == "Ra understorey",],
                  temDF[temDF$term == "Rgrowth",])
    
    ### calculated gpp based on NPP + Ra
    gppDF.est <- rbind(nppDF, raDF)
    gppDF.est$cat <- "NPP+Ra"
    gppDF$cat <- "MAESPA"
    
    plotDF <- rbind(gppDF, gppDF.est)
    
    ### convert unit from g C to kg C
    plotDF$Ring_1 <- plotDF$Ring_1/1000
    plotDF$Ring_2 <- plotDF$Ring_2/1000
    plotDF$Ring_3 <- plotDF$Ring_3/1000
    plotDF$Ring_4 <- plotDF$Ring_4/1000
    plotDF$Ring_5 <- plotDF$Ring_5/1000
    plotDF$Ring_6 <- plotDF$Ring_6/1000
    
    ### Prepare variable labels
    var.labs1 <- c(expression(GPP[o]), expression(GPP[u]),
                  expression(NPP[leaf]), expression(NPP[wood]),
                  expression(NPP[froot]), expression(NPP[croot]),
                  expression(NPP[other]), expression(NPP[ua]),
                  expression(NPP[hb]), expression(R[leaf]),
                  expression(R[wood]), expression(R[root]),
                  expression(R[ua]), #expression(R[hb]),
                  expression(R[g]))
    
    ### Prepare variable colors
    require(viridis)
    col.list1 <- viridis(14)
    
    plotDF$term <- factor(plotDF$term, levels=unique(plotDF$term))
    
    
    ### make the bar plot
    p1 <- ggplot(plotDF,
                aes(cat, Ring_1)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("") + ylab("") +
        scale_x_discrete(labels=c("GPP", expression(paste("NPP+", R[a]))))+
        scale_fill_manual(name="GPP", 
                          values = col.list1,
                          labels=var.labs1) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylim(0, 3)
        
    
    p2 <- ggplot(plotDF,
                 aes(cat, Ring_2)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("") +ylab(expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("GPP", expression(paste("NPP+", R[a]))))+
        scale_fill_manual(name="GPP", 
                          values = col.list1,
                          labels=var.labs1) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylim(0, 3)
    
    p3 <- ggplot(plotDF,
                 aes(cat, Ring_3)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("") + ylab("") +
        scale_x_discrete(labels=c("GPP", expression(paste("NPP+", R[a]))))+
        scale_fill_manual(name="GPP", 
                          values = col.list1,
                          labels=var.labs1) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylim(0, 3)
    
    p4 <- ggplot(plotDF,
                 aes(cat, Ring_4)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("") + ylab("") +
        scale_x_discrete(labels=c("GPP", expression(paste("NPP+", R[a]))))+
        scale_fill_manual(name="GPP", 
                          values = col.list1,
                          labels=var.labs1) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylim(0, 3)
    
    p5 <- ggplot(plotDF,
                 aes(cat, Ring_5)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("") + ylab("") +
        scale_x_discrete(labels=c("GPP", expression(paste("NPP+", R[a]))))+
        scale_fill_manual(name="GPP", 
                          values = col.list1,
                          labels=var.labs1) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        ylim(0, 3)
    
    p6 <- ggplot(plotDF,
                 aes(cat, Ring_6)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("") + ylab("") +
        scale_x_discrete(labels=c("GPP", expression(paste("NPP+", R[a]))))+
        scale_fill_manual(name="GPP", 
                          values = col.list1,
                          labels=var.labs1) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylim(0, 3)
    
    ### subseting NPP DF
    temDF <- inDF$npp[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6")]
    
    ### only include plant NPP
    nppDF <- rbind(temDF[temDF$term == "Leaf NPP",],
                   temDF[temDF$term == "Fine Root NPP",],temDF[temDF$term == "Coarse Root NPP",],
                   temDF[temDF$term == "Other NPP",],temDF[temDF$term == "Understorey NPP",],
                   temDF[temDF$term == "Frass production",])
    aDF <- inDF$inout[, c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6")]
    nppDF <- rbind(nppDF, aDF[aDF$term == "Ra root",])
    
    ### Add change in soil pool into NPP df
    #sDF <- subset(soil_c_pool, Date>"2012-06-17")
    #delta.soil <- data.frame("delta.soil.c", NA, NA, NA, NA, NA, NA)
    #colnames(delta.soil) <- c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6")
    #s.date <- min(sDF$Date)
    #e.date <- max(sDF$Date)
    #ndays <- as.numeric(e.date - s.date)
    #delta.soil[1,2:7] <- sDF[sDF$Date==e.date, "soil_carbon_pool"] - sDF[sDF$Date==s.date, "soil_carbon_pool"]
    #delta.soil[1,2:7] <- delta.soil[1,2:7] / ndays * 365
    #
    #nppDF <- rbind(nppDF, delta.soil)
    
    ### subsetting inout df
    temDF <- inDF$inout[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6")]
    
    ### only include Rsoil
    rsoilDF <- temDF[temDF$term == "Rsoil",]
    
    nppDF$cat <- "Litter+Rroot"
    rsoilDF$cat <- "Rsoil"
    
    plotDF <- rbind(nppDF, rsoilDF)
    plotDF$term <- factor(plotDF$term, levels=unique(plotDF$term))
    
    ### convert unit from g C to kg C
    plotDF$Ring_1 <- as.numeric(plotDF$Ring_1)/1000
    plotDF$Ring_2 <- as.numeric(plotDF$Ring_2)/1000
    plotDF$Ring_3 <- as.numeric(plotDF$Ring_3)/1000
    plotDF$Ring_4 <- as.numeric(plotDF$Ring_4)/1000
    plotDF$Ring_5 <- as.numeric(plotDF$Ring_5)/1000
    plotDF$Ring_6 <- as.numeric(plotDF$Ring_6)/1000
    

    ### Prepare variable labels
    var.labs2 <- c(expression(NPP[l]), expression(NPP[froot]),
                  expression(NPP[croot]),expression(NPP[other]), 
                  expression(NPP[ua]),expression(P[frass]), 
                  expression(R[root]),expression(R[soil]))
    
    ### Prepare variable colors
    col.list2 <- viridis(8)
    
    ### make the bar plot
    p7 <- ggplot(plotDF,
                 aes(cat, Ring_1)) +
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("") + ylab("") +
        scale_x_discrete(labels=c(expression(paste("Litter+", R[root])),
                                  expression(R[soil])))+
        scale_fill_manual(name="Rsoil", 
                          values = col.list2,
                          labels=var.labs2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylim(0, 1.5)
    
    p8 <- ggplot(plotDF,
                 aes(cat, Ring_2)) +
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("") + ylab(expression(paste(R[soil], " (kg C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(paste("Litter+", R[root])),
                                  expression(R[soil])))+
        scale_fill_manual(name="Rsoil", 
                          values = col.list2,
                          labels=var.labs2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylim(0, 1.5)
    
    p9 <- ggplot(plotDF,
                 aes(cat, Ring_3)) +
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("") + ylab("") +
        scale_x_discrete(labels=c(expression(paste("Litter+", R[root])),
                                  expression(R[soil])))+
        scale_fill_manual(name="Rsoil", 
                          values = col.list2,
                          labels=var.labs2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylim(0, 1.5)
    
    p10 <- ggplot(plotDF,
                 aes(cat, Ring_4)) +
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("") + ylab("") +
        scale_x_discrete(labels=c(expression(paste("Litter+", R[root])),
                                  expression(R[soil])))+
        scale_fill_manual(name="Rsoil", 
                          values = col.list2,
                          labels=var.labs2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylim(0, 1.5)
    
    p11 <- ggplot(plotDF,
                 aes(cat, Ring_5)) +
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("") + ylab("") +
        scale_x_discrete(labels=c(expression(paste("Litter+", R[root])),
                                  expression(R[soil])))+
        scale_fill_manual(name="Rsoil", 
                          values = col.list2,
                          labels=var.labs2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        ylim(0, 1.5)
    
    p12 <- ggplot(plotDF,
                 aes(cat, Ring_6)) +
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("") + ylab("") +
        scale_x_discrete(labels=c(expression(paste("Litter+", R[root])),
                                  expression(R[soil])))+
        scale_fill_manual(name="Rsoil", 
                          values = col.list2,
                          labels=var.labs2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        ylim(0, 1.5)
    

    require(grid)
    require(cowplot)
    
    pdf("output/GPP_Rsoil_gap_plot_per_ring.pdf", width=14,height=10)
    plot_grid(p2, p3, p6, p1, p4, p5, 
              p8, p9, p12, p7, p10, p11, 
              labels=c("R2", "R3", "R6", "R1", "R4", "R5",
                       "R2", "R3", "R6", "R1", "R4", "R5"), 
              ncol=6, align="v", axis="l",
                  rel_widths=c(1,1,1,1,1,1.6))
    dev.off()

}
