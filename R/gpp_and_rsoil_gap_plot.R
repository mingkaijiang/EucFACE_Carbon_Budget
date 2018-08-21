gpp_and_rsoil_gap_plot <- function(inDF) {
    
    ### subseting NPP DF
    ### aCO2
    temDF <- inDF$npp[,c("term", "aCO2", "aCO2_sd")]
    
    ### only include plant NPP
    nppDF <- rbind(temDF[temDF$term == "Leaf NPP",],temDF[temDF$term == "Stem NPP",],
                   temDF[temDF$term == "Fine Root NPP",],temDF[temDF$term == "Coarse Root NPP",],
                   temDF[temDF$term == "Other NPP",],temDF[temDF$term == "Understorey NPP",],
                   temDF[temDF$term == "Leaf consumption",])
    
    ### subsetting inout df
    temDF <- inDF$inout[,c("term", "aCO2", "aCO2_sd")]
    
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
    plotDF$aCO2 <- plotDF$aCO2/1000
    plotDF$aCO2_sd <- plotDF$aCO2_sd/1000
    
    ### calculate missing C
    gpp <- sum(gppDF$aCO2, na.rm=T)
    npp <- sum(nppDF$aCO2, na.rm=T)
    ra <- sum(raDF$aCO2, na.rm=T)
    c.miss <- gpp - npp - ra
    
    ### prepare error bar ranges
    errDF <- data.frame(c("NPP+Ra", "MAESPA"), NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg")
    errDF$pos[errDF$cat=="NPP+Ra"] <- sum(plotDF$aCO2[plotDF$cat=="NPP+Ra"])+sum(plotDF$aCO2_sd[plotDF$cat=="NPP+Ra"])
    errDF$neg[errDF$cat=="NPP+Ra"] <- sum(plotDF$aCO2[plotDF$cat=="NPP+Ra"])-sum(plotDF$aCO2_sd[plotDF$cat=="NPP+Ra"])
    errDF$pos[errDF$cat=="MAESPA"] <- sum(plotDF$aCO2[plotDF$cat=="MAESPA"])+sum(plotDF$aCO2_sd[plotDF$cat=="MAESPA"])
    errDF$neg[errDF$cat=="MAESPA"] <- sum(plotDF$aCO2[plotDF$cat=="MAESPA"])-sum(plotDF$aCO2_sd[plotDF$cat=="MAESPA"])
    errDF$sum[errDF$cat=="NPP+Ra"] <- sum(plotDF$aCO2[plotDF$cat=="NPP+Ra"])
    errDF$sum[errDF$cat=="MAESPA"] <- sum(plotDF$aCO2[plotDF$cat=="MAESPA"])
    
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
                aes(cat, aCO2)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        geom_segment(data=errDF, aes(x=cat, xend=cat, y=neg, yend=pos), 
                     colour="black")+
        #geom_errorbar(data=errDF, mapping=aes(x=cat, ymin=neg, ymax=pos),
        #              width=0.1, size=1, color="grey")+ 
        geom_point(data=errDF, mapping=aes(x=cat, y=sum), 
                   size=4, shape=21, fill="white")+
        xlab("") + ylab(expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ")"))) +
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
        
    
    ### eCO2
    ### subseting NPP DF
    temDF <- inDF$npp[,c("term", "eCO2", "eCO2_sd")]
    
    ### only include plant NPP
    nppDF <- rbind(temDF[temDF$term == "Leaf NPP",],temDF[temDF$term == "Stem NPP",],
                   temDF[temDF$term == "Fine Root NPP",],temDF[temDF$term == "Coarse Root NPP",],
                   temDF[temDF$term == "Other NPP",],temDF[temDF$term == "Understorey NPP",],
                   temDF[temDF$term == "Leaf consumption",])
    
    ### subsetting inout df
    temDF <- inDF$inout[,c("term", "eCO2", "eCO2_sd")]
    
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
    plotDF$eCO2 <- plotDF$eCO2/1000
    plotDF$eCO2_sd <- plotDF$eCO2_sd/1000
    
    ### prepare error bar ranges
    errDF <- data.frame(c("NPP+Ra", "MAESPA"), NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg")
    errDF$pos[errDF$cat=="NPP+Ra"] <- sum(plotDF$eCO2[plotDF$cat=="NPP+Ra"])+sum(plotDF$eCO2_sd[plotDF$cat=="NPP+Ra"])
    errDF$neg[errDF$cat=="NPP+Ra"] <- sum(plotDF$eCO2[plotDF$cat=="NPP+Ra"])-sum(plotDF$eCO2_sd[plotDF$cat=="NPP+Ra"])
    errDF$pos[errDF$cat=="MAESPA"] <- sum(plotDF$eCO2[plotDF$cat=="MAESPA"])+sum(plotDF$eCO2_sd[plotDF$cat=="MAESPA"])
    errDF$neg[errDF$cat=="MAESPA"] <- sum(plotDF$eCO2[plotDF$cat=="MAESPA"])-sum(plotDF$eCO2_sd[plotDF$cat=="MAESPA"])
    errDF$sum[errDF$cat=="NPP+Ra"] <- sum(plotDF$eCO2[plotDF$cat=="NPP+Ra"])
    errDF$sum[errDF$cat=="MAESPA"] <- sum(plotDF$eCO2[plotDF$cat=="MAESPA"])
    
    plotDF$term <- factor(plotDF$term, levels=unique(plotDF$term))
    
    
    ### make the bar plot
    p2 <- ggplot(plotDF,
                 aes(cat, eCO2)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        geom_segment(data=errDF, aes(x=cat, xend=cat, y=neg, yend=pos), 
                     colour="black")+
        #geom_errorbar(data=errDF, mapping=aes(x=cat, ymin=neg, ymax=pos),
        #              width=0.1, size=1, color="grey")+ 
        geom_point(data=errDF, mapping=aes(x=cat, y=sum), 
                   size=4, shape=21, fill="white")+
        xlab("") + ylab(expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ")"))) +
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
    
    ### subseting NPP DF
    temDF <- inDF$npp[,c("term", "aCO2", "aCO2_sd")]
    
    ### only include plant NPP
    nppDF <- rbind(temDF[temDF$term == "Leaf NPP",],
                   temDF[temDF$term == "Fine Root NPP",],temDF[temDF$term == "Coarse Root NPP",],
                   temDF[temDF$term == "Other NPP",],temDF[temDF$term == "Understorey NPP",],
                   temDF[temDF$term == "Frass production",])
    aDF <- inDF$inout[, c("term", "aCO2", "aCO2_sd")]
    nppDF <- rbind(nppDF, aDF[aDF$term == "Ra root",])
    
    ### subsetting inout df
    temDF <- inDF$inout[,c("term", "aCO2", "aCO2_sd")]
    
    ### only include Rsoil
    rsoilDF <- temDF[temDF$term == "Rsoil",]
    
    nppDF$cat <- "Litter+Rroot"
    rsoilDF$cat <- "Rsoil"
    
    plotDF <- rbind(nppDF, rsoilDF)
    plotDF$term <- factor(plotDF$term, levels=unique(plotDF$term))
    
    ### convert unit from g C to kg C
    plotDF$aCO2 <- plotDF$aCO2/1000
    plotDF$aCO2_sd <- plotDF$aCO2_sd/1000
    
    ### prepare error bar ranges
    errDF <- data.frame(c("Litter+Rroot", "Rsoil"), NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg")
    errDF$pos[errDF$cat=="Litter+Rroot"] <- sum(plotDF$aCO2[plotDF$cat=="Litter+Rroot"])+sum(plotDF$aCO2_sd[plotDF$cat=="Litter+Rroot"])
    errDF$neg[errDF$cat=="Litter+Rroot"] <- sum(plotDF$aCO2[plotDF$cat=="Litter+Rroot"])-sum(plotDF$aCO2_sd[plotDF$cat=="Litter+Rroot"])
    errDF$pos[errDF$cat=="Rsoil"] <- sum(plotDF$aCO2[plotDF$cat=="Rsoil"])+sum(plotDF$aCO2_sd[plotDF$cat=="Rsoil"])
    errDF$neg[errDF$cat=="Rsoil"] <- sum(plotDF$aCO2[plotDF$cat=="Rsoil"])-sum(plotDF$aCO2_sd[plotDF$cat=="Rsoil"])
    errDF$sum[errDF$cat=="Litter+Rroot"] <- sum(plotDF$aCO2[plotDF$cat=="Litter+Rroot"])
    errDF$sum[errDF$cat=="Rsoil"] <- sum(plotDF$aCO2[plotDF$cat=="Rsoil"])
    
    ### Prepare variable labels
    var.labs2 <- c(expression(NPP[l]), expression(NPP[froot]),
                  expression(NPP[croot]),expression(NPP[other]), 
                  expression(NPP[ua]),expression(P[frass]), 
                  expression(R[root]),expression(R[soil]))
    
    ### Prepare variable colors
    col.list2 <- viridis(8)
    
    ### make the bar plot
    p3 <- ggplot(plotDF,
                 aes(cat, aCO2)) +
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        geom_point(data=errDF, mapping=aes(x=cat, y=sum), 
                   size=4, shape=21, fill="white")+
        geom_segment(data=errDF, aes(x=cat, xend=cat, y=neg, yend=pos), 
                     colour="black")+
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
    
    ### eCO2
    ### subseting NPP DF
    temDF <- inDF$npp[,c("term", "eCO2", "eCO2_sd")]
    
    ### only include plant NPP
    nppDF <- rbind(temDF[temDF$term == "Leaf NPP",],
                   temDF[temDF$term == "Fine Root NPP",],temDF[temDF$term == "Coarse Root NPP",],
                   temDF[temDF$term == "Other NPP",],temDF[temDF$term == "Understorey NPP",],
                   temDF[temDF$term == "Frass production",])
    aDF <- inDF$inout[, c("term", "eCO2", "eCO2_sd")]
    nppDF <- rbind(nppDF, aDF[aDF$term == "Ra root",])
    
    ### subsetting inout df
    temDF <- inDF$inout[,c("term", "eCO2", "eCO2_sd")]
    
    ### only include Rsoil
    rsoilDF <- temDF[temDF$term == "Rsoil",]
    
    nppDF$cat <- "Litter+Rroot"
    rsoilDF$cat <- "Rsoil"
    
    plotDF <- rbind(nppDF, rsoilDF)
    plotDF$term <- factor(plotDF$term, levels=unique(plotDF$term))
    
    ### convert unit from g C to kg C
    plotDF$eCO2 <- plotDF$eCO2/1000
    plotDF$eCO2_sd <- plotDF$eCO2_sd/1000
    
    ### prepare error bar ranges
    errDF <- data.frame(c("Litter+Rroot", "Rsoil"), NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg")
    errDF$pos[errDF$cat=="Litter+Rroot"] <- sum(plotDF$eCO2[plotDF$cat=="Litter+Rroot"])+sum(plotDF$eCO2_sd[plotDF$cat=="Litter+Rroot"])
    errDF$neg[errDF$cat=="Litter+Rroot"] <- sum(plotDF$eCO2[plotDF$cat=="Litter+Rroot"])-sum(plotDF$eCO2_sd[plotDF$cat=="Litter+Rroot"])
    errDF$pos[errDF$cat=="Rsoil"] <- sum(plotDF$eCO2[plotDF$cat=="Rsoil"])+sum(plotDF$eCO2_sd[plotDF$cat=="Rsoil"])
    errDF$neg[errDF$cat=="Rsoil"] <- sum(plotDF$eCO2[plotDF$cat=="Rsoil"])-sum(plotDF$eCO2_sd[plotDF$cat=="Rsoil"])
    errDF$sum[errDF$cat=="Litter+Rroot"] <- sum(plotDF$eCO2[plotDF$cat=="Litter+Rroot"])
    errDF$sum[errDF$cat=="Rsoil"] <- sum(plotDF$eCO2[plotDF$cat=="Rsoil"])
    
    ### make the bar plot
    p4 <- ggplot(plotDF,
                aes(cat, eCO2)) +
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        geom_point(data=errDF, mapping=aes(x=cat, y=sum), 
                   size=4, shape=21, fill="white")+
        geom_segment(data=errDF, aes(x=cat, xend=cat, y=neg, yend=pos), 
                     colour="black")+
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
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)+
        ylim(0, 1.5)

    require(grid)
    require(cowplot)
    
    pdf("output/GPP_Rsoil_gap_plots.pdf", width=12,height=10)
    plot_grid(p1, p2, p3, p4, labels="AUTO", ncol=2, align="v", axis="l",
                  rel_widths=c(1,1.2))
    dev.off()

}
