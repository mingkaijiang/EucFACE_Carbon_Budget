rsoil_gap_plot <- function(inDF) {
    
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
    errDF <- data.frame(rep(c("Litter+Rroot", "Rsoil"),2), NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg")
    errDF$pos[errDF$cat=="Litter+Rroot"] <- sum(plotDF$aCO2[plotDF$cat=="Litter+Rroot"])+sum(plotDF$aCO2_sd[plotDF$cat=="Litter+Rroot"])
    errDF$neg[errDF$cat=="Litter+Rroot"] <- sum(plotDF$aCO2[plotDF$cat=="Litter+Rroot"])-sum(plotDF$aCO2_sd[plotDF$cat=="Litter+Rroot"])
    errDF$pos[errDF$cat=="Rsoil"] <- sum(plotDF$aCO2[plotDF$cat=="Rsoil"])+sum(plotDF$aCO2_sd[plotDF$cat=="Rsoil"])
    errDF$neg[errDF$cat=="Rsoil"] <- sum(plotDF$aCO2[plotDF$cat=="Rsoil"])-sum(plotDF$aCO2_sd[plotDF$cat=="Rsoil"])
    
    ### Prepare variable labels
    var.labs <- c(expression(NPP[l]), expression(NPP[froot]),
                  expression(NPP[croot]),expression(NPP[other]), 
                  expression(NPP[ua]),expression(P[frass]), 
                  expression(R[root]),expression(R[soil]))
    
    ### Prepare variable colors
    col.list <- hsv(seq(0,1 - 1/8,length.out = 8),0.8,1)
    
    ### make the bar plot
    p1 <- ggplot(plotDF,
                aes(cat, aCO2)) +
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        geom_segment(data=errDF, aes(x=cat, xend=cat, y=neg, yend=pos), 
                     colour="grey")+
        xlab("Method") + ylab(expression(paste(R[soil], " (kg C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("Litter+Rroot", "Rsoil"))+
        scale_fill_manual(name="Variables", 
                          values = col.list,
                          labels=var.labs) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="right",
              legend.text.align=0)
    
    plot(p1)
    
    pdf("R_other/rsoil_gap_aCO2.pdf")
    plot(p1)
    dev.off()
    
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
    errDF <- data.frame(rep(c("Litter+Rroot", "Rsoil"),2), NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg")
    errDF$pos[errDF$cat=="Litter+Rroot"] <- sum(plotDF$eCO2[plotDF$cat=="Litter+Rroot"])+sum(plotDF$eCO2_sd[plotDF$cat=="Litter+Rroot"])
    errDF$neg[errDF$cat=="Litter+Rroot"] <- sum(plotDF$eCO2[plotDF$cat=="Litter+Rroot"])-sum(plotDF$eCO2_sd[plotDF$cat=="Litter+Rroot"])
    errDF$pos[errDF$cat=="Rsoil"] <- sum(plotDF$eCO2[plotDF$cat=="Rsoil"])+sum(plotDF$eCO2_sd[plotDF$cat=="Rsoil"])
    errDF$neg[errDF$cat=="Rsoil"] <- sum(plotDF$eCO2[plotDF$cat=="Rsoil"])-sum(plotDF$eCO2_sd[plotDF$cat=="Rsoil"])
    
    ### make the bar plot
    p2 <- ggplot(plotDF,
                 aes(cat, eCO2)) +
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        geom_segment(data=errDF, aes(x=cat, xend=cat, y=neg, yend=pos), 
                     colour="grey")+
        xlab("Method") + ylab(expression(paste(R[soil], " (kg C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("Litter+Rroot", "Rsoil"))+
        scale_fill_manual(name="Variables", 
                          values = col.list,
                          labels=var.labs) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="right",
              legend.text.align=0)
    
    plot(p2)
    
    pdf("R_other/rsoil_gap_eCO2.pdf")
    plot(p2)
    dev.off()
    
}
