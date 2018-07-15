gpp_gap_plot <- function(inDF) {
    
    ### subseting NPP DF
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
    
    ### Mannually fill stem respiration
    plotDF$aCO2[plotDF$term=="Ra stem"] <- 0.0
    plotDF$aCO2_sd[plotDF$term=="Ra stem"] <- 0.0
    
    ### calculate missing C
    gpp <- sum(gppDF$aCO2, na.rm=T)
    npp <- sum(nppDF$aCO2, na.rm=T)
    ra <- sum(raDF$aCO2, na.rm=T)
    c.miss <- gpp - npp - ra
    
    ### prepare error bar ranges
    errDF <- data.frame(rep(c("NPP+Ra", "MAESPA"),2), NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg")
    errDF$pos[errDF$cat=="NPP+Ra"] <- sum(plotDF$aCO2[plotDF$cat=="NPP+Ra"])+sum(plotDF$aCO2_sd[plotDF$cat=="NPP+Ra"])
    errDF$neg[errDF$cat=="NPP+Ra"] <- sum(plotDF$aCO2[plotDF$cat=="NPP+Ra"])-sum(plotDF$aCO2_sd[plotDF$cat=="NPP+Ra"])
    errDF$pos[errDF$cat=="MAESPA"] <- sum(plotDF$aCO2[plotDF$cat=="MAESPA"])+sum(plotDF$aCO2_sd[plotDF$cat=="MAESPA"])
    errDF$neg[errDF$cat=="MAESPA"] <- sum(plotDF$aCO2[plotDF$cat=="MAESPA"])-sum(plotDF$aCO2_sd[plotDF$cat=="MAESPA"])
    
    ### Prepare variable labels
    var.labs <- c(expression(GPP[o]), expression(GPP[u]),
                  expression(NPP[leaf]), expression(NPP[wood]),
                  expression(NPP[froot]), expression(NPP[croot]),
                  expression(NPP[other]), expression(NPP[ua]),
                  expression(NPP[hb]), expression(R[leaf]),
                  expression(R[wood]), expression(R[root]),
                  expression(R[ua]), #expression(R[hb]),
                  expression(R[g]))
    
    ### Prepare variable colors
    col.list <- c("navy",        # GPP overstorey
                  "grey",        # GPP understorey
                  "green",       # NPP leaf
                  "darkgreen",   # NPP wood
                  "lightgreen",  # NPP fineroot
                  "#d2f53c",     # NPP coarseroot, lime
                  "#aaffc3",     # NPP other, mint
                  "beige",       # NPP ua
                  "#008080",     # NPP insect consumption, teal
                  "red",         # Rleaf
                  "yellow",      # Rwood
                  "brown",       # Rroot
                  "orange",      # Rua
                  #"pink",        # Rherb
                  "maroon")      # Rgrowth, yellow
    
    plotDF$term <- factor(plotDF$term, levels=unique(plotDF$term))
    
    
    ### make the bar plot
    p1 <- ggplot(plotDF,
                aes(cat, aCO2)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        geom_segment(data=errDF, aes(x=cat, xend=cat, y=neg, yend=pos), 
                     colour="red")+
        xlab("Method") + ylab(expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("Simulation", "Observation"))+
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
    
    pdf("R_other/gpp_gap.pdf")
    plot(p1)
    dev.off()
    
    
    
    ### Subset the DF to compare NPP and Respiration for each component organ
    subDF <- data.frame(rep(c("Leaf", "Wood", "Root", "UA"), each=2), NA, NA)
    colnames(subDF) <- c("Organ", "Flux", "Value")
    subDF$Flux <- rep(c("NPP", "R"), by=4)
    subDF$Value[subDF$Organ=="Leaf"&subDF$Flux=="NPP"] <- plotDF$aCO2[plotDF$term=="Leaf NPP"]+plotDF$aCO2[plotDF$term=="Leaf consumption"]
    subDF$Value[subDF$Organ=="Leaf"&subDF$Flux=="R"] <- plotDF$aCO2[plotDF$term=="Ra leaf"]
    subDF$Value[subDF$Organ=="Wood"&subDF$Flux=="NPP"] <- plotDF$aCO2[plotDF$term=="Stem NPP"]
    subDF$Value[subDF$Organ=="Wood"&subDF$Flux=="R"] <- plotDF$aCO2[plotDF$term=="Ra stem"]    
    subDF$Value[subDF$Organ=="Root"&subDF$Flux=="NPP"] <- plotDF$aCO2[plotDF$term=="Fine Root NPP"]+plotDF$aCO2[plotDF$term=="Coarse Root NPP"]
    subDF$Value[subDF$Organ=="Root"&subDF$Flux=="R"] <- plotDF$aCO2[plotDF$term=="Ra root"]
    subDF$Value[subDF$Organ=="UA"&subDF$Flux=="NPP"] <- plotDF$aCO2[plotDF$term=="Understorey NPP"]
    subDF$Value[subDF$Organ=="UA"&subDF$Flux=="R"] <- plotDF$aCO2[plotDF$term=="Ra understorey"]
    
    
    
    ### make the bar plot
    p2 <- ggplot(subDF,
                 aes(y=Value, x=Organ, fill=Flux)) +   
        geom_bar(stat = "identity",
                 position="dodge") +
        xlab("") + ylab(expression(paste("kg C ", m^-2, yr^-1))) +
        #scale_x_discrete(labels=c(expression(GPP[s]), expression(GPP[m])))+
        #scale_fill_manual(name="Variables", 
        #                  values = col.list,
        #                  labels=var.labs) +
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
    
    pdf("R_other/npp_respiration_comparison.pdf")
    plot(p2)
    dev.off()
    
    
}
