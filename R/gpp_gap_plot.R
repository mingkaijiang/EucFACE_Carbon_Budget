gpp_gap_plot <- function(inDF) {
    
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
    gpp.d <- summaryBy(GPP~Trt, data=overstorey_gpp_flux, FUN=mean, keep.names=T)
    gpp.sd <- summaryBy(GPP~Trt, data=overstorey_gpp_flux, FUN=sd, keep.names=T)
    
    temDF$aCO2[temDF$term == "GPP overstorey"] <- gpp.d$GPP[gpp.d$Trt=="aCO2"]
    temDF$aCO2_sd[temDF$term == "GPP overstorey"] <- gpp.sd$GPP[gpp.sd$Trt=="aCO2"]
    
    gpp.d <- summaryBy(GPP~Trt, data=understorey_gpp_flux, FUN=mean, keep.names=T)
    gpp.sd <- summaryBy(GPP~Trt, data=understorey_gpp_flux, FUN=sd, keep.names=T)
    
    temDF$aCO2[temDF$term == "GPP understorey"] <- gpp.d$GPP[gpp.d$Trt=="aCO2"]
    
    temDF$aCO2_sd[temDF$term == "GPP understorey"] <- gpp.sd$GPP[gpp.sd$Trt=="aCO2"]
    
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
    #plotDF$aCO2[plotDF$term=="Ra stem"] <- 0.0
    #plotDF$aCO2_sd[plotDF$term=="Ra stem"] <- 0.0
    
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
    var.labs <- c(expression(GPP[o]), expression(GPP[u]),
                  expression(NPP[leaf]), expression(NPP[wood]),
                  expression(NPP[froot]), expression(NPP[croot]),
                  expression(NPP[other]), expression(NPP[ua]),
                  expression(NPP[hb]), expression(R[leaf]),
                  expression(R[wood]), expression(R[root]),
                  expression(R[ua]), #expression(R[hb]),
                  expression(R[g]))
    
    ### Prepare variable colors
    col.list <- hsv(seq(0,1 - 1/14,length.out = 14),0.8,1)
    require(viridis)
    col.list <- viridis(14)
    
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
        xlab("Method") + ylab(expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("GPP", expression(paste("NPP+", R[a]))))+
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
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)
        
    plot(p1)
    
    pdf("R_other/gpp_gap_aCO2.pdf")
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
    p3 <- ggplot(subDF,
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
    
    plot(p3)
    
    pdf("R_other/npp_respiration_comparison_aCO2.pdf")
    plot(p3)
    dev.off()
    
    
    
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
    gpp.d <- summaryBy(GPP~Trt, data=overstorey_gpp_flux, FUN=mean, keep.names=T)
    gpp.sd <- summaryBy(GPP~Trt, data=overstorey_gpp_flux, FUN=sd, keep.names=T)
    
    temDF$eCO2[temDF$term == "GPP overstorey"] <- gpp.d$GPP[gpp.d$Trt=="eCO2"]
    temDF$eCO2_sd[temDF$term == "GPP overstorey"] <- gpp.sd$GPP[gpp.sd$Trt=="eCO2"]
    
    gpp.d <- summaryBy(GPP~Trt, data=understorey_gpp_flux, FUN=mean, keep.names=T)
    gpp.sd <- summaryBy(GPP~Trt, data=understorey_gpp_flux, FUN=sd, keep.names=T)
    
    temDF$eCO2[temDF$term == "GPP understorey"] <- gpp.d$GPP[gpp.d$Trt=="eCO2"]
    temDF$eCO2_sd[temDF$term == "GPP understorey"] <- gpp.sd$GPP[gpp.sd$Trt=="eCO2"]
    
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
    
    ### Mannually fill stem respiration
    #plotDF$eCO2[plotDF$term=="Ra stem"] <- 0.0
    #plotDF$eCO2_sd[plotDF$term=="Ra stem"] <- 0.0
    
    
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
        xlab("Method") + ylab(expression(paste("GPP (kg C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("GPP", expression(paste("NPP+", R[a]))))+
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
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0)
    
    plot(p2)
    
    pdf("R_other/gpp_gap_eCO2.pdf")
    plot(p2)
    dev.off()
    
    
}
