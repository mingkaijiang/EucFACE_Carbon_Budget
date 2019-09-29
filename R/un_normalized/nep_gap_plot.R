nep_gap_plot <- function(inDF) {
    ### subseting each method
    inoutDF <- as.data.frame(inDF$inout[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6")])
    nppDF <- as.data.frame(inDF$npp[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6")])
    deltaDF <- as.data.frame(inDF$delta_pool[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6")])
    
    ### prepare output df
    out <- data.frame(c("In-out", "NPP-Rh", "Pool"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("Method", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")
    
    
    out$R1[out$Method=="In-out"] <- inoutDF[inoutDF$term=="GPP overstorey", "Ring_1"] + 
        inoutDF[inoutDF$term=="GPP understorey", "Ring_1"] + abs(inoutDF[inoutDF$term=="CH4 efflux", "Ring_1"]) -
        inoutDF[inoutDF$term=="Ra leaf", "Ring_1"] - inoutDF[inoutDF$term=="Ra stem", "Ring_1"] - 
        inoutDF[inoutDF$term=="Ra understorey", "Ring_1"] - inoutDF[inoutDF$term=="Rgrowth", "Ring_1"] -
        inoutDF[inoutDF$term=="VOC", "Ring_1"] - inoutDF[inoutDF$term=="Rherbivore", "Ring_1"]  -
        inoutDF[inoutDF$term=="DOC loss", "Ring_1"] - inoutDF[inoutDF$term=="Rsoil", "Ring_1"] 
    
    out$R2[out$Method=="In-out"] <- inoutDF[inoutDF$term=="GPP overstorey", "Ring_2"] + 
        inoutDF[inoutDF$term=="GPP understorey", "Ring_2"] + abs(inoutDF[inoutDF$term=="CH4 efflux", "Ring_2"]) -
        inoutDF[inoutDF$term=="Ra leaf", "Ring_2"] - inoutDF[inoutDF$term=="Ra stem", "Ring_2"] - 
        inoutDF[inoutDF$term=="Ra understorey", "Ring_2"] - inoutDF[inoutDF$term=="Rgrowth", "Ring_2"] -
        inoutDF[inoutDF$term=="VOC", "Ring_2"] - inoutDF[inoutDF$term=="Rherbivore", "Ring_2"]  -
        inoutDF[inoutDF$term=="DOC loss", "Ring_2"] - inoutDF[inoutDF$term=="Rsoil", "Ring_2"] 
    
    out$R3[out$Method=="In-out"] <- inoutDF[inoutDF$term=="GPP overstorey", "Ring_3"] + 
        inoutDF[inoutDF$term=="GPP understorey", "Ring_3"] + abs(inoutDF[inoutDF$term=="CH4 efflux", "Ring_3"]) -
        inoutDF[inoutDF$term=="Ra leaf", "Ring_3"] - inoutDF[inoutDF$term=="Ra stem", "Ring_3"] - 
        inoutDF[inoutDF$term=="Ra understorey", "Ring_3"] - inoutDF[inoutDF$term=="Rgrowth", "Ring_3"] -
        inoutDF[inoutDF$term=="VOC", "Ring_3"] - inoutDF[inoutDF$term=="Rherbivore", "Ring_3"]  -
        inoutDF[inoutDF$term=="DOC loss", "Ring_3"] - inoutDF[inoutDF$term=="Rsoil", "Ring_3"] 
    
    out$R4[out$Method=="In-out"] <- inoutDF[inoutDF$term=="GPP overstorey", "Ring_4"] + 
        inoutDF[inoutDF$term=="GPP understorey", "Ring_4"] + abs(inoutDF[inoutDF$term=="CH4 efflux", "Ring_4"]) -
        inoutDF[inoutDF$term=="Ra leaf", "Ring_4"] - inoutDF[inoutDF$term=="Ra stem", "Ring_4"] - 
        inoutDF[inoutDF$term=="Ra understorey", "Ring_4"] - inoutDF[inoutDF$term=="Rgrowth", "Ring_4"] -
        inoutDF[inoutDF$term=="VOC", "Ring_4"] - inoutDF[inoutDF$term=="Rherbivore", "Ring_4"]  -
        inoutDF[inoutDF$term=="DOC loss", "Ring_4"] - inoutDF[inoutDF$term=="Rsoil", "Ring_4"] 
    
    out$R5[out$Method=="In-out"] <- inoutDF[inoutDF$term=="GPP overstorey", "Ring_5"] + 
        inoutDF[inoutDF$term=="GPP understorey", "Ring_5"] + abs(inoutDF[inoutDF$term=="CH4 efflux", "Ring_5"]) -
        inoutDF[inoutDF$term=="Ra leaf", "Ring_5"] - inoutDF[inoutDF$term=="Ra stem", "Ring_5"] - 
        inoutDF[inoutDF$term=="Ra understorey", "Ring_5"] - inoutDF[inoutDF$term=="Rgrowth", "Ring_5"] -
        inoutDF[inoutDF$term=="VOC", "Ring_5"] - inoutDF[inoutDF$term=="Rherbivore", "Ring_5"]  -
        inoutDF[inoutDF$term=="DOC loss", "Ring_5"] - inoutDF[inoutDF$term=="Rsoil", "Ring_5"] 
    
    out$R6[out$Method=="In-out"] <- inoutDF[inoutDF$term=="GPP overstorey", "Ring_6"] + 
        inoutDF[inoutDF$term=="GPP understorey", "Ring_6"] + abs(inoutDF[inoutDF$term=="CH4 efflux", "Ring_6"]) -
        inoutDF[inoutDF$term=="Ra leaf", "Ring_6"] - inoutDF[inoutDF$term=="Ra stem", "Ring_6"] - 
        inoutDF[inoutDF$term=="Ra understorey", "Ring_6"] - inoutDF[inoutDF$term=="Rgrowth", "Ring_6"] -
        inoutDF[inoutDF$term=="VOC", "Ring_6"] - inoutDF[inoutDF$term=="Rherbivore", "Ring_6"]  -
        inoutDF[inoutDF$term=="DOC loss", "Ring_6"] - inoutDF[inoutDF$term=="Rsoil", "Ring_6"] 
    
    
    ### create dataframe to hold bootstrap results - npp
    out$R1[out$Method=="NPP-Rh"] <- nppDF[nppDF$term=="Leaf NPP", "Ring_1"] + 
        nppDF[nppDF$term=="Stem NPP", "Ring_1"] + abs(nppDF[nppDF$term=="Fine Root NPP", "Ring_1"]) +
        nppDF[nppDF$term=="Coarse Root NPP", "Ring_1"] + nppDF[nppDF$term=="Bole Root NPP", "Ring_1"] +
        nppDF[nppDF$term=="Other NPP", "Ring_1"] + 
        nppDF[nppDF$term=="Understorey NPP", "Ring_1"] + nppDF[nppDF$term=="Leaf consumption", "Ring_1"] -
        nppDF[nppDF$term=="R hetero", "Ring_1"] 
    
    out$R2[out$Method=="NPP-Rh"] <- nppDF[nppDF$term=="Leaf NPP", "Ring_2"] + 
        nppDF[nppDF$term=="Stem NPP", "Ring_2"] + abs(nppDF[nppDF$term=="Fine Root NPP", "Ring_2"]) +
        nppDF[nppDF$term=="Coarse Root NPP", "Ring_2"] + nppDF[nppDF$term=="Bole Root NPP", "Ring_2"] +
        nppDF[nppDF$term=="Other NPP", "Ring_2"] + 
        nppDF[nppDF$term=="Understorey NPP", "Ring_2"] + nppDF[nppDF$term=="Leaf consumption", "Ring_2"] -
        nppDF[nppDF$term=="R hetero", "Ring_2"] 
    
    out$R3[out$Method=="NPP-Rh"] <- nppDF[nppDF$term=="Leaf NPP", "Ring_3"] + 
        nppDF[nppDF$term=="Stem NPP", "Ring_3"] + abs(nppDF[nppDF$term=="Fine Root NPP", "Ring_3"]) +
        nppDF[nppDF$term=="Coarse Root NPP", "Ring_3"] + nppDF[nppDF$term=="Bole Root NPP", "Ring_3"] +
        nppDF[nppDF$term=="Other NPP", "Ring_3"] + 
        nppDF[nppDF$term=="Understorey NPP", "Ring_3"] + nppDF[nppDF$term=="Leaf consumption", "Ring_3"] -
        nppDF[nppDF$term=="R hetero", "Ring_3"] 
    
    out$R4[out$Method=="NPP-Rh"] <- nppDF[nppDF$term=="Leaf NPP", "Ring_4"] + 
        nppDF[nppDF$term=="Stem NPP", "Ring_4"] + abs(nppDF[nppDF$term=="Fine Root NPP", "Ring_4"]) +
        nppDF[nppDF$term=="Coarse Root NPP", "Ring_4"] +nppDF[nppDF$term=="Bole Root NPP", "Ring_4"] +
        nppDF[nppDF$term=="Other NPP", "Ring_4"] + 
        nppDF[nppDF$term=="Understorey NPP", "Ring_4"] + nppDF[nppDF$term=="Leaf consumption", "Ring_4"] -
        nppDF[nppDF$term=="R hetero", "Ring_4"] 
    
    out$R5[out$Method=="NPP-Rh"] <- nppDF[nppDF$term=="Leaf NPP", "Ring_5"] + 
        nppDF[nppDF$term=="Stem NPP", "Ring_5"] + abs(nppDF[nppDF$term=="Fine Root NPP", "Ring_5"]) +
        nppDF[nppDF$term=="Coarse Root NPP", "Ring_5"] + nppDF[nppDF$term=="Bole Root NPP", "Ring_5"] +
        nppDF[nppDF$term=="Other NPP", "Ring_5"] + 
        nppDF[nppDF$term=="Understorey NPP", "Ring_5"] + nppDF[nppDF$term=="Leaf consumption", "Ring_5"] -
        nppDF[nppDF$term=="R hetero", "Ring_5"] 
    
    out$R6[out$Method=="NPP-Rh"] <- nppDF[nppDF$term=="Leaf NPP", "Ring_6"] + 
        nppDF[nppDF$term=="Stem NPP", "Ring_6"] + abs(nppDF[nppDF$term=="Fine Root NPP", "Ring_6"]) +
        nppDF[nppDF$term=="Coarse Root NPP", "Ring_6"] + nppDF[nppDF$term=="Bole Root NPP", "Ring_6"] +
        nppDF[nppDF$term=="Other NPP", "Ring_6"] + 
        nppDF[nppDF$term=="Understorey NPP", "Ring_6"] + nppDF[nppDF$term=="Leaf consumption", "Ring_6"] -
        nppDF[nppDF$term=="R hetero", "Ring_6"] 
    
    
    
    ### create dataframe to hold bootstrap results - change in pools
    out$R1[out$Method=="Pool"] <- deltaDF[deltaDF$term=="Overstorey leaf", "Ring_1"] + 
        deltaDF[deltaDF$term=="Overstorey wood", "Ring_1"] + deltaDF[deltaDF$term=="Understorey above-ground", "Ring_1"] +
        deltaDF[deltaDF$term=="Fine Root", "Ring_1"] + deltaDF[deltaDF$term=="Coarse Root", "Ring_1"] +
        deltaDF[deltaDF$term=="Bole Root", "Ring_1"] +
        deltaDF[deltaDF$term=="Litter", "Ring_1"] + deltaDF[deltaDF$term=="Microbial biomass", "Ring_1"] +
        deltaDF[deltaDF$term=="Soil C", "Ring_1"] + deltaDF[deltaDF$term=="Mycorrhizae", "Ring_1"] +
        deltaDF[deltaDF$term=="Insects", "Ring_1"] 
    
    out$R2[out$Method=="Pool"] <- deltaDF[deltaDF$term=="Overstorey leaf", "Ring_2"] + 
        deltaDF[deltaDF$term=="Overstorey wood", "Ring_2"] + deltaDF[deltaDF$term=="Understorey above-ground", "Ring_2"] +
        deltaDF[deltaDF$term=="Fine Root", "Ring_2"] + deltaDF[deltaDF$term=="Coarse Root", "Ring_2"] + 
        deltaDF[deltaDF$term=="Bole Root", "Ring_2"] +
        deltaDF[deltaDF$term=="Litter", "Ring_2"] + deltaDF[deltaDF$term=="Microbial biomass", "Ring_2"] +
        deltaDF[deltaDF$term=="Soil C", "Ring_2"] + deltaDF[deltaDF$term=="Mycorrhizae", "Ring_2"] +
        deltaDF[deltaDF$term=="Insects", "Ring_2"] 
    
    out$R3[out$Method=="Pool"] <- deltaDF[deltaDF$term=="Overstorey leaf", "Ring_3"] + 
        deltaDF[deltaDF$term=="Overstorey wood", "Ring_3"] + deltaDF[deltaDF$term=="Understorey above-ground", "Ring_3"] +
        deltaDF[deltaDF$term=="Fine Root", "Ring_3"] + deltaDF[deltaDF$term=="Coarse Root", "Ring_3"] + 
        deltaDF[deltaDF$term=="Bole Root", "Ring_3"] +
        deltaDF[deltaDF$term=="Litter", "Ring_3"] + deltaDF[deltaDF$term=="Microbial biomass", "Ring_3"] +
        deltaDF[deltaDF$term=="Soil C", "Ring_3"] + deltaDF[deltaDF$term=="Mycorrhizae", "Ring_3"] +
        deltaDF[deltaDF$term=="Insects", "Ring_3"] 
    
    out$R4[out$Method=="Pool"] <- deltaDF[deltaDF$term=="Overstorey leaf", "Ring_4"] + 
        deltaDF[deltaDF$term=="Overstorey wood", "Ring_4"] + deltaDF[deltaDF$term=="Understorey above-ground", "Ring_4"] +
        deltaDF[deltaDF$term=="Fine Root", "Ring_4"] + deltaDF[deltaDF$term=="Coarse Root", "Ring_4"] + 
        deltaDF[deltaDF$term=="Bole Root", "Ring_4"] +
        deltaDF[deltaDF$term=="Litter", "Ring_4"] + deltaDF[deltaDF$term=="Microbial biomass", "Ring_4"] +
        deltaDF[deltaDF$term=="Soil C", "Ring_4"] + deltaDF[deltaDF$term=="Mycorrhizae", "Ring_4"] +
        deltaDF[deltaDF$term=="Insects", "Ring_4"] 
    
    out$R5[out$Method=="Pool"] <- deltaDF[deltaDF$term=="Overstorey leaf", "Ring_5"] + 
        deltaDF[deltaDF$term=="Overstorey wood", "Ring_5"] + deltaDF[deltaDF$term=="Understorey above-ground", "Ring_5"] +
        deltaDF[deltaDF$term=="Fine Root", "Ring_5"] + deltaDF[deltaDF$term=="Coarse Root", "Ring_5"] + 
        deltaDF[deltaDF$term=="Bole Root", "Ring_5"] +
        deltaDF[deltaDF$term=="Litter", "Ring_5"] + deltaDF[deltaDF$term=="Microbial biomass", "Ring_5"] +
        deltaDF[deltaDF$term=="Soil C", "Ring_5"] + deltaDF[deltaDF$term=="Mycorrhizae", "Ring_5"] +
        deltaDF[deltaDF$term=="Insects", "Ring_5"] 
    
    out$R6[out$Method=="Pool"] <- deltaDF[deltaDF$term=="Overstorey leaf", "Ring_6"] + 
        deltaDF[deltaDF$term=="Overstorey wood", "Ring_6"] + deltaDF[deltaDF$term=="Understorey above-ground", "Ring_6"] +
        deltaDF[deltaDF$term=="Fine Root", "Ring_6"] + deltaDF[deltaDF$term=="Coarse Root", "Ring_6"] + 
        deltaDF[deltaDF$term=="Bole Root", "Ring_6"] +
        deltaDF[deltaDF$term=="Litter", "Ring_6"] + deltaDF[deltaDF$term=="Microbial biomass", "Ring_6"] +
        deltaDF[deltaDF$term=="Soil C", "Ring_6"] + deltaDF[deltaDF$term=="Mycorrhizae", "Ring_6"] +
        deltaDF[deltaDF$term=="Insects", "Ring_6"] 
    
    out$aCO2 <- rowMeans(subset(out, select=c(R2, R3, R6)), na.rm=T)
    out$eCO2 <- rowMeans(subset(out, select=c(R1, R4, R5)), na.rm=T)
    out$aCO2_sd <- rowSds(as.matrix(subset(out, select=c(R2, R3, R6)), na.rm=T))
    out$eCO2_sd <- rowSds(as.matrix(subset(out, select=c(R1, R4, R5)), na.rm=T))
    
    write.csv(out, "R_other/NEP_unnormalized_comparison.csv", row.names=F)
    
    
    ### prepare plotDF
    plotDF <- data.frame(rep(c("In-out", "NPP-Rh", "Pool"), 2), NA, NA, NA)
    colnames(plotDF) <- c("Method", "NEP", "NEP_conf", "Trt")
    plotDF$Trt <- rep(c("aCO2", "eCO2"), each=3)
    
    plotDF$NEP[plotDF$Method=="In-out" & plotDF$Trt=="aCO2"] <- out$aCO2[out$Method=="In-out"]
    plotDF$NEP[plotDF$Method=="NPP-Rh" & plotDF$Trt=="aCO2"] <- out$aCO2[out$Method=="NPP-Rh"]
    plotDF$NEP[plotDF$Method=="Pool" & plotDF$Trt=="aCO2"] <- out$aCO2[out$Method=="Pool"]
    plotDF$NEP[plotDF$Method=="In-out" & plotDF$Trt=="eCO2"] <- out$eCO2[out$Method=="In-out"]
    plotDF$NEP[plotDF$Method=="NPP-Rh" & plotDF$Trt=="eCO2"] <- out$eCO2[out$Method=="NPP-Rh"]
    plotDF$NEP[plotDF$Method=="Pool" & plotDF$Trt=="eCO2"] <- out$eCO2[out$Method=="Pool"]
    
    plotDF$NEP_conf[plotDF$Method=="In-out" & plotDF$Trt=="aCO2"] <- out$aCO2_sd[out$Method=="In-out"]
    plotDF$NEP_conf[plotDF$Method=="NPP-Rh" & plotDF$Trt=="aCO2"] <- out$aCO2_sd[out$Method=="NPP-Rh"]
    plotDF$NEP_conf[plotDF$Method=="Pool" & plotDF$Trt=="aCO2"] <- out$aCO2_sd[out$Method=="Pool"]
    plotDF$NEP_conf[plotDF$Method=="In-out" & plotDF$Trt=="eCO2"] <- out$eCO2_sd[out$Method=="In-out"]
    plotDF$NEP_conf[plotDF$Method=="NPP-Rh" & plotDF$Trt=="eCO2"] <- out$eCO2_sd[out$Method=="NPP-Rh"]
    plotDF$NEP_conf[plotDF$Method=="Pool" & plotDF$Trt=="eCO2"] <- out$eCO2_sd[out$Method=="Pool"]
    
    plotDF$pos <- plotDF$NEP + plotDF$NEP_conf
    plotDF$neg <- plotDF$NEP - plotDF$NEP_conf
    
    write.csv(plotDF, "R_other/nep_unnormalized_summary.csv", row.names=F)
    
    ### make the bar plot
    p1 <- ggplot(plotDF,
                 aes(Method, NEP)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        #geom_point(mapping=aes(x=Method, y=NEP, fill=Trt), 
        #           size=4, shape=21,position = position_dodge(0.9))+
        xlab("Method") + ylab(expression(paste("NEP (g C ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="bottom")+
        geom_hline(yintercept = 0, linetype="dashed", color="black")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete("",  
                         labels=c("In - Out",
                                  expression(paste("NPP - ", R[h])),
                                  expression(Delta*C[pools])))+
        theme(legend.justification=c(1,0), legend.position=c(0.2,0.05))+
        scale_y_continuous(limits=c(-500, 500), 
                           breaks=c(-500, -250, -100, 0, 100, 250, 500),
                           labels=c(-500, -250, -100, 0, 100, 250, 500))
    
    
    
    #plot(p1)
    
    pdf("Output/nep_unnormalized_plot.pdf", width=8, height=8)
    plot(p1)
    dev.off()
    
    
}