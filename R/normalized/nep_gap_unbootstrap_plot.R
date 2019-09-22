nep_gap_unbootstrap_plot <- function(inDF) {
    
    ### subseting each method
    inoutDF <- as.data.frame(inDF$inout[,c("term", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")])
    nppDF <- as.data.frame(inDF$npp[,c("term", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")])
    deltaDF <- as.data.frame(inDF$delta_pool[,c("term", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")])

    ### prepare output df
    out <- data.frame(c("In-out", "NPP-Rh", "Pool"), NA, NA, NA, NA)
    colnames(out) <- c("Method", "aCO2", "eCO2", "aCO2_conf", "eCO2_conf")
    

    out$aCO2[out$Method=="In-out"] <- inoutDF[inoutDF$term=="GPP overstorey", "aCO2"] + 
        inoutDF[inoutDF$term=="GPP understorey", "aCO2"] + abs(inoutDF[inoutDF$term=="CH4 efflux", "aCO2"]) -
        inoutDF[inoutDF$term=="Ra leaf", "aCO2"] - inoutDF[inoutDF$term=="Ra stem", "aCO2"] - 
        inoutDF[inoutDF$term=="Ra understorey", "aCO2"] - inoutDF[inoutDF$term=="Rgrowth", "aCO2"] -
        inoutDF[inoutDF$term=="VOC", "aCO2"] - inoutDF[inoutDF$term=="Rherbivore", "aCO2"]  -
        inoutDF[inoutDF$term=="DOC loss", "aCO2"] - inoutDF[inoutDF$term=="Rsoil", "aCO2"] 
    
    sd1 <- sqrt((inoutDF[inoutDF$term=="GPP overstorey", "aCO2_sd"]^2 + 
                     inoutDF[inoutDF$term=="GPP understorey", "aCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="CH4 efflux", "aCO2_sd"]^2)/3)
    
    sd2 <- sqrt((inoutDF[inoutDF$term=="Ra leaf", "aCO2_sd"]^2 + 
                     inoutDF[inoutDF$term=="Ra stem", "aCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="Ra root", "aCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="Ra understorey", "aCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="VOC", "aCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="Rherbivore", "aCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="DOC loss", "aCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="Rsoil", "aCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="Rgrowth", "aCO2_sd"]^2)/9)
    
    out$aCO2_conf[out$Method=="In-out"] <- sqrt(sd1^2/3 + sd2^2/3)
    
    ## elevated rings
    out$eCO2[out$Method=="In-out"] <- inoutDF[inoutDF$term=="GPP overstorey", "eCO2"] + 
        inoutDF[inoutDF$term=="GPP understorey", "eCO2"] + abs(inoutDF[inoutDF$term=="CH4 efflux", "eCO2"]) -
        inoutDF[inoutDF$term=="Ra leaf", "eCO2"] - inoutDF[inoutDF$term=="Ra stem", "eCO2"] - 
        inoutDF[inoutDF$term=="Ra understorey", "eCO2"] - inoutDF[inoutDF$term=="Rgrowth", "eCO2"] -
        inoutDF[inoutDF$term=="VOC", "eCO2"] - inoutDF[inoutDF$term=="Rherbivore", "eCO2"]  -
        inoutDF[inoutDF$term=="DOC loss", "eCO2"] - inoutDF[inoutDF$term=="Rsoil", "eCO2"] 
    
    sd1 <- sqrt((inoutDF[inoutDF$term=="GPP overstorey", "eCO2_sd"]^2 + 
                     inoutDF[inoutDF$term=="GPP understorey", "eCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="CH4 efflux", "eCO2_sd"]^2)/3)
    
    sd2 <- sqrt((inoutDF[inoutDF$term=="Ra leaf", "eCO2_sd"]^2 + 
                     inoutDF[inoutDF$term=="Ra stem", "eCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="Ra root", "eCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="Ra understorey", "eCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="VOC", "eCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="Rherbivore", "eCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="DOC loss", "eCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="Rsoil", "eCO2_sd"]^2 +
                     inoutDF[inoutDF$term=="Rgrowth", "eCO2_sd"]^2)/9)
    
    out$eCO2_conf[out$Method=="In-out"] <- sqrt(sd1^2/3 + sd2^2/3)
    
    ### create dataframe to hold bootstrap results - npp
    out$aCO2[out$Method=="NPP-Rh"] <- nppDF[nppDF$term=="Leaf NPP", "aCO2"] + 
        nppDF[nppDF$term=="Stem NPP", "aCO2"] + abs(nppDF[nppDF$term=="Fine Root NPP", "aCO2"]) +
        nppDF[nppDF$term=="Coarse Root NPP", "aCO2"] + nppDF[nppDF$term=="Other NPP", "aCO2"] + 
        nppDF[nppDF$term=="Understorey NPP", "aCO2"] + nppDF[nppDF$term=="Leaf consumption", "aCO2"] -
        nppDF[nppDF$term=="R hetero", "aCO2"] 
    
    sd1 <- sqrt((nppDF[nppDF$term=="Leaf NPP", "aCO2_sd"]^2 + 
                     nppDF[nppDF$term=="Stem NPP", "aCO2_sd"]^2 +
                     nppDF[nppDF$term=="Fine Root NPP", "aCO2_sd"]^2 + 
                     nppDF[nppDF$term=="Coarse Root NPP", "aCO2_sd"]^2 +
                     nppDF[nppDF$term=="Other NPP", "aCO2_sd"]^2 +
                     nppDF[nppDF$term=="Understorey NPP", "aCO2_sd"]^2 +
                     nppDF[nppDF$term=="Leaf consumption", "aCO2_sd"]^2)/7)
    
    sd2 <- nppDF[nppDF$term=="R hetero", "aCO2_sd"]
    
    out$aCO2_conf[out$Method=="NPP-Rh"] <- sqrt(sd1^2/3 + sd2^2/3)
    
    
    ## elevated rings
    out$eCO2[out$Method=="NPP-Rh"] <- nppDF[nppDF$term=="Leaf NPP", "eCO2"] + 
        nppDF[nppDF$term=="Stem NPP", "eCO2"] + abs(nppDF[nppDF$term=="Fine Root NPP", "eCO2"]) +
        nppDF[nppDF$term=="Coarse Root NPP", "eCO2"] + nppDF[nppDF$term=="Other NPP", "eCO2"] + 
        nppDF[nppDF$term=="Understorey NPP", "eCO2"] + nppDF[nppDF$term=="Leaf consumption", "eCO2"] -
        nppDF[nppDF$term=="R hetero", "eCO2"] 
    
    sd1 <- sqrt((nppDF[nppDF$term=="Leaf NPP", "eCO2_sd"]^2 + 
                     nppDF[nppDF$term=="Stem NPP", "eCO2_sd"]^2 +
                     nppDF[nppDF$term=="Fine Root NPP", "eCO2_sd"]^2 + 
                     nppDF[nppDF$term=="Coarse Root NPP", "eCO2_sd"]^2 +
                     nppDF[nppDF$term=="Other NPP", "eCO2_sd"]^2 +
                     nppDF[nppDF$term=="Understorey NPP", "eCO2_sd"]^2 +
                     nppDF[nppDF$term=="Leaf consumption", "eCO2_sd"]^2)/7)
    
    sd2 <- nppDF[nppDF$term=="R hetero", "eCO2_sd"]
    
    out$eCO2_conf[out$Method=="NPP-Rh"] <- sqrt(sd1^2/3 + sd2^2/3)
    
    
    ### create dataframe to hold bootstrap results - change in pools
    out$aCO2[out$Method=="Pool"] <- deltaDF[deltaDF$term=="Overstorey leaf", "aCO2"] + 
        deltaDF[deltaDF$term=="Overstorey wood", "aCO2"] + deltaDF[deltaDF$term=="Understorey above-ground", "aCO2"] +
        deltaDF[deltaDF$term=="Fine Root", "aCO2"] + deltaDF[deltaDF$term=="Coarse Root", "aCO2"] + 
        deltaDF[deltaDF$term=="Litter", "aCO2"] + deltaDF[deltaDF$term=="Microbial biomass", "aCO2"] +
        deltaDF[deltaDF$term=="Soil C", "aCO2"] + deltaDF[deltaDF$term=="Mycorrhizae", "aCO2"] +
        deltaDF[deltaDF$term=="Insects", "aCO2"] 

    
    out$aCO2_conf[out$Method=="Pool"] <- sqrt((deltaDF[deltaDF$term=="Overstorey leaf", "aCO2_sd"]^2 + 
                     deltaDF[deltaDF$term=="Overstorey wood", "aCO2_sd"]^2 +
                     deltaDF[deltaDF$term=="Understorey above-ground", "aCO2_sd"]^2 + 
                     deltaDF[deltaDF$term=="Fine Root", "aCO2_sd"]^2 +
                     deltaDF[deltaDF$term=="Coarse Root", "aCO2_sd"]^2 +
                     deltaDF[deltaDF$term=="Litter", "aCO2_sd"]^2 +
                     deltaDF[deltaDF$term=="Microbial biomass", "aCO2_sd"]^2 +
                     deltaDF[deltaDF$term=="Soil C", "aCO2_sd"]^2 +
                     deltaDF[deltaDF$term=="Mycorrhizae", "aCO2_sd"]^2 +
                     deltaDF[deltaDF$term=="Insects", "aCO2_sd"]^2)/7)
    
    
    ## elevated rings
    out$eCO2[out$Method=="Pool"] <- deltaDF[deltaDF$term=="Overstorey leaf", "eCO2"] + 
        deltaDF[deltaDF$term=="Overstorey wood", "eCO2"] + deltaDF[deltaDF$term=="Understorey above-ground", "eCO2"] +
        deltaDF[deltaDF$term=="Fine Root", "eCO2"] + deltaDF[deltaDF$term=="Coarse Root", "eCO2"] + 
        deltaDF[deltaDF$term=="Litter", "eCO2"] + deltaDF[deltaDF$term=="Microbial biomass", "eCO2"] +
        deltaDF[deltaDF$term=="Soil C", "eCO2"] + deltaDF[deltaDF$term=="Mycorrhizae", "eCO2"] +
        deltaDF[deltaDF$term=="Insects", "eCO2"] 
    
    
    out$eCO2_conf[out$Method=="Pool"] <- sqrt((deltaDF[deltaDF$term=="Overstorey leaf", "eCO2_sd"]^2 + 
                                                   deltaDF[deltaDF$term=="Overstorey wood", "eCO2_sd"]^2 +
                                                   deltaDF[deltaDF$term=="Understorey above-ground", "eCO2_sd"]^2 + 
                                                   deltaDF[deltaDF$term=="Fine Root", "eCO2_sd"]^2 +
                                                   deltaDF[deltaDF$term=="Coarse Root", "eCO2_sd"]^2 +
                                                   deltaDF[deltaDF$term=="Litter", "eCO2_sd"]^2 +
                                                   deltaDF[deltaDF$term=="Microbial biomass", "eCO2_sd"]^2 +
                                                   deltaDF[deltaDF$term=="Soil C", "eCO2_sd"]^2 +
                                                   deltaDF[deltaDF$term=="Mycorrhizae", "eCO2_sd"]^2 +
                                                   deltaDF[deltaDF$term=="Insects", "eCO2_sd"]^2)/7)
    
 
    write.csv(out, "R_other/NEP_bootstrapped_method_comparison.csv", row.names=F)
    
    
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
    
    plotDF$NEP_conf[plotDF$Method=="In-out" & plotDF$Trt=="aCO2"] <- out$aCO2_conf[out$Method=="In-out"]
    plotDF$NEP_conf[plotDF$Method=="NPP-Rh" & plotDF$Trt=="aCO2"] <- out$aCO2_conf[out$Method=="NPP-Rh"]
    plotDF$NEP_conf[plotDF$Method=="Pool" & plotDF$Trt=="aCO2"] <- out$aCO2_conf[out$Method=="Pool"]
    plotDF$NEP_conf[plotDF$Method=="In-out" & plotDF$Trt=="eCO2"] <- out$eCO2_conf[out$Method=="In-out"]
    plotDF$NEP_conf[plotDF$Method=="NPP-Rh" & plotDF$Trt=="eCO2"] <- out$eCO2_conf[out$Method=="NPP-Rh"]
    plotDF$NEP_conf[plotDF$Method=="Pool" & plotDF$Trt=="eCO2"] <- out$eCO2_conf[out$Method=="Pool"]
    
    plotDF$pos <- plotDF$NEP + plotDF$NEP_conf
    plotDF$neg <- plotDF$NEP - plotDF$NEP_conf
    
    write.csv(plotDF, "R_other/nep_bootstrapped_summary.csv", row.names=F)
    
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
    
    plot(p1)
    
    pdf("Output/nep_gap_bootstrapped.pdf", width=8, height=8)
    plot(p1)
    dev.off()
    
}