nep_gap_bootstrap_plot <- function(inDF) {
    
    ### subseting each method
    inoutDF <- as.data.frame(inDF$inout[,c("term", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")])
    nppDF <- as.data.frame(inDF$npp[,c("term", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")])
    deltaDF <- as.data.frame(inDF$delta_pool[,c("term", "aCO2", "eCO2", "aCO2_sd", "eCO2_sd")])

    ### prepare output df
    out <- data.frame(c("In-out", "NPP-Rh", "Pool"), NA, NA, NA, NA)
    colnames(out) <- c("Method", "aCO2", "eCO2", "aCO2_conf", "eCO2_conf")
    
    ### create dataframe to hold bootstrap results - inout
    bDF1 <- data.frame(c(1:1000), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "GPP_overstorey", "GPP_understorey", 
                        "CH4", "Ra_leaf", "Ra_stem", "Ra_und", "VOC",
                        "Rhb", "DOC", "Rsoil", "Rgrowth")
    
    ## set seed
    set.seed(99)
    
    ## ambient rings
    bDF1$GPP_overstorey <- rnorm(1000, mean=inoutDF$aCO2[inoutDF$term=="GPP overstorey"],
                                 sd=inoutDF$aCO2_sd[inoutDF$term=="GPP overstorey"])
    bDF1$GPP_understorey <- rnorm(1000, mean=inoutDF$aCO2[inoutDF$term=="GPP understorey"],
                                  sd=inoutDF$aCO2_sd[inoutDF$term=="GPP understorey"])
    bDF1$CH4 <- rnorm(1000, mean=inoutDF$aCO2[inoutDF$term=="CH4 efflux"],
                                  sd=inoutDF$aCO2_sd[inoutDF$term=="CH4 efflux"])
    bDF1$Ra_leaf <- rnorm(1000, mean=inoutDF$aCO2[inoutDF$term=="Ra leaf"],
                                  sd=inoutDF$aCO2_sd[inoutDF$term=="Ra leaf"])
    bDF1$Ra_stem <- rnorm(1000, mean=inoutDF$aCO2[inoutDF$term=="Ra stem"],
                                  sd=inoutDF$aCO2_sd[inoutDF$term=="Ra stem"])
    bDF1$Ra_und <- rnorm(1000, mean=inoutDF$aCO2[inoutDF$term=="Ra understorey"],
                                  sd=inoutDF$aCO2_sd[inoutDF$term=="Ra understorey"])
    bDF1$VOC <- rnorm(1000, mean=inoutDF$aCO2[inoutDF$term=="VOC"],
                      sd=inoutDF$aCO2_sd[inoutDF$term=="VOC"])
    bDF1$Rhb <- rnorm(1000, mean=inoutDF$aCO2[inoutDF$term=="Rherbivore"],
                                  sd=inoutDF$aCO2_sd[inoutDF$term=="Rherbivore"])
    bDF1$DOC <- rnorm(1000, mean=inoutDF$aCO2[inoutDF$term=="DOC loss"],
                                  sd=inoutDF$aCO2_sd[inoutDF$term=="DOC loss"])
    bDF1$Rsoil <- rnorm(1000, mean=inoutDF$aCO2[inoutDF$term=="Rsoil"],
                                  sd=inoutDF$aCO2_sd[inoutDF$term=="Rsoil"])
    bDF1$Rgrowth <- rnorm(1000, mean=inoutDF$aCO2[inoutDF$term=="Rgrowth"],
                                  sd=inoutDF$aCO2_sd[inoutDF$term=="Rgrowth"])
    
    bDF1$inout <- with(bDF1, (GPP_overstorey + GPP_understorey - CH4 - Ra_leaf - 
                                  Ra_stem - Ra_und - VOC - Rhb - DOC - Rsoil - Rgrowth))

    out$aCO2[out$Method=="In-out"] <- mean(bDF1$inout)
    out$aCO2_conf[out$Method=="In-out"] <- sd(bDF1$inout)#/sqrt(1000) * 1.96
    
    ## elevated rings
    bDF1$GPP_overstorey <- rnorm(1000, mean=inoutDF$eCO2[inoutDF$term=="GPP overstorey"],
                                 sd=inoutDF$eCO2_sd[inoutDF$term=="GPP overstorey"])
    bDF1$GPP_understorey <- rnorm(1000, mean=inoutDF$eCO2[inoutDF$term=="GPP understorey"],
                                  sd=inoutDF$eCO2_sd[inoutDF$term=="GPP understorey"])
    bDF1$CH4 <- rnorm(1000, mean=inoutDF$eCO2[inoutDF$term=="CH4 efflux"],
                      sd=inoutDF$eCO2_sd[inoutDF$term=="CH4 efflux"])
    bDF1$Ra_leaf <- rnorm(1000, mean=inoutDF$eCO2[inoutDF$term=="Ra leaf"],
                          sd=inoutDF$eCO2_sd[inoutDF$term=="Ra leaf"])
    bDF1$Ra_stem <- rnorm(1000, mean=inoutDF$eCO2[inoutDF$term=="Ra stem"],
                          sd=inoutDF$eCO2_sd[inoutDF$term=="Ra stem"])
    bDF1$Ra_und <- rnorm(1000, mean=inoutDF$eCO2[inoutDF$term=="Ra understorey"],
                         sd=inoutDF$eCO2_sd[inoutDF$term=="Ra understorey"])
    bDF1$VOC <- rnorm(1000, mean=inoutDF$eCO2[inoutDF$term=="VOC"],
                      sd=inoutDF$eCO2_sd[inoutDF$term=="VOC"])
    bDF1$Rhb <- rnorm(1000, mean=inoutDF$eCO2[inoutDF$term=="Rherbivore"],
                      sd=inoutDF$eCO2_sd[inoutDF$term=="Rherbivore"])
    bDF1$DOC <- rnorm(1000, mean=inoutDF$eCO2[inoutDF$term=="DOC loss"],
                      sd=inoutDF$eCO2_sd[inoutDF$term=="DOC loss"])
    bDF1$Rsoil <- rnorm(1000, mean=inoutDF$eCO2[inoutDF$term=="Rsoil"],
                        sd=inoutDF$eCO2_sd[inoutDF$term=="Rsoil"])
    bDF1$Rgrowth <- rnorm(1000, mean=inoutDF$eCO2[inoutDF$term=="Rgrowth"],
                          sd=inoutDF$eCO2_sd[inoutDF$term=="Rgrowth"])
    
    bDF1$inout <- with(bDF1, (GPP_overstorey + GPP_understorey - CH4 - Ra_leaf - 
                                  Ra_stem - Ra_und - VOC - Rhb - DOC - Rsoil - Rgrowth))
    
    out$eCO2[out$Method=="In-out"] <- mean(bDF1$inout)
    out$eCO2_conf[out$Method=="In-out"] <- sd(bDF1$inout)#/sqrt(1000) * 1.96
    
    ### create dataframe to hold bootstrap results - npp
    bDF1 <- data.frame(c(1:1000), NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "Leaf_NPP", "Stem_NPP", 
                        "Froot_NPP", "Croot_NPP", "Other_NPP", "Und_NPP", "Leaf_cons",
                        "Mycorrhizal_prod", "Rh")
    
    ## ambient rings
    bDF1$Leaf_NPP <- rnorm(1000, mean=nppDF$aCO2[nppDF$term=="Leaf NPP"],
                                 sd=nppDF$aCO2_sd[nppDF$term=="Leaf NPP"])
    bDF1$Stem_NPP <- rnorm(1000, mean=nppDF$aCO2[nppDF$term=="Stem NPP"],
                                  sd=nppDF$aCO2_sd[nppDF$term=="Stem NPP"])
    bDF1$Froot_NPP <- rnorm(1000, mean=nppDF$aCO2[nppDF$term=="Fine Root NPP"],
                      sd=nppDF$aCO2_sd[nppDF$term=="Fine Root NPP"])
    bDF1$Croot_NPP <- rnorm(1000, mean=nppDF$aCO2[nppDF$term=="Coarse Root NPP"],
                          sd=nppDF$aCO2_sd[nppDF$term=="Coarse Root NPP"])
    bDF1$Other_NPP <- rnorm(1000, mean=nppDF$aCO2[nppDF$term=="Other NPP"],
                          sd=nppDF$aCO2_sd[nppDF$term=="Other NPP"])
    bDF1$Und_NPP <- rnorm(1000, mean=nppDF$aCO2[nppDF$term=="Understorey NPP"],
                         sd=nppDF$aCO2_sd[nppDF$term=="Understorey NPP"])
    bDF1$Mycorrhizal_prod <- 0.0
    bDF1$Leaf_cons <- rnorm(1000, mean=nppDF$aCO2[nppDF$term=="Leaf consumption"],
                      sd=nppDF$aCO2_sd[nppDF$term=="Leaf consumption"])
    bDF1$Rh <- rnorm(1000, mean=nppDF$aCO2[nppDF$term=="R hetero"],
                      sd=nppDF$aCO2_sd[nppDF$term=="R hetero"])
    
    bDF1$tot <- with(bDF1, (Leaf_NPP + Stem_NPP + Froot_NPP + Croot_NPP + Other_NPP + Und_NPP + 
                                  Mycorrhizal_prod + Leaf_cons - Rh))
    
    out$aCO2[out$Method=="NPP-Rh"] <- mean(bDF1$tot)
    out$aCO2_conf[out$Method=="NPP-Rh"] <- sd(bDF1$tot)#/sqrt(1000) * 1.96
    
    ## elevated rings
    bDF1$Leaf_NPP <- rnorm(1000, mean=nppDF$eCO2[nppDF$term=="Leaf NPP"],
                           sd=nppDF$eCO2_sd[nppDF$term=="Leaf NPP"])
    bDF1$Stem_NPP <- rnorm(1000, mean=nppDF$eCO2[nppDF$term=="Stem NPP"],
                           sd=nppDF$eCO2_sd[nppDF$term=="Stem NPP"])
    bDF1$Froot_NPP <- rnorm(1000, mean=nppDF$eCO2[nppDF$term=="Fine Root NPP"],
                            sd=nppDF$eCO2_sd[nppDF$term=="Fine Root NPP"])
    bDF1$Croot_NPP <- rnorm(1000, mean=nppDF$eCO2[nppDF$term=="Coarse Root NPP"],
                            sd=nppDF$eCO2_sd[nppDF$term=="Coarse Root NPP"])
    bDF1$Other_NPP <- rnorm(1000, mean=nppDF$eCO2[nppDF$term=="Other NPP"],
                            sd=nppDF$eCO2_sd[nppDF$term=="Other NPP"])
    bDF1$Und_NPP <- rnorm(1000, mean=nppDF$eCO2[nppDF$term=="Understorey NPP"],
                          sd=nppDF$eCO2_sd[nppDF$term=="Understorey NPP"])
    bDF1$Mycorrhizal_prod <- 0.0
    bDF1$Leaf_cons <- rnorm(1000, mean=nppDF$eCO2[nppDF$term=="Leaf consumption"],
                            sd=nppDF$eCO2_sd[nppDF$term=="Leaf consumption"])
    bDF1$Rh <- rnorm(1000, mean=nppDF$eCO2[nppDF$term=="R hetero"],
                     sd=nppDF$eCO2_sd[nppDF$term=="R hetero"])
    
    bDF1$tot <- with(bDF1, (Leaf_NPP + Stem_NPP + Froot_NPP + Croot_NPP + Other_NPP + Und_NPP + 
                                Mycorrhizal_prod + Leaf_cons - Rh))
    
    out$eCO2[out$Method=="NPP-Rh"] <- mean(bDF1$tot)
    out$eCO2_conf[out$Method=="NPP-Rh"] <- sd(bDF1$tot)#/sqrt(1000) * 1.96
    
    
    
    ### create dataframe to hold bootstrap results - change in pools
    bDF1 <- data.frame(c(1:1000), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "soilc", "leafc", 
                        "woodc", "crootc", "frootc", "uac", "micc",
                        "mycc", "litter", "insect")
    
    ## ambient rings
    bDF1$soilc <- rnorm(1000, mean=deltaDF$aCO2[deltaDF$term=="Soil C"],
                                 sd=deltaDF$aCO2_sd[deltaDF$term=="Soil C"])
    bDF1$leafc <- rnorm(1000, mean=deltaDF$aCO2[deltaDF$term=="Overstorey leaf"],
                                  sd=deltaDF$aCO2_sd[deltaDF$term=="Overstorey leaf"])
    bDF1$woodc <- rnorm(1000, mean=deltaDF$aCO2[deltaDF$term=="Overstorey wood"],
                      sd=deltaDF$aCO2_sd[deltaDF$term=="Overstorey wood"])
    bDF1$crootc <- rnorm(1000, mean=deltaDF$aCO2[deltaDF$term=="Coarse Root"],
                          sd=deltaDF$aCO2_sd[deltaDF$term=="Coarse Root"])
    bDF1$frootc <- rnorm(1000, mean=deltaDF$aCO2[deltaDF$term=="Fine Root"],
                          sd=deltaDF$aCO2_sd[deltaDF$term=="Fine Root"])
    bDF1$uac <- rnorm(1000, mean=deltaDF$aCO2[deltaDF$term=="Understorey above-ground"],
                         sd=deltaDF$aCO2_sd[deltaDF$term=="Understorey above-ground"])
    bDF1$micc <- rnorm(1000, mean=deltaDF$aCO2[deltaDF$term=="Microbial biomass"],
                      sd=deltaDF$aCO2_sd[deltaDF$term=="Microbial biomass"])
    bDF1$mycc <- rnorm(1000, mean=deltaDF$aCO2[deltaDF$term=="Mycorrhizae"],
                      sd=deltaDF$aCO2_sd[deltaDF$term=="Mycorrhizae"])
    bDF1$litter <- rnorm(1000, mean=deltaDF$aCO2[deltaDF$term=="Litter"],
                        sd=deltaDF$aCO2_sd[deltaDF$term=="Litter"])
    bDF1$insect <- rnorm(1000, mean=deltaDF$aCO2[deltaDF$term=="Insects"],
                          sd=deltaDF$aCO2_sd[deltaDF$term=="Insects"])
    
    bDF1$tot <- with(bDF1, (soilc+leafc+woodc+uac+frootc+crootc+micc+mycc+litter+insect))
    
    out$aCO2[out$Method=="Pool"] <- mean(bDF1$tot)
    out$aCO2_conf[out$Method=="Pool"] <- sd(bDF1$tot)#/sqrt(1000) * 1.96
    
    ## elevated rings
    bDF1$soilc <- rnorm(1000, mean=deltaDF$eCO2[deltaDF$term=="Soil C"],
                        sd=deltaDF$eCO2_sd[deltaDF$term=="Soil C"])
    bDF1$leafc <- rnorm(1000, mean=deltaDF$eCO2[deltaDF$term=="Overstorey leaf"],
                        sd=deltaDF$eCO2_sd[deltaDF$term=="Overstorey leaf"])
    bDF1$woodc <- rnorm(1000, mean=deltaDF$eCO2[deltaDF$term=="Overstorey wood"],
                        sd=deltaDF$eCO2_sd[deltaDF$term=="Overstorey wood"])
    bDF1$crootc <- rnorm(1000, mean=deltaDF$eCO2[deltaDF$term=="Coarse Root"],
                         sd=deltaDF$eCO2_sd[deltaDF$term=="Coarse Root"])
    bDF1$frootc <- rnorm(1000, mean=deltaDF$eCO2[deltaDF$term=="Fine Root"],
                         sd=deltaDF$eCO2_sd[deltaDF$term=="Fine Root"])
    bDF1$uac <- rnorm(1000, mean=deltaDF$eCO2[deltaDF$term=="Understorey above-ground"],
                      sd=deltaDF$eCO2_sd[deltaDF$term=="Understorey above-ground"])
    bDF1$micc <- rnorm(1000, mean=deltaDF$eCO2[deltaDF$term=="Microbial biomass"],
                       sd=deltaDF$eCO2_sd[deltaDF$term=="Microbial biomass"])
    bDF1$mycc <- rnorm(1000, mean=deltaDF$eCO2[deltaDF$term=="Mycorrhizae"],
                       sd=deltaDF$eCO2_sd[deltaDF$term=="Mycorrhizae"])
    bDF1$litter <- rnorm(1000, mean=deltaDF$eCO2[deltaDF$term=="Litter"],
                         sd=deltaDF$eCO2_sd[deltaDF$term=="Litter"])
    bDF1$insect <- rnorm(1000, mean=deltaDF$eCO2[deltaDF$term=="Insects"],
                         sd=deltaDF$eCO2_sd[deltaDF$term=="Insects"])
    
    bDF1$tot <- with(bDF1, (soilc+leafc+woodc+uac+frootc+crootc+micc+mycc+litter+insect))
    
    out$eCO2[out$Method=="Pool"] <- mean(bDF1$tot)
    out$eCO2_conf[out$Method=="Pool"] <- sd(bDF1$tot)#/sqrt(1000) * 1.96
    
 
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
        theme(legend.justification=c(1,0), legend.position=c(0.9,0.05))
    
    #plot(p1)
    
    pdf("Output/nep_gap_bootstrapped.pdf", width=8, height=8)
    plot(p1)
    dev.off()
    
}