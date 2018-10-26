gpp_and_rsoil_gap_bootstrap_plot <- function(inDF) {
    
    ### subseting NPP DF
    ### aCO2
    temDF <- inDF$npp[,c("term", "aCO2", "aCO2_sd")]
    
    ### only include plant NPP + mycorrhizal production
    nppDF <- rbind(temDF[temDF$term == "Leaf NPP",],temDF[temDF$term == "Stem NPP",],
                   temDF[temDF$term == "Fine Root NPP",],temDF[temDF$term == "Coarse Root NPP",],
                   temDF[temDF$term == "Other NPP",],temDF[temDF$term == "Understorey NPP",],
                   temDF[temDF$term == "Leaf consumption",])#, temDF[temDF$term == "Mycorrhizal production",])
    
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
    
    
    ### set number of bootstrapping
    n.b <- 1000
    
    ### perform bootstrapping for each category sum (i.e. cat == MAESPA and NPP+Ra)
    set.seed(1234)
    bDF1 <- data.frame(c(1:n.b), NA, NA, NA)
    colnames(bDF1) <- c("bootID", "GPP_overstorey", "GPP_understorey", "sum")
    bDF1$GPP_overstorey <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="GPP overstorey"],
                                 sd=plotDF$aCO2_sd[plotDF$term=="GPP overstorey"])
    bDF1$GPP_understorey <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="GPP understorey"],
                                  sd=plotDF$aCO2_sd[plotDF$term=="GPP understorey"])
    bDF1$sum <- with(bDF1, GPP_overstorey + GPP_understorey)
    
    bDF2 <- data.frame(c(1:n.b), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,NA)
    colnames(bDF2) <- c("bootID", "Leaf_NPP", "Stem_NPP", "Fine_Root_NPP", "Coarse_Root_NPP", 
                        "Other_NPP", "Understorey_NPP", "Leaf_consumption", "Ra_leaf", 
                        "Ra_stem", "Ra_root", "Ra_understorey", "Rgrowth", "sum")
    bDF2$Leaf_NPP <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Leaf NPP"],
                                 sd=plotDF$aCO2_sd[plotDF$term=="Leaf NPP"])
    bDF2$Stem_NPP <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Stem NPP"],
                                  sd=plotDF$aCO2_sd[plotDF$term=="Stem NPP"])
    bDF2$Fine_Root_NPP <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Fine Root NPP"],
                           sd=plotDF$aCO2_sd[plotDF$term=="Fine Root NPP"])
    bDF2$Coarse_Root_NPP <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Coarse Root NPP"],
                           sd=plotDF$aCO2_sd[plotDF$term=="Coarse Root NPP"])
    bDF2$Other_NPP <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Other NPP"],
                           sd=plotDF$aCO2_sd[plotDF$term=="Other NPP"])
    bDF2$Understorey_NPP <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Understorey NPP"],
                           sd=plotDF$aCO2_sd[plotDF$term=="Understorey NPP"])
    bDF2$Leaf_consumption <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Leaf consumption"],
                           sd=plotDF$aCO2_sd[plotDF$term=="Leaf consumption"])
    bDF2$Ra_leaf <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Ra leaf"],
                           sd=plotDF$aCO2_sd[plotDF$term=="Ra leaf"])
    bDF2$Ra_stem <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Ra stem"],
                           sd=plotDF$aCO2_sd[plotDF$term=="Ra stem"])
    bDF2$Ra_root <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Ra root"],
                           sd=plotDF$aCO2_sd[plotDF$term=="Ra root"])
    bDF2$Ra_understorey <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Ra understorey"],
                           sd=plotDF$aCO2_sd[plotDF$term=="Ra understorey"])
    bDF2$Rgrowth <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Rgrowth"],
                           sd=plotDF$aCO2_sd[plotDF$term=="Rgrowth"])

    
    bDF2$sum <- rowSums(bDF2[,2:13])
    
    
    ### prepare error bar ranges
    errDF <- data.frame(c("NPP+Ra", "MAESPA"), NA, NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg", "sum")
    errDF$sum[errDF$cat=="NPP+Ra"] <- sum(plotDF$aCO2[plotDF$cat=="NPP+Ra"])
    errDF$sum[errDF$cat=="MAESPA"] <- sum(plotDF$aCO2[plotDF$cat=="MAESPA"])
    
    errDF$sd[errDF$cat=="NPP+Ra"] <- sd(bDF2$sum)
    errDF$sd[errDF$cat=="MAESPA"] <- sd(bDF1$sum)
    
    #errDF$se[errDF$cat=="NPP+Ra"] <- se(bDF2$sum)
    #errDF$se[errDF$cat=="MAESPA"] <- se(bDF1$sum)
    
    #errDF$conf[errDF$cat=="NPP+Ra"] <- qt(0.95/2 + .5, length(bDF2$sum)-1) * errDF$se[errDF$cat=="NPP+Ra"]
    #errDF$conf[errDF$cat=="MAESPA"] <- qt(0.95/2 + .5, length(bDF1$sum)-1) * errDF$se[errDF$cat=="MAESPA"]
    
    errDF$conf[errDF$cat=="NPP+Ra"] <- errDF$sd[errDF$cat=="NPP+Ra"]
    errDF$conf[errDF$cat=="MAESPA"] <- errDF$sd[errDF$cat=="MAESPA"]
    
    errDF$pos[errDF$cat=="NPP+Ra"] <- errDF$sum[errDF$cat=="NPP+Ra"] + errDF$conf[errDF$cat=="NPP+Ra"] 
    errDF$neg[errDF$cat=="NPP+Ra"] <- errDF$sum[errDF$cat=="NPP+Ra"] - errDF$conf[errDF$cat=="NPP+Ra"] 
    errDF$pos[errDF$cat=="MAESPA"] <- errDF$sum[errDF$cat=="MAESPA"] + errDF$conf[errDF$cat=="MAESPA"] 
    errDF$neg[errDF$cat=="MAESPA"] <- errDF$sum[errDF$cat=="MAESPA"] - errDF$conf[errDF$cat=="MAESPA"] 
    
    ### Prepare variable labels
    var.labs1 <- c(expression(GPP[o]), expression(GPP[u]),
                  expression(NPP[leaf]), expression(NPP[wood]),
                  expression(NPP[froot]), expression(NPP[croot]),
                  expression(NPP[other]), expression(NPP[ua]),
                  expression(NPP[hb]), #expression(NPP[myc]), 
                  expression(R[leaf]),
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
                   temDF[temDF$term == "Leaf consumption",])#, temDF[temDF$term == "Mycorrhizal production",])
    
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
    
    bDF1 <- data.frame(c(1:n.b), NA, NA, NA)
    colnames(bDF1) <- c("bootID", "GPP_overstorey", "GPP_understorey", "sum")
    bDF1$GPP_overstorey <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="GPP overstorey"],
                                 sd=plotDF$eCO2_sd[plotDF$term=="GPP overstorey"])
    bDF1$GPP_understorey <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="GPP understorey"],
                                  sd=plotDF$eCO2_sd[plotDF$term=="GPP understorey"])
    bDF1$sum <- with(bDF1, GPP_overstorey + GPP_understorey)
    
    bDF2 <- data.frame(c(1:n.b), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,NA)
    colnames(bDF2) <- c("bootID", "Leaf_NPP", "Stem_NPP", "Fine_Root_NPP", "Coarse_Root_NPP", 
                        "Other_NPP", "Understorey_NPP", "Leaf_consumption", "Ra_leaf", 
                        "Ra_stem", "Ra_root", "Ra_understorey", "Rgrowth", "sum")
    bDF2$Leaf_NPP <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Leaf NPP"],
                           sd=plotDF$eCO2_sd[plotDF$term=="Leaf NPP"])
    bDF2$Stem_NPP <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Stem NPP"],
                           sd=plotDF$eCO2_sd[plotDF$term=="Stem NPP"])
    bDF2$Fine_Root_NPP <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Fine Root NPP"],
                                sd=plotDF$eCO2_sd[plotDF$term=="Fine Root NPP"])
    bDF2$Coarse_Root_NPP <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Coarse Root NPP"],
                                  sd=plotDF$eCO2_sd[plotDF$term=="Coarse Root NPP"])
    bDF2$Other_NPP <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Other NPP"],
                            sd=plotDF$eCO2_sd[plotDF$term=="Other NPP"])
    bDF2$Understorey_NPP <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Understorey NPP"],
                                  sd=plotDF$eCO2_sd[plotDF$term=="Understorey NPP"])
    bDF2$Leaf_consumption <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Leaf consumption"],
                                   sd=plotDF$eCO2_sd[plotDF$term=="Leaf consumption"])
    bDF2$Ra_leaf <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Ra leaf"],
                          sd=plotDF$eCO2_sd[plotDF$term=="Ra leaf"])
    bDF2$Ra_stem <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Ra stem"],
                          sd=plotDF$eCO2_sd[plotDF$term=="Ra stem"])
    bDF2$Ra_root <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Ra root"],
                          sd=plotDF$eCO2_sd[plotDF$term=="Ra root"])
    bDF2$Ra_understorey <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Ra understorey"],
                                 sd=plotDF$eCO2_sd[plotDF$term=="Ra understorey"])
    bDF2$Rgrowth <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Rgrowth"],
                          sd=plotDF$eCO2_sd[plotDF$term=="Rgrowth"])
    
    
    bDF2$sum <- rowSums(bDF2[,2:13])
    
    
    ### prepare error bar ranges
    errDF <- data.frame(c("NPP+Ra", "MAESPA"), NA, NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg", "sum")
    errDF$sum[errDF$cat=="NPP+Ra"] <- sum(plotDF$eCO2[plotDF$cat=="NPP+Ra"])
    errDF$sum[errDF$cat=="MAESPA"] <- sum(plotDF$eCO2[plotDF$cat=="MAESPA"])
    
    errDF$sd[errDF$cat=="NPP+Ra"] <- sd(bDF2$sum)
    errDF$sd[errDF$cat=="MAESPA"] <- sd(bDF1$sum)
    
    #errDF$se[errDF$cat=="NPP+Ra"] <- se(bDF2$sum)
    #errDF$se[errDF$cat=="MAESPA"] <- se(bDF1$sum)
    
    #errDF$conf[errDF$cat=="NPP+Ra"] <- qt(0.95/2 + .5, length(bDF2$sum)-1) * errDF$se[errDF$cat=="NPP+Ra"]
    #errDF$conf[errDF$cat=="MAESPA"] <- qt(0.95/2 + .5, length(bDF1$sum)-1) * errDF$se[errDF$cat=="MAESPA"]
    
    errDF$conf[errDF$cat=="NPP+Ra"] <- errDF$sd[errDF$cat=="NPP+Ra"]
    errDF$conf[errDF$cat=="MAESPA"] <- errDF$sd[errDF$cat=="MAESPA"]
    
    errDF$pos[errDF$cat=="NPP+Ra"] <- errDF$sum[errDF$cat=="NPP+Ra"] + errDF$conf[errDF$cat=="NPP+Ra"] 
    errDF$neg[errDF$cat=="NPP+Ra"] <- errDF$sum[errDF$cat=="NPP+Ra"] - errDF$conf[errDF$cat=="NPP+Ra"] 
    errDF$pos[errDF$cat=="MAESPA"] <- errDF$sum[errDF$cat=="MAESPA"] + errDF$conf[errDF$cat=="MAESPA"] 
    errDF$neg[errDF$cat=="MAESPA"] <- errDF$sum[errDF$cat=="MAESPA"] - errDF$conf[errDF$cat=="MAESPA"] 
    
    
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
                   temDF[temDF$term == "Frass production",])#, temDF[temDF$term == "Mycorrhizal production",])
    aDF <- inDF$inout[, c("term", "aCO2", "aCO2_sd")]
    nppDF <- rbind(nppDF, aDF[aDF$term == "Ra root",])
    
    ### Add change in soil pool into NPP df
    temDF <- inDF$delta_pool
    asoil <- temDF[temDF$term=="Soil C", c("term", "aCO2", "aCO2_sd")]
    
    ### subsetting inout df
    temDF <- inDF$inout[,c("term", "aCO2", "aCO2_sd")]
    
    ### only include Rsoil
    rsoilDF <- temDF[temDF$term == "Rsoil",]
    
    nppDF$cat <- "Litter+Rroot"
    rsoilDF$cat <- "Rsoil"
    
    plotDF <- rbind(nppDF, rsoilDF)
    plotDF$term <- factor(plotDF$term, levels=unique(plotDF$term))
    
    ### convert unit from g C to kg C
    plotDF$aCO2 <- as.numeric(plotDF$aCO2)/1000
    plotDF$aCO2_sd <- as.numeric(plotDF$aCO2_sd)/1000
    
    ### prepare bootstrap
    bDF1 <- data.frame(c(1:n.b), NA)
    colnames(bDF1) <- c("bootID", "Rsoil")
    bDF1$Rsoil <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Rsoil"],
                                 sd=plotDF$aCO2_sd[plotDF$term=="Rsoil"])

    
    bDF2 <- data.frame(c(1:n.b), NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF2) <- c("bootID", "Leaf_NPP", "Fine_Root_NPP", "Coarse_Root_NPP", 
                        "Other_NPP", "Understorey_NPP", "Frass_production", "Ra_root", "sum")
    bDF2$Leaf_NPP <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Leaf NPP"],
                           sd=plotDF$aCO2_sd[plotDF$term=="Leaf NPP"])
    bDF2$Fine_Root_NPP <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Fine Root NPP"],
                                sd=plotDF$aCO2_sd[plotDF$term=="Fine Root NPP"])
    bDF2$Coarse_Root_NPP <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Coarse Root NPP"],
                                  sd=plotDF$aCO2_sd[plotDF$term=="Coarse Root NPP"])
    bDF2$Other_NPP <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Other NPP"],
                            sd=plotDF$aCO2_sd[plotDF$term=="Other NPP"])
    bDF2$Understorey_NPP <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Understorey NPP"],
                                  sd=plotDF$aCO2_sd[plotDF$term=="Understorey NPP"])
    bDF2$Frass_production <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Frass production"],
                                   sd=plotDF$aCO2_sd[plotDF$term=="Frass production"])
    bDF2$Ra_root <- rnorm(n.b, mean=plotDF$aCO2[plotDF$term=="Ra root"],
                          sd=plotDF$aCO2_sd[plotDF$term=="Ra root"])
    
    bDF2$sum <- rowSums(bDF2[,2:8])
    
    
    
    ### prepare error bar ranges
    errDF <- data.frame(c("Litter+Rroot", "Rsoil"), NA, NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg", "sum")
    errDF$sum[errDF$cat=="Litter+Rroot"] <- sum(plotDF$aCO2[plotDF$cat=="Litter+Rroot"])
    errDF$sum[errDF$cat=="Rsoil"] <- sum(plotDF$aCO2[plotDF$cat=="Rsoil"])
    
    errDF$sd[errDF$cat=="Litter+Rroot"] <- sd(bDF2$sum)
    errDF$sd[errDF$cat=="Rsoil"] <- sd(bDF1$Rsoil)
    
    #errDF$se[errDF$cat=="Litter+Rroot"] <- se(bDF2$sum)
    #errDF$se[errDF$cat=="Rsoil"] <- se(bDF1$Rsoil)
    
    #errDF$conf[errDF$cat=="Litter+Rroot"] <- qt(0.95/2 + .5, length(bDF2$sum)-1) * errDF$se[errDF$cat=="Litter+Rroot"]
    #errDF$conf[errDF$cat=="Rsoil"] <- qt(0.95/2 + .5, length(bDF1$Rsoil)-1) * errDF$se[errDF$cat=="Rsoil"]
    
    errDF$conf[errDF$cat=="Litter+Rroot"] <- errDF$sd[errDF$cat=="Litter+Rroot"]
    errDF$conf[errDF$cat=="Rsoil"] <- errDF$sd[errDF$cat=="Rsoil"]
    
    errDF$pos[errDF$cat=="Litter+Rroot"] <- errDF$sum[errDF$cat=="Litter+Rroot"] + errDF$conf[errDF$cat=="Litter+Rroot"] 
    errDF$neg[errDF$cat=="Litter+Rroot"] <- errDF$sum[errDF$cat=="Litter+Rroot"] - errDF$conf[errDF$cat=="Litter+Rroot"] 
    errDF$pos[errDF$cat=="Rsoil"] <- errDF$sum[errDF$cat=="Rsoil"] + errDF$conf[errDF$cat=="Rsoil"] 
    errDF$neg[errDF$cat=="Rsoil"] <- errDF$sum[errDF$cat=="Rsoil"] - errDF$conf[errDF$cat=="Rsoil"]
    
    
    ### Prepare variable labels
    var.labs2 <- c(expression(NPP[leaf]), expression(NPP[froot]),
                  expression(NPP[croot]),expression(NPP[other]), 
                  expression(NPP[ua]),expression(P[frass]), #expression(NPP[myc]), 
                  expression(R[root]),#expression(Delta*C[soil]), 
                  expression(R[soil]))
    
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
        ylim(0, 1.6)
    
    ### eCO2
    ### subseting NPP DF
    temDF <- inDF$npp[,c("term", "eCO2", "eCO2_sd")]
    
    ### only include plant NPP
    nppDF <- rbind(temDF[temDF$term == "Leaf NPP",],
                   temDF[temDF$term == "Fine Root NPP",],temDF[temDF$term == "Coarse Root NPP",],
                   temDF[temDF$term == "Other NPP",],temDF[temDF$term == "Understorey NPP",],
                   temDF[temDF$term == "Frass production",])#, temDF[temDF$term == "Mycorrhizal production",])
    aDF <- inDF$inout[, c("term", "eCO2", "eCO2_sd")]
    nppDF <- rbind(nppDF, aDF[aDF$term == "Ra root",])
    
    ### Add change in soil pool into NPP df
    temDF <- inDF$delta_pool
    esoil <- temDF[temDF$term=="Soil C", c("term", "eCO2", "eCO2_sd")]
    
    ### subsetting inout df
    temDF <- inDF$inout[,c("term", "eCO2", "eCO2_sd")]
    
    ### only include Rsoil
    rsoilDF <- temDF[temDF$term == "Rsoil",]
    
    nppDF$cat <- "Litter+Rroot"
    rsoilDF$cat <- "Rsoil"
    
    plotDF <- rbind(nppDF, rsoilDF)
    plotDF$term <- factor(plotDF$term, levels=unique(plotDF$term))
    
    ### convert unit from g C to kg C
    plotDF$eCO2 <- as.numeric(plotDF$eCO2)/1000
    plotDF$eCO2_sd <- as.numeric(plotDF$eCO2_sd)/1000
    
    ### prepare bootstrap
    bDF1 <- data.frame(c(1:n.b), NA)
    colnames(bDF1) <- c("bootID", "Rsoil")
    bDF1$Rsoil <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Rsoil"],
                        sd=plotDF$eCO2_sd[plotDF$term=="Rsoil"])
    
    
    bDF2 <- data.frame(c(1:n.b), NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF2) <- c("bootID", "Leaf_NPP", "Fine_Root_NPP", "Coarse_Root_NPP", 
                        "Other_NPP", "Understorey_NPP", "Frass_production", "Ra_root", "sum")
    bDF2$Leaf_NPP <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Leaf NPP"],
                           sd=plotDF$eCO2_sd[plotDF$term=="Leaf NPP"])
    bDF2$Fine_Root_NPP <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Fine Root NPP"],
                                sd=plotDF$eCO2_sd[plotDF$term=="Fine Root NPP"])
    bDF2$Coarse_Root_NPP <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Coarse Root NPP"],
                                  sd=plotDF$eCO2_sd[plotDF$term=="Coarse Root NPP"])
    bDF2$Other_NPP <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Other NPP"],
                            sd=plotDF$eCO2_sd[plotDF$term=="Other NPP"])
    bDF2$Understorey_NPP <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Understorey NPP"],
                                  sd=plotDF$eCO2_sd[plotDF$term=="Understorey NPP"])
    bDF2$Frass_production <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Frass production"],
                                   sd=plotDF$eCO2_sd[plotDF$term=="Frass production"])
    bDF2$Ra_root <- rnorm(n.b, mean=plotDF$eCO2[plotDF$term=="Ra root"],
                          sd=plotDF$eCO2_sd[plotDF$term=="Ra root"])
    
    bDF2$sum <- rowSums(bDF2[,2:8])
    
    
    ### prepare error bar ranges
    ### prepare error bar ranges
    errDF <- data.frame(c("Litter+Rroot", "Rsoil"), NA, NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg", "sum")
    errDF$sum[errDF$cat=="Litter+Rroot"] <- sum(plotDF$eCO2[plotDF$cat=="Litter+Rroot"])
    errDF$sum[errDF$cat=="Rsoil"] <- sum(plotDF$eCO2[plotDF$cat=="Rsoil"])
    
    #errDF$se[errDF$cat=="Litter+Rroot"] <- se(bDF2$sum)
    #errDF$se[errDF$cat=="Rsoil"] <- se(bDF1$Rsoil)
    
    errDF$sd[errDF$cat=="Litter+Rroot"] <- sd(bDF2$sum)
    errDF$sd[errDF$cat=="Rsoil"] <- sd(bDF1$Rsoil)
    
    #errDF$conf[errDF$cat=="Litter+Rroot"] <- qt(0.95/2 + .5, length(bDF2$sum)-1) * errDF$se[errDF$cat=="Litter+Rroot"]
    #errDF$conf[errDF$cat=="Rsoil"] <- qt(0.95/2 + .5, length(bDF1$Rsoil)-1) * errDF$se[errDF$cat=="Rsoil"]
    
    errDF$conf[errDF$cat=="Litter+Rroot"] <- errDF$sd[errDF$cat=="Litter+Rroot"]
    errDF$conf[errDF$cat=="Rsoil"] <- errDF$sd[errDF$cat=="Rsoil"]
    
    errDF$pos[errDF$cat=="Litter+Rroot"] <- errDF$sum[errDF$cat=="Litter+Rroot"] + errDF$conf[errDF$cat=="Litter+Rroot"] 
    errDF$neg[errDF$cat=="Litter+Rroot"] <- errDF$sum[errDF$cat=="Litter+Rroot"] - errDF$conf[errDF$cat=="Litter+Rroot"] 
    errDF$pos[errDF$cat=="Rsoil"] <- errDF$sum[errDF$cat=="Rsoil"] + errDF$conf[errDF$cat=="Rsoil"] 
    errDF$neg[errDF$cat=="Rsoil"] <- errDF$sum[errDF$cat=="Rsoil"] - errDF$conf[errDF$cat=="Rsoil"]
    
    
    
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
        ylim(0, 1.6)

    require(grid)
    require(cowplot)
    
    pdf("output/GPP_Rsoil_gap_bootstrap_plots.pdf", width=12,height=10)
    plot_grid(p1, p2, p3, p4, labels="AUTO", ncol=2, align="v", axis="l",
                  rel_widths=c(1,1.2))
    dev.off()

}
