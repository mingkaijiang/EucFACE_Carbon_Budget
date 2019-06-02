gpp_and_rsoil_gap_unbootstrap_plot_2 <- function(inDF) {
    
    ### subseting DF
    temDF1 <- inDF$npp[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6", "aCO2", "eCO2")]
    temDF2 <- inDF$inout[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6", "aCO2", "eCO2")]
    temDF3 <- inDF$pool[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6", "aCO2", "eCO2")]
    temDF4 <- inDF$delta_pool[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6", "aCO2", "eCO2")]
    
    
    ### create data frames to store ring average for aCO2 and eCO2
    ## aCO2
    plotDF1 <- rbind(temDF2[temDF2$term == "GPP overstorey", "aCO2"],
                     temDF2[temDF2$term == "GPP understorey", "aCO2"],
                     temDF1[temDF1$term == "Leaf NPP","aCO2"],
                     temDF1[temDF1$term == "Stem NPP","aCO2"],
                     temDF1[temDF1$term == "Fine Root NPP","aCO2"],
                     temDF1[temDF1$term == "Coarse Root NPP","aCO2"],
                     temDF1[temDF1$term == "Other NPP","aCO2"],
                     temDF1[temDF1$term == "Understorey NPP","aCO2"],
                     temDF1[temDF1$term == "Leaf consumption","aCO2"],
                     temDF2[temDF2$term == "Ra leaf","aCO2"],
                     temDF2[temDF2$term == "Ra stem","aCO2"],
                     temDF2[temDF2$term == "Ra root","aCO2"],
                     temDF2[temDF2$term == "Ra understorey","aCO2"],
                     temDF2[temDF2$term == "Rgrowth","aCO2"],
                     temDF2[temDF2$term == "VOC", "aCO2"])
    
    plotDF2 <- rbind(temDF2[temDF2$term == "GPP overstorey", "eCO2"],
                     temDF2[temDF2$term == "GPP understorey", "eCO2"],
                     temDF1[temDF1$term == "Leaf NPP","eCO2"],
                     temDF1[temDF1$term == "Stem NPP","eCO2"],
                     temDF1[temDF1$term == "Fine Root NPP","eCO2"],
                     temDF1[temDF1$term == "Coarse Root NPP","eCO2"],
                     temDF1[temDF1$term == "Other NPP","eCO2"],
                     temDF1[temDF1$term == "Understorey NPP","eCO2"],
                     temDF1[temDF1$term == "Leaf consumption","eCO2"],
                     temDF2[temDF2$term == "Ra leaf","eCO2"],
                     temDF2[temDF2$term == "Ra stem","eCO2"],
                     temDF2[temDF2$term == "Ra root","eCO2"],
                     temDF2[temDF2$term == "Ra understorey","eCO2"],
                     temDF2[temDF2$term == "Rgrowth","eCO2"],
                     temDF2[temDF2$term == "VOC", "eCO2"])
    plotDF1$cat <- plotDF2$cat <- c(rep("MAESPA", 2), rep("NPP+Ra", 13))
    plotDF1$term <- plotDF2$term <- c("GPP overstorey", "GPP understorey",
                                      "Leaf NPP","Stem NPP","Fine Root NPP","Coarse Root NPP","Other NPP",
                                      "Understorey NPP","Leaf consumption","Ra leaf","Ra stem","Ra root",
                                      "Ra understorey","Rgrowth","VOC")
    
    plotDF1$term <- factor(plotDF1$term, levels=unique(plotDF1$term))
    plotDF2$term <- factor(plotDF2$term, levels=unique(plotDF2$term))
    
    plotDF1$aCO2 <- plotDF1$aCO2/1000
    plotDF2$eCO2 <- plotDF2$eCO2/1000
    
    
    ### create data frames to store ring data for aCO2 and eCO2
    ## aCO2
    errDF1 <- data.frame(c("NPP+Ra", "MAESPA"), NA, NA, NA)
    colnames(errDF1) <- c("cat", "R2", "R3", "R6")
    ## eCO2
    errDF2 <- data.frame(c("NPP+Ra", "MAESPA"), NA, NA, NA)
    colnames(errDF2) <- c("cat", "R1", "R4", "R5")

    ### calculate sum of NPP + ra for each ring
    ## aCO2
    errDF1[errDF1$cat=="NPP+Ra", "R2"] <- sum(temDF1[temDF1$term == "Leaf NPP","Ring_2"],
                                                temDF1[temDF1$term == "Stem NPP","Ring_2"],
                                                temDF1[temDF1$term == "Fine Root NPP","Ring_2"],
                                                temDF1[temDF1$term == "Coarse Root NPP","Ring_2"],
                                                temDF1[temDF1$term == "Other NPP","Ring_2"],
                                                temDF1[temDF1$term == "Understorey NPP","Ring_2"],
                                                temDF1[temDF1$term == "Leaf consumption","Ring_2"],
                                                temDF2[temDF2$term == "Ra leaf","Ring_2"],
                                                temDF2[temDF2$term == "Ra stem","Ring_2"],
                                                temDF2[temDF2$term == "Ra root","Ring_2"],
                                                temDF2[temDF2$term == "Ra understorey","Ring_2"],
                                                temDF2[temDF2$term == "Rgrowth","Ring_2"],
                                                temDF2[temDF2$term == "VOC","Ring_2"])
    
    
    errDF1[errDF1$cat=="NPP+Ra", "R3"] <- sum(temDF1[temDF1$term == "Leaf NPP","Ring_3"],
                                                temDF1[temDF1$term == "Stem NPP","Ring_3"],
                                                temDF1[temDF1$term == "Fine Root NPP","Ring_3"],
                                                temDF1[temDF1$term == "Coarse Root NPP","Ring_3"],
                                                temDF1[temDF1$term == "Other NPP","Ring_3"],
                                                temDF1[temDF1$term == "Understorey NPP","Ring_3"],
                                                temDF1[temDF1$term == "Leaf consumption","Ring_3"],
                                                temDF2[temDF2$term == "Ra leaf","Ring_3"],
                                                temDF2[temDF2$term == "Ra stem","Ring_3"],
                                                temDF2[temDF2$term == "Ra root","Ring_3"],
                                                temDF2[temDF2$term == "Ra understorey","Ring_3"],
                                                temDF2[temDF2$term == "Rgrowth","Ring_3"],
                                                temDF2[temDF2$term == "VOC","Ring_3"])
    
    
    errDF1[errDF1$cat=="NPP+Ra", "R6"] <- sum(temDF1[temDF1$term == "Leaf NPP","Ring_6"],
                                                temDF1[temDF1$term == "Stem NPP","Ring_6"],
                                                temDF1[temDF1$term == "Fine Root NPP","Ring_6"],
                                                temDF1[temDF1$term == "Coarse Root NPP","Ring_6"],
                                                temDF1[temDF1$term == "Other NPP","Ring_6"],
                                                temDF1[temDF1$term == "Understorey NPP","Ring_6"],
                                                temDF1[temDF1$term == "Leaf consumption","Ring_6"],
                                                temDF2[temDF2$term == "Ra leaf","Ring_6"],
                                                temDF2[temDF2$term == "Ra stem","Ring_6"],
                                                temDF2[temDF2$term == "Ra root","Ring_6"],
                                                temDF2[temDF2$term == "Ra understorey","Ring_6"],
                                                temDF2[temDF2$term == "Rgrowth","Ring_6"],
                                                temDF2[temDF2$term == "VOC","Ring_6"])
    
    
    ## eCO2
    errDF2[errDF2$cat=="NPP+Ra", "R1"] <- sum(temDF1[temDF1$term == "Leaf NPP","Ring_1"],
                                                temDF1[temDF1$term == "Stem NPP","Ring_1"],
                                                temDF1[temDF1$term == "Fine Root NPP","Ring_1"],
                                                temDF1[temDF1$term == "Coarse Root NPP","Ring_1"],
                                                temDF1[temDF1$term == "Other NPP","Ring_1"],
                                                temDF1[temDF1$term == "Understorey NPP","Ring_1"],
                                                temDF1[temDF1$term == "Leaf consumption","Ring_1"],
                                                temDF2[temDF2$term == "Ra leaf","Ring_1"],
                                                temDF2[temDF2$term == "Ra stem","Ring_1"],
                                                temDF2[temDF2$term == "Ra root","Ring_1"],
                                                temDF2[temDF2$term == "Ra understorey","Ring_1"],
                                                temDF2[temDF2$term == "Rgrowth","Ring_1"],
                                                temDF2[temDF2$term == "VOC","Ring_1"])
    
    
    errDF2[errDF2$cat=="NPP+Ra", "R4"] <- sum(temDF1[temDF1$term == "Leaf NPP","Ring_4"],
                                                temDF1[temDF1$term == "Stem NPP","Ring_4"],
                                                temDF1[temDF1$term == "Fine Root NPP","Ring_4"],
                                                temDF1[temDF1$term == "Coarse Root NPP","Ring_4"],
                                                temDF1[temDF1$term == "Other NPP","Ring_4"],
                                                temDF1[temDF1$term == "Understorey NPP","Ring_4"],
                                                temDF1[temDF1$term == "Leaf consumption","Ring_4"],
                                                temDF2[temDF2$term == "Ra leaf","Ring_4"],
                                                temDF2[temDF2$term == "Ra stem","Ring_4"],
                                                temDF2[temDF2$term == "Ra root","Ring_4"],
                                                temDF2[temDF2$term == "Ra understorey","Ring_4"],
                                                temDF2[temDF2$term == "Rgrowth","Ring_4"],
                                                temDF2[temDF2$term == "VOC","Ring_4"])
    
    
    errDF2[errDF2$cat=="NPP+Ra", "R5"] <- sum(temDF1[temDF1$term == "Leaf NPP","Ring_5"],
                                                temDF1[temDF1$term == "Stem NPP","Ring_5"],
                                                temDF1[temDF1$term == "Fine Root NPP","Ring_5"],
                                                temDF1[temDF1$term == "Coarse Root NPP","Ring_5"],
                                                temDF1[temDF1$term == "Other NPP","Ring_5"],
                                                temDF1[temDF1$term == "Understorey NPP","Ring_5"],
                                                temDF1[temDF1$term == "Leaf consumption","Ring_5"],
                                                temDF2[temDF2$term == "Ra leaf","Ring_5"],
                                                temDF2[temDF2$term == "Ra stem","Ring_5"],
                                                temDF2[temDF2$term == "Ra root","Ring_5"],
                                                temDF2[temDF2$term == "Ra understorey","Ring_5"],
                                                temDF2[temDF2$term == "Rgrowth","Ring_5"],
                                                temDF2[temDF2$term == "VOC","Ring_5"])
    
    ### calculate sum of GPP for each ring
    ## aCO2
    errDF1[errDF1$cat=="MAESPA", "R2"] <- sum(temDF2[temDF2$term == "GPP overstorey","Ring_2"],
                                                temDF2[temDF2$term == "GPP understorey","Ring_2"])
    
    errDF1[errDF1$cat=="MAESPA", "R3"] <- sum(temDF2[temDF2$term == "GPP overstorey","Ring_3"],
                                                temDF2[temDF2$term == "GPP understorey","Ring_3"])
    
    errDF1[errDF1$cat=="MAESPA", "R6"] <- sum(temDF2[temDF2$term == "GPP overstorey","Ring_6"],
                                                temDF2[temDF2$term == "GPP understorey","Ring_6"])
    
    ## eCO2
    errDF2[errDF2$cat=="MAESPA", "R1"] <- sum(temDF2[temDF2$term == "GPP overstorey","Ring_1"],
                                                temDF2[temDF2$term == "GPP understorey","Ring_1"])
    
    errDF2[errDF2$cat=="MAESPA", "R4"] <- sum(temDF2[temDF2$term == "GPP overstorey","Ring_4"],
                                                temDF2[temDF2$term == "GPP understorey","Ring_4"])
    
    errDF2[errDF2$cat=="MAESPA", "R5"] <- sum(temDF2[temDF2$term == "GPP overstorey","Ring_5"],
                                                temDF2[temDF2$term == "GPP understorey","Ring_5"])
    
    
    ## calculate means and sd, convert unit from g C to kg C
    errDF1$aCO2 <- rowMeans(subset(errDF1, select=c(R2, R3, R6)), na.rm=T)/1000
    errDF1$aCO2_sd <- rowSds(as.matrix(subset(errDF1, select=c(R2, R3, R6)), na.rm=T))/1000
    
    errDF2$eCO2 <- rowMeans(subset(errDF2, select=c(R1, R4, R5)), na.rm=T)/1000
    errDF2$eCO2_sd <- rowSds(as.matrix(subset(errDF2, select=c(R1, R4, R5)), na.rm=T))/1000
    
    errDF1$pos <- errDF1$aCO2+errDF1$aCO2_sd
    errDF2$pos <- errDF2$eCO2+errDF2$eCO2_sd
    
    errDF1$neg <- errDF1$aCO2-errDF1$aCO2_sd
    errDF2$neg <- errDF2$eCO2-errDF2$eCO2_sd
    
    
    ### Prepare variable labels
    var.labs1 <- c(expression(GPP[o]), expression(GPP[u]),
                  expression(NPP[leaf]), expression(NPP[stem]),
                  expression(NPP[froot]), expression(NPP[croot]),
                  expression(NPP[other]), expression(NPP[ua]),
                  expression(NPP[hb]), 
                  expression(R[leaf]),
                  expression(R[stem]), expression(R[root]),
                  expression(R[ua]), 
                  expression(R[g]), "VOC")
    
    ### Prepare variable colors
    require(viridis)
    col.list1 <- viridis(15)
    
    ### make the bar plot
    p1 <- ggplot(plotDF1,
                aes(cat, aCO2)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        geom_segment(data=errDF1, aes(x=cat, xend=cat, y=neg, yend=pos), 
                     colour="black")+
        geom_point(data=errDF1, mapping=aes(x=cat, y=aCO2), 
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
        
    plot(p1)

    
    ### make the bar plot
    p2 <- ggplot(plotDF2,
                 aes(cat, eCO2)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        geom_segment(data=errDF2, aes(x=cat, xend=cat, y=neg, yend=pos), 
                     colour="black")+
        geom_point(data=errDF2, mapping=aes(x=cat, y=eCO2), 
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
    
    plot(p2)
    
    ### subseting NPP DF
    temDF <- inDF$npp[,c("term", "aCO2", "aCO2_sd")]
    
    ### only include plant NPP
    nppDF <- rbind(temDF[temDF$term == "Leaf NPP",],
                   temDF[temDF$term == "Fine Root NPP",],temDF[temDF$term == "Coarse Root NPP",],
                   temDF[temDF$term == "Other NPP",],temDF[temDF$term == "Understorey NPP",],
                   temDF[temDF$term == "Frass production",])
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
    
    
    ### prepare error bar ranges
    errDF <- data.frame(c("Litter+Rroot", "Rsoil"), NA, NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg", "sum")
    errDF$sum[errDF$cat=="Litter+Rroot"] <- sum(plotDF$aCO2[plotDF$cat=="Litter+Rroot"])
    errDF$sum[errDF$cat=="Rsoil"] <- sum(plotDF$aCO2[plotDF$cat=="Rsoil"])
    
    errDF$sd[errDF$cat=="Litter+Rroot"] <- sqrt((plotDF[plotDF$term=="Leaf NPP","aCO2_sd"]^2 + 
                                                    plotDF[plotDF$term=="Fine Root NPP","aCO2_sd"]^2 + 
                                                    plotDF[plotDF$term=="Coarse Root NPP","aCO2_sd"]^2 + 
                                                    plotDF[plotDF$term=="Other NPP","aCO2_sd"]^2 + 
                                                    plotDF[plotDF$term=="Understorey NPP","aCO2_sd"]^2 + 
                                                    plotDF[plotDF$term=="Frass production","aCO2_sd"]^2 + 
                                                    plotDF[plotDF$term=="Ra root","aCO2_sd"]^2)/7)
    errDF$sd[errDF$cat=="Rsoil"] <- plotDF[plotDF$term=="Rsoil","aCO2_sd"]
    
    errDF$conf[errDF$cat=="Litter+Rroot"] <- errDF$sd[errDF$cat=="Litter+Rroot"]
    errDF$conf[errDF$cat=="Rsoil"] <- errDF$sd[errDF$cat=="Rsoil"]
    
    errDF$pos[errDF$cat=="Litter+Rroot"] <- errDF$sum[errDF$cat=="Litter+Rroot"] + as.numeric(errDF$conf[errDF$cat=="Litter+Rroot"])
    errDF$neg[errDF$cat=="Litter+Rroot"] <- errDF$sum[errDF$cat=="Litter+Rroot"] - as.numeric(errDF$conf[errDF$cat=="Litter+Rroot"]) 
    errDF$pos[errDF$cat=="Rsoil"] <- errDF$sum[errDF$cat=="Rsoil"] + as.numeric(errDF$conf[errDF$cat=="Rsoil"])
    errDF$neg[errDF$cat=="Rsoil"] <- errDF$sum[errDF$cat=="Rsoil"] - as.numeric(errDF$conf[errDF$cat=="Rsoil"])
    
    
    ### Prepare variable labels
    var.labs2 <- c(expression(NPP[leaf]), expression(NPP[froot]),
                  expression(NPP[croot]),expression(NPP[other]), 
                  expression(NPP[ua]),expression(Frass), #expression(NPP[myc]), 
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
    
    
    ### prepare error bar ranges
    errDF <- data.frame(c("Litter+Rroot", "Rsoil"), NA, NA, NA)
    colnames(errDF) <- c("cat", "pos", "neg", "sum")
    errDF$sum[errDF$cat=="Litter+Rroot"] <- sum(plotDF$eCO2[plotDF$cat=="Litter+Rroot"])
    errDF$sum[errDF$cat=="Rsoil"] <- sum(plotDF$eCO2[plotDF$cat=="Rsoil"])
    
    errDF$sd[errDF$cat=="Litter+Rroot"] <- sqrt((plotDF[plotDF$term=="Leaf NPP","eCO2_sd"]^2 + 
                                                    plotDF[plotDF$term=="Fine Root NPP","eCO2_sd"]^2 + 
                                                    plotDF[plotDF$term=="Coarse Root NPP","eCO2_sd"]^2 + 
                                                    plotDF[plotDF$term=="Other NPP","eCO2_sd"]^2 + 
                                                    plotDF[plotDF$term=="Understorey NPP","eCO2_sd"]^2 + 
                                                    plotDF[plotDF$term=="Frass production","eCO2_sd"]^2 + 
                                                    plotDF[plotDF$term=="Ra root","eCO2_sd"]^2)/7)
    errDF$sd[errDF$cat=="Rsoil"] <- plotDF[plotDF$term=="Rsoil","eCO2_sd"]
    
    errDF$conf[errDF$cat=="Litter+Rroot"] <- errDF$sd[errDF$cat=="Litter+Rroot"]
    errDF$conf[errDF$cat=="Rsoil"] <- errDF$sd[errDF$cat=="Rsoil"]
    
    errDF$pos[errDF$cat=="Litter+Rroot"] <- errDF$sum[errDF$cat=="Litter+Rroot"] + as.numeric(errDF$conf[errDF$cat=="Litter+Rroot"])
    errDF$neg[errDF$cat=="Litter+Rroot"] <- errDF$sum[errDF$cat=="Litter+Rroot"] - as.numeric(errDF$conf[errDF$cat=="Litter+Rroot"]) 
    errDF$pos[errDF$cat=="Rsoil"] <- errDF$sum[errDF$cat=="Rsoil"] + as.numeric(errDF$conf[errDF$cat=="Rsoil"])
    errDF$neg[errDF$cat=="Rsoil"] <- errDF$sum[errDF$cat=="Rsoil"] - as.numeric(errDF$conf[errDF$cat=="Rsoil"])
    
    
    
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
