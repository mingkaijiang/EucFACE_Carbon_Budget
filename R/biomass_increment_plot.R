#### To make EucFACE table by year
#### Ignore ring variability
biomass_increment_plot <- function() {
    
    ### Define terms and dataframe
    term <- c("OverstoreyLeaf", "OverstoreyWood", "UnderstoreyAboveground",
              "FineRoot", "CoarseRoot", "Litter", "CoarseWoodyDebris", 
              "MicrobialBiomass", "SoilC", "Mycorrhizae", "Insects")
    
    incDF <- data.frame(term, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(incDF) <- c("term", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    
    
    ### Overstorey leaf
    min.date <- min(leaf_c_pool$Date)
    max.date <- max(leaf_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- leaf_c_pool[leaf_c_pool$Ring == i & leaf_c_pool$Date == max.date, "leaf_pool"] - leaf_c_pool[leaf_c_pool$Ring == i & leaf_c_pool$Date == min.date, "leaf_pool"]
        incDF[incDF$term == "OverstoreyLeaf", i+1] <- round(diff / date.diff * 365, 2)
        #incDF[incDF$term == "OverstoreyLeaf", i+1] <- diff
        
    }


    ### Overstorey wood
    min.date <- min(wood_c_pool$Date)
    max.date <- max(wood_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- wood_c_pool[wood_c_pool$Ring == i & wood_c_pool$Date == max.date, "wood_pool"] - wood_c_pool[wood_c_pool$Ring == i & wood_c_pool$Date == min.date, "wood_pool"]
        incDF[incDF$term == "OverstoreyWood", i+1] <- round(diff / date.diff * 365, 2)
        #incDF[incDF$term == "OverstoreyWood", i+1] <- diff
        
    }
    

    ### Understorey aboveground
    min.date <- min(understorey_aboveground_c_pool$Date)
    max.date <- max(understorey_aboveground_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- understorey_aboveground_c_pool[understorey_aboveground_c_pool$Ring == i & understorey_aboveground_c_pool$Date == max.date, "Total_g_C_m2"] - understorey_aboveground_c_pool[understorey_aboveground_c_pool$Ring == i & understorey_aboveground_c_pool$Date == min.date, "Total_g_C_m2"]
        incDF[incDF$term == "UnderstoreyAboveground", i+1] <- round(diff / date.diff * 365, 2)
        #incDF[incDF$term == "UnderstoreyAboveground", i+1] <- diff
        
    }

    ### Fine root
    min.date <- min(fineroot_c_pool$Date)
    max.date <- max(fineroot_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- fineroot_c_pool[fineroot_c_pool$Ring == i & fineroot_c_pool$Date == max.date, "fineroot_pool"] - fineroot_c_pool[fineroot_c_pool$Ring == i & fineroot_c_pool$Date == min.date, "fineroot_pool"]
        incDF[incDF$term == "FineRoot", i+1] <- round(diff / date.diff * 365, 2)
        #incDF[incDF$term == "FineRoot", i+1] <- diff
    }

    ### Coarse root
    min.date <- min(coarse_root_c_pool_1$Date)
    max.date <- max(coarse_root_c_pool_1$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- coarse_root_c_pool_1[coarse_root_c_pool_1$Ring == i & coarse_root_c_pool_1$Date == max.date, "coarse_root_pool"] - coarse_root_c_pool_1[coarse_root_c_pool_1$Ring == i & coarse_root_c_pool_1$Date == min.date, "coarse_root_pool"]
        incDF[incDF$term == "CoarseRoot", i+1] <- round(diff / date.diff * 365, 2)
        #incDF[incDF$term == "CoarseRoot", i+1] <- diff
        
    }

    ### Soil C
    min.date <- min(soil_c_pool$Date)
    max.date <- max(soil_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- soil_c_pool[soil_c_pool$Ring == i & soil_c_pool$Date == max.date, "soil_carbon_pool"] - soil_c_pool[soil_c_pool$Ring == i & soil_c_pool$Date == min.date, "soil_carbon_pool"]
        incDF[incDF$term == "SoilC", i+1] <- round(diff / date.diff * 365, 2)
        #incDF[incDF$term == "SoilC", i+1] <- diff
        
    }

    ### microbial pool
    min.date <- min(microbial_c_pool$Date)
    max.date <- max(microbial_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- microbial_c_pool[microbial_c_pool$Ring == i & microbial_c_pool$Date == max.date, "microbial_pool"] - microbial_c_pool[microbial_c_pool$Ring == i & microbial_c_pool$Date == min.date, "microbial_pool"]
        incDF[incDF$term == "MicrobialBiomass", i+1] <- round(diff / date.diff * 365, 2)
        #incDF[incDF$term == "MicrobialBiomass", i+1] <- diff
        
    }
    
    ### mycorrhizal pool
    min.date <- min(mycorrhizal_c_pool$Date)
    max.date <- max(mycorrhizal_c_pool$Date)
    date.diff <- as.numeric(max.date - min.date)
    
    for (i in 1:6) {
        diff <- mycorrhizal_c_pool[mycorrhizal_c_pool$Ring == i & mycorrhizal_c_pool$Date == max.date, "mycorrhizal_c_pool"] - mycorrhizal_c_pool[mycorrhizal_c_pool$Ring == i & mycorrhizal_c_pool$Date == min.date, "mycorrhizal_c_pool"]
        incDF[incDF$term == "Mycorrhizae", i+1] <- round(diff / date.diff * 365, 2)
        #incDF[incDF$term == "Mycorrhizae", i+1] <- diff
    }
        
    # CO2 effect
    incDF$aCO2 <- rowMeans(subset(incDF, select=c(R2, R3, R6)), na.rm=T)
    incDF$eCO2 <- rowMeans(subset(incDF, select=c(R1, R4, R5)), na.rm=T)
    
    incDF$diff <- round(incDF$eCO2 - incDF$aCO2,2)
    incDF$percent_diff <- round(incDF$diff / incDF$aCO2,2)
    
    incDF$aCO2_sd <- rowSds(as.matrix(subset(incDF, select=c(R2, R3, R6))), na.rm=T)
    incDF$eCO2_sd <- rowSds(as.matrix(subset(incDF, select=c(R1, R4, R5))), na.rm=T)
    
    # new terms
    new.terms <- c("Col", "Cw", "Cua", "Cfr", "Ccr", "Cl", "Ccwd", "Cmi", "Cs", "Cmy", "Ci")
    
    ### prepare plot DF
    plotDF1 <- data.frame(new.terms, incDF$aCO2, incDF$aCO2_sd)
    names(plotDF1) <- c("term", "increment", "sd")
    plotDF2 <- data.frame(new.terms, incDF$eCO2, incDF$eCO2_sd)
    names(plotDF2) <- c("term", "increment", "sd")
    
    plotDF <- rbind(plotDF1, plotDF2)
    n <- nrow(plotDF1)
    plotDF$treatment <- rep(c("aCO2", "eCO2"), each=n)
    
    plotDF$pos <- plotDF$increment + plotDF$sd
    plotDF$neg <- plotDF$increment - plotDF$sd
    
    plotDF <- plotDF[complete.cases(plotDF),]
    

    ### make the bar plot
    p1 <- ggplot(plotDF,
                aes(x=term, y=increment, fill=treatment)) +   
        geom_bar(stat="identity", position=position_dodge()) +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(treatment)), 
                      position = position_dodge(0.9), width=0.2) +
        xlab("pools") + ylab(expression(paste("Biomass Increment (g C ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="right")+
        scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete("Pool variable", 
                         labels=c(expression(C[cr]),
                                  expression(C[fr]),
                                  expression(C[mi]),
                                  expression(C[my]),
                                  expression(C[ol]),
                                  expression(C[s]),
                                  expression(C[ua]),
                                  expression(C[w])))
    
    
    pdf("R_other/biomass_increment.pdf")
    plot(p1)
    dev.off()
    
    
    
}
