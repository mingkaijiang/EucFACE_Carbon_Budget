gpp_gap_plot <- function(inDF) {
    
    ### subseting NPP DF
    temDF <- inDF$npp[,c("term", "aCO2")]
    
    ### only include plant NPP
    nppDF <- rbind(temDF[temDF$term == "Leaf NPP",],temDF[temDF$term == "Stem NPP",],
                   temDF[temDF$term == "Fine Root NPP",],temDF[temDF$term == "Coarse Root NPP",],
                   temDF[temDF$term == "Other NPP",],temDF[temDF$term == "Understorey NPP",],
                   temDF[temDF$term == "Leaf consumption",])
    
    ### subsetting inout df
    temDF <- inDF$inout[,c("term", "aCO2")]
    
    ### only include GPP
    gppDF <- rbind(temDF[temDF$term == "GPP overstorey",],temDF[temDF$term == "GPP understorey",])
    
    ### only include respiration
    raDF <- rbind(temDF[temDF$term == "Ra leaf",],temDF[temDF$term == "Ra stem",],
                  temDF[temDF$term == "Ra root",],temDF[temDF$term == "Ra understorey",],
                  temDF[temDF$term == "Rherbivore",],temDF[temDF$term == "Rgrowth",])
    
    ### calculated gpp based on NPP + Ra
    gppDF.est <- rbind(nppDF, raDF)
    gppDF.est$cat <- "NPP+Ra"
    gppDF$cat <- "MAESPA"
    
    plotDF <- rbind(gppDF, gppDF.est)
    
    ### calculate missing C
    gpp <- sum(gppDF$value, na.rm=T)
    npp <- sum(nppDF$value, na.rm=T)
    ra <- sum(raDF$value, na.rm=T)
    c.miss <- gpp - npp - ra

    ### make the bar plot
    p <- ggplot(plotDF,
                aes(cat, aCO2)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("Method") + ylab("g C m-2 yr-1") +
        scale_fill_manual(name="term", values = c("GPP overstorey" = "Navy", "GPP understorey" = "blue",
                                                  "Leaf NPP" = "light green", "Stem NPP" = "yellowgreen", "Fine Root NPP" = "springgreen",
                                                  "Coarse Root NPP" = "darkgreen", "Other NPP" = "greenyellow", "Understorey NPP" = "green",
                                                  "Leaf consumption" = "red", "Ra leaf" = "purple", "Ra stem" = "lavender",
                                                  "Ra root" = "tomato4", "Ra understorey" = "coral", "Rherbivore" = "orange",
                                                  "Rgrowth" = "yellow")) 
    plot(p)
    
}