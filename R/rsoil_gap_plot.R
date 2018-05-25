rsoil_gap_plot <- function(inDF) {
    
    ### subseting NPP DF
    temDF <- inDF$npp[,c("term", "aCO2")]
    
    ### only include plant NPP
    nppDF <- rbind(temDF[temDF$term == "Leaf NPP",],
                   temDF[temDF$term == "Fine Root NPP",],temDF[temDF$term == "Coarse Root NPP",],
                   temDF[temDF$term == "Other NPP",],temDF[temDF$term == "Understorey NPP",],
                   temDF[temDF$term == "Frass production",])
    aDF <- inDF$inout[, c("term", "aCO2")]
    nppDF <- rbind(nppDF, aDF[aDF$term == "Ra root",])
    
    ### subsetting inout df
    temDF <- inDF$inout[,c("term", "aCO2")]
    
    ### only include Rsoil
    rsoilDF <- temDF[temDF$term == "Rsoil",]
    
    nppDF$cat <- "Litter+Rroot"
    rsoilDF$cat <- "Rsoil"
    
    plotDF <- rbind(nppDF, rsoilDF)

    ### make the bar plot
    p <- ggplot(plotDF,
                aes(cat, aCO2)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("Method") + ylab("g C m-2 yr-1")+
        scale_fill_manual(name="term", values = c(
                                                  "Leaf NPP" = "light green", "Fine Root NPP" = "springgreen",
                                                  "Coarse Root NPP" = "darkgreen", "Other NPP" = "greenyellow", 
                                                  "Understorey NPP" = "green","Frass production" = "red", 
                                                  "Ra root" = "tomato4", "Rsoil" = "orange")) 
    plot(p)
    
}
