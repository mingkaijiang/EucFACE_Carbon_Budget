rsoil_gap_plot <- function(inDF) {
    
    ### subseting NPP DF
    temDF <- inDF$npp[,1:2]
    
    ### only include plant NPP
    nppDF <- rbind(temDF[temDF$term == "Leaf NPP",],
                   temDF[temDF$term == "Fine Root NPP",],temDF[temDF$term == "Coarse Root NPP",],
                   temDF[temDF$term == "Other NPP",],temDF[temDF$term == "Understorey NPP",],
                   temDF[temDF$term == "Frass production",])
    nppDF <- rbind(nppDF, inDF$inout[inDF$inout$term == "Ra root", 1:2])
    
    ### subsetting inout df
    temDF <- inDF$inout[,1:2]
    
    ### only include GPP
    rsoilDF <- temDF[temDF$term == "Rsoil",]
    
    nppDF$cat <- "Litter+Rroot"
    rsoilDF$cat <- "Rsoil"
    
    plotDF <- rbind(nppDF, rsoilDF)

    ### make the bar plot
    p <- ggplot(plotDF,
                aes(cat, value)) +   
        geom_bar(stat = "identity", aes(fill=term),
                 position="stack") +
        xlab("Method") + ylab("g C m-2 yr-1")
    plot(p)
    
}