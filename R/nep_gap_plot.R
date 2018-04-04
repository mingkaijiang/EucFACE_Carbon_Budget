nep_gap_plot <- function(inDF) {
    
    ### subseting each method
    inoutDF <- inDF$inout[,1:2]
    nppDF <- inDF$npp[,1:2]
    # poolDF <- inDF$pool[,1:2]
    
    ### calculate NEP based on each method
    nep1 <- inoutDF$value[inoutDF$term == "GPP overstorey"] + 
        inoutDF$value[inoutDF$term == "GPP understorey"] -
        #inoutDF$value[inoutDF$term == "CH4 efflux"] -
        inoutDF$value[inoutDF$term == "Ra leaf"] -
        #inoutDF$value[inoutDF$term == "Ra stem"] -
        inoutDF$value[inoutDF$term == "Ra root"] -
        inoutDF$value[inoutDF$term == "Ra understorey"] -
        #inoutDF$value[inoutDF$term == "VOC"] -
        inoutDF$value[inoutDF$term == "Rherbivore"] -
        inoutDF$value[inoutDF$term == "DOC loss"] -
        inoutDF$value[inoutDF$term == "Rsoil"] -
        inoutDF$value[inoutDF$term == "Rgrowth"] 
    
    
    nep2 <- nppDF$value[nppDF$term == "Leaf NPP"] +
        nppDF$value[nppDF$term == "Stem NPP"] +
        nppDF$value[nppDF$term == "Fine Root NPP"] +
        nppDF$value[nppDF$term == "Coarse Root NPP"] +
        nppDF$value[nppDF$term == "Other NPP"] +
        nppDF$value[nppDF$term == "Understorey NPP"] -
        nppDF$value[nppDF$term == "R hetero"] -
        #nppDF$value[nppDF$term == "Mycorrhizal production"] +-
        #nppDF$value[nppDF$term == "Flower production"] -
        nppDF$value[nppDF$term == "Leaf consumption"] 

    # plotDF
    plotDF <- data.frame(rbind(nep1, nep2), NA)
    colnames(plotDF) <- c("NEP", "Method")
    plotDF$Method[1] <- "Inout"
    plotDF$Method[2] <- "NPP"
        
    ### make the bar plot
    p <- ggplot(plotDF,
                aes(Method, NEP)) +   
        geom_bar(stat = "identity",
                 position="stack") +
        xlab("Method") + ylab("NEP [g C m-2 yr-1]")
    plot(p)
    
}