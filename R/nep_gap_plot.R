nep_gap_plot <- function(inDF) {
    
    ### subseting each method
    inoutDF <- inDF$inout[,1:2]
    nppDF <- inDF$npp[,1:2]

    ### calculate NEP based on each method
    nep1 <- inoutDF$value[inoutDF$term == "GPP overstorey"] + 
        inoutDF$value[inoutDF$term == "GPP understorey"] -
        inoutDF$value[inoutDF$term == "CH4 efflux"] -
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
    
    ### Change in pools
    source("R/stats/change_in_pool/make_change_in_pool.R")
    
    ### Compute changes in pool variables
    delta_soil_c <- make_change_in_pool(mypool=soil_c_pool, var.col=3)
    delta_leaf_c <- make_change_in_pool(mypool=leaf_c_pool, var.col=3)
    delta_wood_c <- make_change_in_pool(mypool=wood_c_pool, var.col=3)
    delta_croot_c <- make_change_in_pool(mypool=coarse_root_c_pool_1, var.col=3)
    delta_froot_c <- make_change_in_pool(mypool=fineroot_c_pool, var.col=3)
    delta_ua_c <- make_change_in_pool(mypool=understorey_aboveground_c_pool, var.col=5)
    delta_mic_c <- make_change_in_pool(mypool=microbial_c_pool, var.col=3)
    delta_myc_c <- make_change_in_pool(mypool=mycorrhizal_c_pool, var.col=3)
    # missing litter, CWD, insect
    

    ### create df to store pools
    pool.list <- c("soilc", "leafc", "woodc", "crootc", "frootc", "uac",
                   "micc", "mycc", "litter", "cwd", "insect")
    poolDF <- data.frame(pool.list, NA)
    colnames(poolDF) <- c("Term", "Value")
    
    ### A function to calculate means of each ring, return unit of g C m-2 yr-1
    calculate_variable_mean <- function(delta_pool) {
        temp <- data.frame(c(1:6), NA)
        colnames(temp) <- c("Ring", "Value")
        
        for (i in 1:6) {
            temp$Value[temp$Ring==i] <- with(delta_pool[delta_pool$Ring == i,],
                                             sum(daily_biomass_change*ndays)/sum(ndays)) * 365 
        }
        
        out <- mean(temp$Value, na.rm=T)
        
        return(out)
    }

    ### assign values
    poolDF$Value[poolDF$Term=="soilc"] <- calculate_variable_mean(delta_soil_c)
    poolDF$Value[poolDF$Term=="leafc"] <- calculate_variable_mean(delta_leaf_c)
    poolDF$Value[poolDF$Term=="woodc"] <- calculate_variable_mean(delta_wood_c)
    poolDF$Value[poolDF$Term=="crootc"] <- calculate_variable_mean(delta_croot_c)
    poolDF$Value[poolDF$Term=="frootc"] <- calculate_variable_mean(delta_froot_c)
    poolDF$Value[poolDF$Term=="uac"] <- calculate_variable_mean(delta_ua_c)
    poolDF$Value[poolDF$Term=="micc"] <- calculate_variable_mean(delta_mic_c)
    poolDF$Value[poolDF$Term=="mycc"] <- calculate_variable_mean(delta_myc_c)
    poolDF$Value[poolDF$Term=="insect"] <- 0.0
    poolDF$Value[poolDF$Term=="cwd"] <- 0.0
    poolDF$Value[poolDF$Term=="litter"] <- 0.0
    
    ### NEP change in pools
    nep3 <- sum(poolDF$Value)
    
    # plotDF
    plotDF <- data.frame(rbind(nep1, nep2, nep3), NA)
    colnames(plotDF) <- c("NEP", "Method")
    plotDF$Method[1] <- "Inout"
    plotDF$Method[2] <- "NPP"
    plotDF$Method[3] <- "Pool"
    
    ### make the bar plot
    p <- ggplot(plotDF,
                aes(Method, NEP)) +   
        geom_bar(stat = "identity",
                 position="stack") +
        xlab("Method") + ylab(paste0(expression("NEP (g C ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")
    
    pdf("R_other/nep_gap.pdf")
    plot(p)
    dev.off()
    
}