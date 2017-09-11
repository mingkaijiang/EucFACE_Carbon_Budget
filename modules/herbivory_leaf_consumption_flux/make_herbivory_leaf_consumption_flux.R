# Make leaf mass consumption by herbivory flux
make_herbivory_leaf_consumption_flux <- function(sla,frass_flux) {
    
    # download frass vs. leaf consumption experimental data
    download_frass_consumption_data()
    
    # read in the consumption relationship data
    inDF1 <- read.csv(file.path(getToPath(), 
                                "GHS36_AG-THESIS_CA_FRASS-LEAFAREA_L2_20110101-20151231.csv"))

    # update column names
    colnames(inDF1) <- c("leaf_area_consumed_cm2", "weight_of_frass_g", "insect_species", "tree_species", "temperature", "co2")
    
    # update units - g to mg
    inDF1$weight_of_frass_mg <- inDF1$weight_of_frass_g * g_to_mg
    
    # average sla for all rings   unit: cm2/g
    sla_avg <- mean(sla$sla_variable, na.rm=T)
    
    # derive herbivory consumption of leaf mass based on leaf area lost and sla
    inDF1$leaf_mass_consumed_mg <- inDF1$leaf_area_consumed_cm2 / sla_avg * g_to_mg
    
    # visually assess co2-based relationships
    # with(inDF1[inDF1$co2== 400, ], plot(leaf_mass_consumed_mg~weight_of_frass_mg, col="red"))
    # with(inDF1[inDF1$co2== 640, ], points(leaf_mass_consumed_mg~weight_of_frass_mg, col="blue"))
    
    # visually examine tree species based relationships 
    # require(ggplot2)
    # ggplot(inDF1, aes(weight_of_frass_mg, leaf_mass_consumed_mg, colour=tree_species)) + 
    #    geom_point()
    
    
    # obtain leaf consumption ~ frass weight relationship
    # currently assuming one relationship for all tree species, all insect species and all co2 levels
    lrtn <- lm(inDF1$leaf_mass_consumed_mg~inDF1$weight_of_frass_mg)
    
    # extract coefficients 
    int <- coefficients(lrtn)[[1]]
    slp <- coefficients(lrtn)[[2]]
    
    # generate leaf consumption mass df
    out <- frass_flux
    out$herbivory_leaf_consumption_flux <- out$frass_production_flux * slp + int
    
    out$frass_production_flux <- NULL
    
    outDF <- out[,c("Start_date", "End_date", "Ring", "herbivory_leaf_consumption_flux")]
    
    # question: would this consumed leaf C be added on top of the leaf pool?
    
    return(outDF)
    
}