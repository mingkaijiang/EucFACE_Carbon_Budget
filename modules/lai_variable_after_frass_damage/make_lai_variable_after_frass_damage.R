# Make LAI modification after frass damage
make_lai_variable_after_frass_damage <- function(old_lai, frass_flux) {
    
    # download frass vs. leaf consumption experimental data
    download_frass_consumption_data()
    
    # read in the consumption relationship data
    inDF1 <- read.csv(file.path(getToPath(), 
                                "GHS36_AG-THESIS_CA_FRASS-LEAFAREA_L2_20110101-20151231.csv"))

    # update column names
    colnames(inDF1) <- c("leaf_area_consumed_cm2", "weight_of_frass_g", "insect_species", "tree_species", "temperature", "co2")
    
    # update units  - leaf area from cm-2 to m-2
    inDF1$leaf_area_consumed_m2 <- inDF1$leaf_area_consumed_cm2 * cm2_to_m2
    
    # update units - g to mg
    inDF1$weight_of_frass_mg <- inDF1$weight_of_frass_g * g_to_mg
    
    # obtain leaf area ~ frass weight relationship
    # currently assuming one relationship for all tree species, all insect species and all co2 levels
    lrtn <- lm(inDF1$leaf_area_consumed_m2~inDF1$weight_of_frass_mg)
    
    # combine frass production and old LAI data according to date and ring
    outDF <- merge(old_lai, frass_flux, by=c("Date","Ring"), all.x=TRUE, all.y=FALSE)
    
    # drop NA data  - currently there are very limited data points left
    # so need to estimate frass production for all dates that have lai information
    # otherwise won't be able to calculate frass damage of lai for all the rest dates
    outDF <- outDF[complete.cases(outDF),]
    
    
    
    new_lai <- old_lai
    
    return(new_lai)
    
}