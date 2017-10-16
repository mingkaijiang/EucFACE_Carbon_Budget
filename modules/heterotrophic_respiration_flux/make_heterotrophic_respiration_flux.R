make_heterotrophic_respiration_flux <- function(rsoil, rroot) {
    #### Use Rsoil and Rroot to get Heterotrophic respiration rate
    
    ### combining dataframe
    rhDF <- cbind(rsoil, rroot$root_respiration_flux)
    names(rhDF) <- c("Start_date", "End_date", "Ring", "soil_respiration_flux",
                     "root_respiration_flux")
    
    rhDF$heterotrophic_respiration_flux <- rhDF$soil_respiration_flux - rhDF$root_respiration_flux
    
    rhDF.out <- rhDF[,c("Start_date", "End_date", "Ring", "heterotrophic_respiration_flux")]
     
    return(rhDF.out)
}