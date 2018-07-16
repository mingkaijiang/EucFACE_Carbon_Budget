### Herbivory respiration flux
make_herbivory_respiration_flux <- function(leaf_consumed, frass_prod, method) {
    ### leaf_consumed: df for leaf consumption flux
    ### frass_prod: df for frass production flux
    
    if (method == "diff") {
        leaf_consumed$frass_prod <- frass_prod$frass_production_flux
        
        leaf_consumed$respiration_flux <- leaf_consumed$herbivory_leaf_consumption_flux-leaf_consumed$frass_prod
        
        leaf_consumed$ndays <- as.numeric(leaf_consumed$End_date - leaf_consumed$Start_date) + 1
        
        out <- leaf_consumed[,c("Start_date", "End_date", "Date", "Ring", "respiration_flux", "ndays")]
        
    } else if (method == "cue") {
        frass_prod$respiration_flux <- frass_prod$frass_production_flux / insect_cue - frass_prod$frass_production_flux
        
        out <- leaf_prod[,c("Start_date", "End_date", "Date", "Ring", "respiration_flux", "ndays")]
        
    }
    
    # Only use data period 2012-2016
    out <- out[out$Date<="2016-12-31",]

    return(out)
}