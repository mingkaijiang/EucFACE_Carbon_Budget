make_understorey_lai_variable <- function(abg_biomass, abg_sla) {

    # Use average SLA over campaigns
    SLA <- summaryBy(Understorey_sla_variable~Ring, data=abg_sla, fun=mean, 
                     keep.names=T)
    
    for (i in c(1:6)) {
        abg_biomass[abg_biomass$Ring == i, "sla"] <- SLA[SLA$Ring == i, "Understorey_sla_variable"]
    } 
    
    # calcualte lai
    abg_biomass$lai <- with(abg_biomass, sla * cm2_to_m2 * (Live_g_C_m2/c_fraction))
    
    out <- data.frame(abg_biomass$Date, abg_biomass$Ring, abg_biomass$lai)
    names(out) <- c("Date", "Ring", "LAI")
    
    #write.csv(out, "download/understorey_lai.csv", row.names=F)
    
    return(out)
}

