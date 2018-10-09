make_understorey_lai_variable_2 <- function(abg_biomass, abg_sla, prDF) {

    # Use average SLA over campaigns
    SLA <- summaryBy(Understorey_sla_variable~Ring, data=abg_sla, fun=mean, 
                     keep.names=T)
    
    ### Assign SLA onto biomass dataframe
    for (i in c(1:6)) {
        abg_biomass[abg_biomass$Ring == i, "sla"] <- SLA[SLA$Ring == i, "Understorey_sla_variable"]
    } 
    
    ### Calculate % Live and therefore green based on harvesting data
    prDF$live_percent <- with(prDF, Live_g_C_m2/Total_g_C_m2)
    pDF <- summaryBy(live_percent~Ring, data=prDF, FUN=mean, keep.names=T)
    
    ### assign % live info onto data frame
    for (i in c(1:6)) {
        abg_biomass[abg_biomass$Ring == i, "percent_live"] <- pDF$live_percent[pDF$Ring==i]
    } 
    
    abg_biomass$Live_g_C_m2 <- abg_biomass$Total_g_C_m2 * abg_biomass$percent_live
    
    # calcualte lai
    abg_biomass$lai <- with(abg_biomass, sla * cm2_to_m2 * (Live_g_C_m2/c_fraction))
    
    abg_biomass$Trt[abg_biomass$Ring%in%c(1,4,5)] <- "eCO2"
    abg_biomass$Trt[abg_biomass$Ring%in%c(2,3,6)] <- "aCO2"
    
    out <- data.frame(abg_biomass$Date, abg_biomass$Ring, abg_biomass$lai)
    names(out) <- c("Date", "Ring", "LAI")
    
    #out$Trt[out$Ring%in%c(1,4,5)] <- "eCO2"
    #out$Trt[out$Ring%in%c(2,3,6)] <- "aCO2"
    #
    #ggplot(out, aes(x=Trt, y=LAI)) +
    #    geom_boxplot()
    #
    #ggplot(abg_biomass, aes(x=Trt, y=sla))+
    #    geom_boxplot()
    #
    #ggplot(abg_biomass, aes(x=Trt, y=Live_g_C_m2))+
    #    geom_boxplot()
    #
    #ggplot(abg_biomass, aes(x=Trt, y=Total_g_C_m2))+
    #    geom_boxplot()
    #
    #ggplot(abg_biomass, aes(x=Trt, y=percent_live))+
    #    geom_boxplot()
    #
    #understorey_aboveground_c_pool$Trt[understorey_aboveground_c_pool$Ring%in%c(1,4,5)] <- "eCO2"
    #understorey_aboveground_c_pool$Trt[understorey_aboveground_c_pool$Ring%in%c(2,3,6)] <- "aCO2"
    #
    #ggplot(understorey_aboveground_c_pool, aes(x=Trt, y=Total_g_C_m2))+
    #    geom_boxplot()
    #
    #understorey_aboveground_c_pool_2$Trt[understorey_aboveground_c_pool_2$Ring%in%c(1,4,5)] <- "eCO2"
    #understorey_aboveground_c_pool_2$Trt[understorey_aboveground_c_pool_2$Ring%in%c(2,3,6)] <- "aCO2"
    #
    #ggplot(understorey_aboveground_c_pool_2, aes(x=Trt, y=Total_g_C_m2))+
    #    geom_boxplot()
    
    write.csv(out, "R_other/understorey_lai.csv", row.names=F)

    return(out)
}

