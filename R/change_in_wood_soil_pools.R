#### Check wood and soil C pool changes at the start and end of the experiment

plot_change_in_wood_soil_pools <- function(soilDF, woodDF, destDir) {
    
    #### compare wood C and soil C at the start and end dates among rings
    
    ### add eCO2 and aCO2 ring information
    eCO2 <- c(1, 4, 5)
    aCO2 <- c(2, 3, 6)
    
    for (i in eCO2) {
        soilDF[soilDF$Ring == i, "Treatment"] <- "eCO2"
        woodDF[woodDF$Ring == i, "Treatment"] <- "eCO2"
    }
    
    for (i in aCO2) {
        soilDF[soilDF$Ring == i, "Treatment"] <- "aCO2"
        woodDF[woodDF$Ring == i, "Treatment"] <- "aCO2"
    }
    
    ### get start and end dates from both dataframes
    soil.s.date <- min(soilDF$Date)
    soil.e.date <- max(soilDF$Date)
    wood.s.date <- min(woodDF$Date)
    wood.e.date <- max(woodDF$Date)

    ### extract start and end dates
    soil.sub.DF <- subset(soilDF, Date == soil.s.date | Date == soil.e.date)
    wood.sub.DF <- subset(woodDF, Date == wood.s.date | Date == wood.e.date)
    
    ### Calulate changes in wood and soil C pools, start and end of measurement period
    soil.delta <- soil.sub.DF[soil.sub.DF$Date == soil.e.date, "soil_carbon_pool"] -
        soil.sub.DF[soil.sub.DF$Date == soil.s.date, "soil_carbon_pool"]
    soil.delta.DF <- soil.sub.DF[1:6, c(2,4)]
    soil.delta.DF$change_in_soil <- soil.delta
    
    wood.delta <- wood.sub.DF[wood.sub.DF$Date == wood.e.date, "wood_pool"] -
        wood.sub.DF[wood.sub.DF$Date == wood.s.date, "wood_pool"]
    wood.delta.DF <- wood.sub.DF[1:6, c(2,4)]
    wood.delta.DF$change_in_wood <- wood.delta
    
    
    ### Plotting
    outName <- paste0(destDir, "/Soil_wood_pool_comparison.pdf")
    pdf(outName)
    
    ## plot the comparison for soil
    boxplot(soil_carbon_pool~Date*Treatment, data=soil.sub.DF,
            notch=FALSE, col=c("gold", "darkgreen"),
            main = "Soil Carbon pool", ylab = "Soil C [g C m-2]",
            xlab = "Date", names=c(paste0("                               ", soil.s.date), NA,
                                   paste0("                               ", soil.e.date), NA))    
    legend("topleft", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    
    ## plot the comparison for wood
    boxplot(wood_pool~Date*Treatment, data=wood.sub.DF,
            notch=FALSE, col=c("gold", "darkgreen"),
            main = "Wood Carbon pool", ylab = "Wood C [g C m-2]",
            xlab = "Date", names=c(paste0("                               ", wood.s.date), NA,
                                   paste0("                               ", wood.e.date), NA))    
    legend("topleft", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    ## plot changes in soil pool
    boxplot(change_in_soil~Treatment, data=soil.delta.DF,
            col=c("gold", "darkgreen"),
            main = paste0("Change in soil C ", soil.s.date, " to ", soil.e.date),
            xlab = "Treatment", names = c("aCO2", "eCO2"), ylab = "Soil C [g C m-2]")
    
    ## plot changes in wood pool
    boxplot(change_in_wood~Treatment, data=wood.delta.DF,
            col=c("gold", "darkgreen"),
            main = paste0("Change in wood C ", wood.s.date, " to ", wood.e.date),
            xlab = "Treatment", names = c("aCO2", "eCO2"), ylab = "Wood C [g C m-2]")
    
    dev.off()    
    
}