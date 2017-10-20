#### Check wood, soil C and microbial C pool changes at the start and end of the experiment

plot_change_in_wood_soil_microbe_pools <- function(soilDF, woodDF, micrDF, destDir) {
    
    #### compare wood C and soil C at the start and end dates among rings
    
    ### add eCO2 and aCO2 ring information
    eCO2 <- c(1, 4, 5)
    aCO2 <- c(2, 3, 6)
    
    for (i in eCO2) {
        soilDF[soilDF$Ring == i, "Treatment"] <- "eCO2"
        woodDF[woodDF$Ring == i, "Treatment"] <- "eCO2"
        micrDF[micrDF$ring == i, "Treatment"] <- "eCO2"
        
    }
    
    for (i in aCO2) {
        soilDF[soilDF$Ring == i, "Treatment"] <- "aCO2"
        woodDF[woodDF$Ring == i, "Treatment"] <- "aCO2"
        micrDF[micrDF$ring == i, "Treatment"] <- "aCO2"
        
    }
    
    soilDF$Ring <- as.factor(soilDF$Ring)
    woodDF$Ring <- as.factor(woodDF$Ring)
    micrDF$ring <- as.factor(micrDF$ring)
    micrDF$Date <- as.Date(micrDF$date, format = "%d/%m/%Y")
    
    
    ### get start and end dates from both dataframes
    soil.s.date <- min(soilDF$Date)
    soil.e.date <- max(soilDF$Date)
    wood.s.date <- min(woodDF$Date)
    wood.e.date <- max(woodDF$Date)
    micr.s.date <- min(micrDF$Date)
    micr.e.date <- max(micrDF$Date)

    ### extract start and end dates
    soil.sub.DF <- subset(soilDF, Date == soil.s.date | Date == soil.e.date)
    wood.sub.DF <- subset(woodDF, Date == wood.s.date | Date == wood.e.date)
    micr.sub.DF <- subset(micrDF, Date == micr.s.date | Date == micr.e.date)
    
    ### Calulate changes in wood and soil C pools, start and end of measurement period
    soil.delta <- soil.sub.DF[soil.sub.DF$Date == soil.e.date, "soil_carbon_pool"] -
        soil.sub.DF[soil.sub.DF$Date == soil.s.date, "soil_carbon_pool"]
    soil.delta.DF <- soil.sub.DF[1:6, c(2,4)]
    soil.delta.DF$change_in_soil <- soil.delta
    
    wood.delta <- wood.sub.DF[wood.sub.DF$Date == wood.e.date, "wood_pool"] -
        wood.sub.DF[wood.sub.DF$Date == wood.s.date, "wood_pool"]
    wood.delta.DF <- wood.sub.DF[1:6, c(2,4)]
    wood.delta.DF$change_in_wood <- wood.delta
    
    micr.delta <- micr.sub.DF[micr.sub.DF$Date == micr.e.date, "Cmic_g_m2"] -
        micr.sub.DF[micr.sub.DF$Date == micr.s.date, "Cmic_g_m2"]
    ring <- c(1:6)
    micr.delta.DF <- data.frame(ring, micr.delta, c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2"))
    colnames(micr.delta.DF) <- c("Ring", "change_in_micr", "Treatment")
    
    ### calculate the relative change in each pool
    for (i in ring) {
        soil.delta.DF[soil.delta.DF$Ring == i, "baseline_soil"] <- soil.sub.DF[soil.sub.DF$Ring == i & soil.sub.DF$Date == soil.s.date, "soil_carbon_pool"]
        wood.delta.DF[wood.delta.DF$Ring == i, "baseline_wood"] <- wood.sub.DF[wood.sub.DF$Ring == i & wood.sub.DF$Date == wood.s.date, "wood_pool"]
        micr.delta.DF[micr.delta.DF$Ring == i, "baseline_micr"] <- micr.sub.DF[micr.sub.DF$ring == i & micr.sub.DF$Date == micr.s.date, "Cmic_g_m2"]
    }
    
    soil.delta.DF$percent_change <- soil.delta.DF$change_in_soil / soil.delta.DF$baseline_soil * 100
    wood.delta.DF$percent_change <- wood.delta.DF$change_in_wood / wood.delta.DF$baseline_wood * 100
    micr.delta.DF$percent_change <- micr.delta.DF$change_in_micr / micr.delta.DF$baseline_micr * 100
    
    

    ### Plotting
    outName <- paste0(destDir, "/Soil_wood_microbe_pool_comparison.pdf")
    pdf(outName)
    
    ## plot the comparison for soil
    #boxplot(soil_carbon_pool~Treatment*Date, data=soil.sub.DF,
    #        notch=FALSE, col=c("gold", "darkgreen"),
    #        main = "Soil Carbon pool", ylab = "Soil C [g C m-2]",
    #        xlab = "Date", names=c(paste0("                               ", soil.s.date), NA,
    #                               paste0("                               ", soil.e.date), NA))    
    # legend("topleft", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    stripchart(soil_carbon_pool~Treatment*Date, data=soil.sub.DF, vertical=T, 
               method="stack", pch=20, col=c("gold", "darkgreen"), cex=3,
               group.names=c(paste0("                               ", soil.s.date), NA,
                             paste0("                               ", soil.e.date), NA),
               ylab = "Soil C [g C m-2]", main = "Soil C by Treatment", xlab = "Date")
    legend("topleft", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    p1 <- ggplot(soil.sub.DF, aes(x=Ring, y=soil_carbon_pool, color=as.factor(Date))) +
        theme(legend.title=element_blank()) + 
        xlim("1", "2", "3", "4", "5", "6") +
        ylab("Soil C [g C m-2]") +
        geom_point(size=3) + 
        ggtitle("Soil C Pool by Ring")

    print(p1)
    
    ## plot the comparison for wood
    #boxplot(wood_pool~Treatment*Date, data=wood.sub.DF,
    #        notch=FALSE, col=c("gold", "darkgreen"),
    #        main = "Wood Carbon pool", ylab = "Wood C [g C m-2]",
    #        xlab = "Date", names=c(paste0("                               ", wood.s.date), NA,
    #                               paste0("                               ", wood.e.date), NA))    
    #legend("topleft", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    stripchart(wood_pool~Treatment*Date, data=wood.sub.DF, vertical=T, 
               method="stack", pch=20, col=c("gold", "darkgreen"), cex=3,
               group.names=c(paste0("                               ", wood.s.date), NA,
                             paste0("                               ", wood.e.date), NA),
               ylab = "Wood C [g C m-2]", main = "Wood C by Treatment", xlab = "Date")
    legend("topleft", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    p2 <- ggplot(wood.sub.DF, aes(x=Ring, y=wood_pool, color=as.factor(Date))) +
        theme(legend.title=element_blank()) + 
        xlim("1", "2", "3", "4", "5", "6") +
        ylab("Wood C [g C m-2]") +
        geom_point(size=3) + 
        ggtitle("Wood C Pool by Ring")
    
    print(p2)
    
    ## plot comparison for microbe
    stripchart(Cmic_g_m2~Treatment*Date, data=micr.sub.DF, vertical=T, 
               method="stack", pch=20, col=c("gold", "darkgreen"), cex=3,
               group.names=c(paste0("                               ", micr.s.date), NA,
                             paste0("                               ", micr.e.date), NA),
               ylab = "Microbial C [g C m-2]", main = "Microbial C by Treatment", xlab = "Date")
    legend("topleft", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    p3 <- ggplot(micr.sub.DF, aes(x=ring, y=Cmic_g_m2, color=as.factor(Date))) +
        theme(legend.title=element_blank()) + 
        xlim("1", "2", "3", "4", "5", "6") +
        ylab("Microbial C [g C m-2]") +
        geom_point(size=3) + 
        ggtitle("Microbial C Pool by Ring")
    
    print(p3)
    
    
    ## plot changes in soil pool
    #boxplot(change_in_soil~Treatment, data=soil.delta.DF,
    #        col=c("gold", "darkgreen"),
    #        main = paste0("Change in soil C ", soil.s.date, " to ", soil.e.date),
    #        xlab = "Treatment", names = c("aCO2", "eCO2"), ylab = "Soil C [g C m-2]")
    
    stripchart(change_in_soil~Treatment, data=soil.delta.DF, vertical=T, 
               method="stack", pch=20, col=c("gold", "darkgreen"), cex=3,
               ylab = "Change in Soil C [g C m-2]", main = "Change in Soil C by Treatment", xlab = "Treatment")
    legend("topright", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    p4 <- ggplot(soil.delta.DF, aes(x=Ring, y=change_in_soil)) +
        theme(legend.title=element_blank()) + 
        xlim("1", "2", "3", "4", "5", "6") +
        ylab("Soil C [g C m-2]") +
        geom_point(size=3) +
        ggtitle("Change in Soil C by Ring")
    
    print(p4)
    
    ## plot changes in wood pool
    # boxplot(change_in_wood~Treatment, data=wood.delta.DF,
    #        col=c("gold", "darkgreen"),
    #        main = paste0("Change in wood C ", wood.s.date, " to ", wood.e.date),
    #        xlab = "Treatment", names = c("aCO2", "eCO2"), ylab = "Wood C [g C m-2]")
    
    stripchart(change_in_wood~Treatment, data=wood.delta.DF, vertical=T, 
               method="stack", pch=20, col=c("gold", "darkgreen"), cex=3,
               ylab = "Change in Wood C [g C m-2]", main = "Change in Wood C by Treatment", xlab = "Treatment")
    legend("topright", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    p5 <- ggplot(wood.delta.DF, aes(x=Ring, y=change_in_wood)) +
        theme(legend.title=element_blank()) + 
        xlim("1", "2", "3", "4", "5", "6") +
        ylab("Wood C [g C m-2]") +
        geom_point(size=3) +
        ggtitle("Change in Wood C by Ring")
    
    print(p5)
    
    ## plot changes in microbial pool
    stripchart(change_in_micr~Treatment, data=micr.delta.DF, vertical=T, 
               method="stack", pch=20, col=c("gold", "darkgreen"), cex=3,
               ylab = "Change in Microbial C [g C m-2]", main = "Change in Microbial C by Treatment", xlab = "Treatment")
    legend("topright", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    p6 <- ggplot(micr.delta.DF, aes(x=Ring, y=change_in_micr)) +
        theme(legend.title=element_blank()) + 
        xlim("1", "2", "3", "4", "5", "6") +
        ylab("Microbial C [g C m-2]") +
        geom_point(size=3) +
        ggtitle("Change in Microbial C by Ring")
    
    print(p6)
    
    ## the relative change in soil pools
    stripchart(percent_change~Treatment, data=soil.delta.DF, vertical=T, 
               method="stack", pch=20, col=c("gold", "darkgreen"), cex=3,
               ylab = "Percent Change [%]", main = "Percent Change in Soil C by Treatment", xlab = "Treatment")
    legend("topright", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    ## the relative change in wood pools
    stripchart(percent_change~Treatment, data=wood.delta.DF, vertical=T, 
               method="stack", pch=20, col=c("gold", "darkgreen"), cex=3,
               ylab = "Percent Change [%]", main = "Percent Change in Wood C by Treatment", xlab = "Treatment")
    legend("topright", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    ## the relative change in microbial pools
    stripchart(percent_change~Treatment, data=micr.delta.DF, vertical=T, 
               method="stack", pch=20, col=c("gold", "darkgreen"), cex=3,
               ylab = "Percent Change [%]", main = "Percent Change in Microbial C by Treatment", xlab = "Treatment")
    legend("topright", c("aCO2", "eCO2"), fill=c("gold", "darkgreen"))
    
    dev.off()    
    
}
