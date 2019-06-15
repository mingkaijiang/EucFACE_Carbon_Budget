understorey_production_check <- function() {
    #### This function compares understory biomass using two methods
    #### for each ring and during time that both methods have data;
    #### This is to show how big an effect of measurement difference is on understorey
    #### C estimates, given that some rings have shrubs in (3 rings).
    
    ### Calculate understorey pools
    m1 <- make_understorey_aboveground_c_pool(c_fraction)
    m2 <- make_understorey_aboveground_c_pool_2(c_fraction)
    
    ### add year month 
    m1$Year <- year(m1$Date)
    m2$Year <- year(m2$Date)
    
    m1$Month <- month(m1$Date)
    m2$Month <- month(m2$Date)
    
    ### out DF
    ym <- unique(m1$Date)
    ym <- ym[1:3]
    outDF <- data.frame(rep(ym, 6), NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Date", "Year", "Month", "Ring", "Clipping", "StereoCamera")
    outDF$Year <- year(outDF$Date)
    outDF$Month <- month(outDF$Date)
    outDF$Ring <- rep(c(1:6), each=3)
    
    ### fill the out DF
    for (i in unique(outDF$Year)) {
        for (j in unique(outDF$Month)) {
            for (k in 1:6) {
                outDF[outDF$Year == i & outDF$Month == j & outDF$Ring == k, "Clipping"] <- m1[m1$Year == i & m1$Month == j & m1$Ring == k, "Total_g_C_m2"]
                outDF[outDF$Year == i & outDF$Month == j & outDF$Ring == k, "StereoCamera"] <- m2[m2$Year == i & m2$Month == j & m2$Ring == k, "Total_g_C_m2"]
                
            }
        }
    }
    
    ### save the file
    write.csv(outDF, "R_other/understorey_C_pool_comparison_g_C_m2.csv", row.names=F)
    
    ### convert the data to plot DF
    plotDF <- rbind(outDF[,1:4], outDF[,1:4])
    plotDF$Method <- rep(c("Clipping", "StereoCamera"), each=nrow(outDF))
    plotDF$Biomass[1:nrow(outDF)] <- outDF[,"Clipping"]
    plotDF$Biomass[(nrow(outDF)+1):(nrow(outDF)*2)] <- outDF[,"StereoCamera"]
    
    p1DF <- subset(plotDF, Date == "2015-02-01")
    p2DF <- subset(plotDF, Date == "2015-08-01")
    p3DF <- subset(plotDF, Date == "2016-02-01")
    
    require(ggplot2)
    
    p1 <- ggplot(p1DF, aes(Ring, Biomass)) +   
        geom_bar(stat = "identity", aes(fill=Method),
                 position="dodge") +
        xlab("Ring") + ylab("Understorey C Pool (g C m-2)") +
        ggtitle("2015-02")
    
    p2 <- ggplot(p2DF, aes(Ring, Biomass)) +   
        geom_bar(stat = "identity", aes(fill=Method),
                 position="dodge") +
        xlab("Ring") + ylab("Understorey C Pool (g C m-2)") +
        ggtitle("2015-08")
    
    p3 <- ggplot(p3DF, aes(Ring, Biomass)) +   
        geom_bar(stat = "identity", aes(fill=Method),
                 position="dodge") +
        xlab("Ring") + ylab("Understorey C Pool (g C m-2)") +
        ggtitle("2016-02")
    
    pdf("R_other/Understorey_biomass_method_comparison_by_date.pdf")
        
    plot(p1)
    plot(p2)
    plot(p3)
    
   dev.off()
    
}