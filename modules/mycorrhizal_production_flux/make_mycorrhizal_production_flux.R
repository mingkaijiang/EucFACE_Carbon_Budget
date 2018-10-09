make_mycorrhizal_production_flux <- function(bk_density) {
    
    ### read in the df
    myDF <- read.csv("data/Mycorrhizae_data_JennifferW_experiment.csv")
    
    ### aggregate ring average bulk density
    bk <- aggregate(bk_density$bulk_density_kg_m3, by=list(bk_density$ring), mean, na.action=na.rm)
    colnames(bk) <- c("Ring", "bk")
    
    ### sand bulk density
    bk.sand <- 1200
    
    #### convert unit from % (or mg C per g sand) to mg C kg soil
    for (i in 1:6) {
        myDF[myDF$Ring == i, "bk"] <- bk[bk$Ring == i, "bk"]
    }
    
    ### Calculate bag volume
    myDF$Bag_volume <- myDF$Bag_dimension * 0.005
    
    ### calculate mycorrhizal biomass increment, in unit of g m-2 period -1
    #myDF$mycorrhizal_c_pool2 <- myDF$mgC /g_to_mg * myDF$grsand / myDF$Bag_volume * 0.1
    #myDF$mycorrhizal_c_pool <- myDF$percentC / 100 * myDF$bk * 0.1 / g_to_kg
    myDF$mycorrhizal_c_pool <- myDF$percentC / 100 * bk.sand * 0.1 / g_to_kg
    
    ### convert dates
    myDF$Start_date <- as.Date(as.character(myDF$Start_date), format="%d/%m/%Y")
    myDF$End_Date <- as.Date(as.character(myDF$End_Date), format="%d/%m/%Y")
    myDF$ndays <- as.numeric(myDF$End_Date - myDF$Start_date) + 1
    myDF$Date <- myDF$End_Date
    
    ### Calculate daily rate of mycorrhizal production in mg C m-2 d-1
    myDF$mycorrhizal_production <- myDF$mycorrhizal_c_pool / myDF$ndays * 1000
    
    ### Because mycorrhizal pool is a cumulative pool, can just subtract the last date to get a daily rate
    ### This rate should apply to all dates in this dataset
    myDF2 <- subset(myDF, Date=="2015-10-15")
    for (i in 1:6) {
        myDF$mycorrhizal_production[myDF$Ring==i] <- myDF2$mycorrhizal_production[myDF2$Ring==i]
    }
    
    outDF <- myDF[,c("Start_date","End_Date","Date", "Ring", "mycorrhizal_production")]
    colnames(outDF) <- c("Start_date","End_date","Date", "Ring", "mycorrhizal_production")
    outDF$ndays <- as.numeric(outDF$End_date - outDF$Start_date) + 1
    
    ### Add the start date onto the dataframe
    tmpDF <- data.frame(rep("2014-10-14", 6),rep("2014-10-14", 6),rep("2014-10-14", 6),
                        c(1:6), myDF2$mycorrhizal_production, rep(1,6))
    colnames(tmpDF) <- c("Start_date","End_date","Date", "Ring", "mycorrhizal_production", "ndays")
    tmpDF$Start_date <- as.Date(as.character(tmpDF$Start_date))
    tmpDF$End_date <- as.Date(as.character(tmpDF$End_date))
    tmpDF$Date <- as.Date(as.character(tmpDF$Date))
    
    out <- rbind(tmpDF, outDF)
    out$ndays <- 1
    
    return(out)
    
}