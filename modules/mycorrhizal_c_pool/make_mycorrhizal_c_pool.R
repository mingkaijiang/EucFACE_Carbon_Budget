make_mycorrhizal_c_pool <- function(bk_density) {
    
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
    
    outDF <- myDF[,c("Date", "Ring", "mycorrhizal_c_pool")]
    
    return(outDF)
    
}