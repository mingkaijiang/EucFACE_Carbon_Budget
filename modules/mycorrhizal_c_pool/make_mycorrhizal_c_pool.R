make_mycorrhizal_c_pool <- function(bk_density) {
    
    ### read in the df
    myDF <- read.csv("data/Mycorrhizae_data_JennifferW_experiment.csv")
    
    ### aggregate ring average bulk density
    bk <- aggregate(bk_density$bulk_density_kg_m3, by=list(bk_density$ring), mean, na.action=na.rm)
    colnames(bk) <- c("Ring", "bk")
    
    #### convert unit from % (or mg C per g sand) to mg C kg soil
    for (i in 1:6) {
        myDF[myDF$Ring == i, "bk"] <- bk[bk$Ring == i, "bk"]
    }
    
    ### calculate mycorrhizal biomass increment, in unit of g m-2 period -1
    myDF$mycorrhizal_c_pool <- myDF$percentC/100 * myDF$bk * 0.3 / g_to_kg * percent_sand
    
    ### convert dates
    myDF$Start_date <- as.Date(as.character(myDF$Start_date), format="%d/%m/%Y")
    myDF$End_Date <- as.Date(as.character(myDF$End_Date), format="%d/%m/%Y")
    myDF$ndays <- as.numeric(myDF$End_Date - myDF$Start_date) + 1
    myDF$Date <- myDF$End_Date
    
    outDF <- myDF[,c("Date", "Ring", "mycorrhizal_c_pool")]
    
    return(outDF)
    
}