
make_soil_p_mineralization_flux <- function(bk_density) {
    
    # download the data
    download_soil_p_mineralization_data()
    
    ## read in data - extractable NP data
    myDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILMINERALISATION_L3_20120724-20140124.csv"))
    
    # average across rings, dates, and depths, unit: mg/kg/d 
    myDF1.m <- summaryBy(P_mineralisation~date+ring,data=myDF1,FUN=mean,keep.names=T,na.rm=T)
    

    # obtain ring averaged soil bulk density (0 - 10 cm only)
    bk_density <- subset(bk_density, Depth == "0-10cm")
    
    # add bulk density
    for (i in 1:6){
        myDF1.m[myDF1.m$ring == i, "bk_density"] <- bk_density[bk_density$ring == i, "bulk_density_kg_m3"] 
    }
    
    # from mg kg-1 d-1 to mg m-2 d-1
    myDF1.m$p_mineralization_mg_m2_d <- myDF1.m$P_mineralisation * myDF1.m$bk_density * 0.1 

    # output table
    myDF1.out <- myDF1.m[,c("date", "ring", "p_mineralization_mg_m2_d")]
    
    # Shun's GCB paper indicates that P mineralization rate was higher
    # under eCO2, but here it is lower. 
    # However, the exact texts were:
    # "eCO2 was also associated with faster nutrient turnover rates
    # in the first six months of the experiment,
    # with higher N (+175%) and P (+211%) mineralization rates compared to ambient rings, 
    # although this difference did not persist."
    # Hence, CO2 treatment effct is only there for the first few points! 
    
    return(myDF1.out[,1:3])
}