#- Make the soil bulk density variable
make_soil_bulk_density <- function() {
    # return ring-specific soil density data

    # download the data
    download_soil_bulk_density_data()
    
    df <- read.csv(file.path(getToPath(), 
                            "FACE_RA_P0088_BULKDENSITY_L1_20170914.csv"))

    df <- df[,1:7]
    names(df)[7] <- "bulk_density"
    df$bulk_density <- as.numeric(as.character(df$bulk_density))
    
    # average across rings and depths, unit: g/cm3
    df.m <- summaryBy(bulk_density~ring+Depth,
                      data=df,FUN=mean,keep.names=T,na.rm=T)
    
    # unit conversion: g/cm3 to kg/m3
    df.m$bulk_density_kg_m3 <- df.m$bulk_density * g_to_kg / cm3_to_m3
    
    # update variables to output
    df.out <- df.m[,c("ring", "Depth", "bulk_density_kg_m3")]
    
    # change factor names to match with soil factor names
    df.out$Depth <- plyr::revalue(df.out$Depth, c("0-10"="0-10cm", " 10-20"="10-20cm", "20-30"="20-30cm"))
    
    # obtain ring averaged soil bulk density (0 - 30 cm only)
    df.out <- subset(df.out, Depth != "30-42")
    df.out <- subset(df.out, Depth != "30-44")
    df.out <- subset(df.out, Depth != "30-55")
    df.out <- subset(df.out, Depth != "30-60")
    df.out <- subset(df.out, Depth != "60-88")
    
    return(df.out)
    
}
