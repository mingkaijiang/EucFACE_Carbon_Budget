make_mycorrhizal_c_pool <- function(micDF) {
    
    ### calculate mycorrhizal biomass
    micDF$mycorrhizal_c_pool[micDF$Ring==1] <- micDF$microbial_pool[micDF$Ring==1] * 0.105
    micDF$mycorrhizal_c_pool[micDF$Ring==2] <- micDF$microbial_pool[micDF$Ring==2] * 0.115
    micDF$mycorrhizal_c_pool[micDF$Ring==3] <- micDF$microbial_pool[micDF$Ring==3] * 0.101
    micDF$mycorrhizal_c_pool[micDF$Ring==4] <- micDF$microbial_pool[micDF$Ring==4] * 0.11
    micDF$mycorrhizal_c_pool[micDF$Ring==5] <- micDF$microbial_pool[micDF$Ring==5] * 0.088
    micDF$mycorrhizal_c_pool[micDF$Ring==6] <- micDF$microbial_pool[micDF$Ring==6] * 0.128
    
    outDF <- micDF[,c("Date", "Ring", "mycorrhizal_c_pool")]
    
    # Only use data period 2012-2016
    outDF <- outDF[outDF$Date<="2016-12-31",]
    
    return(outDF)
    
}