make_intermediate_root_production_flux <- function(inDF) {
    

    ### obtain global coarseroot turnover rate from 
    ### Zhang and Wang, 2015. The decomposition of fine and coarse roots, global patterns and their controlling factors
    ### Scientific Reports, 5: 09940.
    gDF <- read.csv("data/support_data/Zhang_Wang_2015_Global_Coarseroot.csv")
    
    subDF <- subset(gDF, Size%in%c("coarse root", "coarse root 2-5 mm"))
    subDF <- subset(subDF, Life.form == "Evergreen broadleaf")
    
    tau.croot <- mean(subDF$K.value_yr.1_)
    
    ### assign tau to coarseroot DF
    inDF$tau <- tau.croot
    
    inDF$intermediate_root_production_flux <- with(inDF, intermediate_root_pool * tau) / 365 * 1000
    
    inDF$Start_date <- inDF$Date - 365 + 1
    
    inDF$length <- 365
    
    ### prepare output
    outDF <- inDF[,c("Start_date", "Date", "Date", "Ring", "intermediate_root_production_flux", "length")]
    colnames(outDF) <- c("Start_date", "End_date", "Date", "Ring", "intermediate_root_production_flux", "ndays")
    
    return(outDF)
    
}