make_intermediate_root_production_flux_2 <- function(inDF) {
    
    ### calculate delta pool
    ### extract start and end date
    s.date <- min(inDF$Date)
    e.date <- max(inDF$Date)
    
    ### date list
    d.list <- unique(inDF$Date)
    d.list <- d.list[order(d.list)]
    
    ### create delta df
    delta <- subset(inDF, Date != d.list[1])
    delta$Start_date <- delta$Date  
    
    #### calculate differences
    for (i in 1:length(delta$Date)) {
        delta$Start_date[i] <- d.list[which(d.list == delta$Date[i]) - 1]
        delta$prev_biom[i] <- inDF$intermediate_root_pool[inDF$Ring == delta$Ring[i] &
                                             as.numeric(inDF$Date-delta$Start_date[i])==0]
    }
    
    ### Length of period
    delta$length <- as.numeric(delta$Date - delta$Start_date)
    
    ### annualize the difference
    delta$diff_g_yr <- (delta$intermediate_root_pool - delta$prev_biom) / delta$length * 365
    
    ### obtain global intermediateroot turnover rate from 
    ### Zhang and Wang, 2015. The decomposition of fine and coarse roots, global patterns and their controlling factors
    ### Scientific Reports, 5: 09940.
    gDF <- read.csv("data/Zhang_Wang_2015_Global_Coarseroot.csv")
    
    subDF <- subset(gDF, Size%in%c("coarse root", "coarse root 2-5 mm"))
    subDF <- subset(subDF, Life.form == "Evergreen broadleaf")
    
    tau.croot <- mean(subDF$K.value_yr.1_)
    
    ### assign tau to coarseroot DF
    delta$tau <- tau.croot
    
    delta$influx <- with(delta, diff_g_yr + prev_biom * tau)
    
    ### calculate intermediateroot production as the difference over two periods
    delta$intermediate_root_production_flux <- delta$influx / 365 * 1000
    
    
    ### prepare output
    outDF <- delta[,c("Start_date", "Date", "Date", "Ring", "intermediate_root_production_flux", "length")]
    colnames(outDF) <- c("Start_date", "End_date", "Date", "Ring", "intermediate_root_production_flux", "ndays")
    
    return(outDF)
    
}