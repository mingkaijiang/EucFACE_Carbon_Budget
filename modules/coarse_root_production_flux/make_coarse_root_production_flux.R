make_coarse_root_production_flux <- function(cr_pool) {
    
    
    ### obtain global coarseroot turnover rate from 
    ### Zhang and Wang, 2015. The decomposition of fine and coarse roots, global patterns and their controlling factors
    ### Scientific Reports, 5: 09940.
    gDF <- read.csv("data/Zhang_Wang_2015_Global_Coarseroot.csv")
    
    subDF <- subset(gDF, Size%in%c("coarse root", "coarse root 2-5 mm"))
    subDF <- subset(subDF, Life.form == "Evergreen broadleaf")
    
    tau.croot <- mean(subDF$K.value_yr.1_)
    
    ### just calculate production over a year
    cr_pool$Date <- as.character(cr_pool$Date)
    eDF <- subset(cr_pool, Date == "2014-09-22")
    lDF <- subset(cr_pool, Date == "2015-09-23")
    
    eDF$late <- lDF$coarse_root_pool
    eDF$ndays <- as.numeric(as.Date("2015-09-23") - as.Date("2014-09-22")) + 1
    eDF$diff <- eDF$late - eDF$coarse_root_pool
    eDF$tau <- tau.croot
    
    eDF$influx <- with(eDF, diff + coarse_root_pool * tau)
    
    ### calculate coarseroot production as the difference over two periods
    eDF$coarse_root_production_flux <- eDF$influx / 365 * 1000
    
    
    ### prepare output
    outDF <- eDF[,c("Date", "Ring", "coarse_root_production_flux", "ndays")]
    outDF$End_date <- as.Date("2015-09-23")
    outDF$Start_date <- outDF$Date
    outDF <- outDF[,c("Start_date", "End_date", "Date", "Ring", "coarse_root_production_flux", "ndays")]
    
    return(outDF)
    
}