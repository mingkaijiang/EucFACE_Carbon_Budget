calculate_annual_mean_met_data <- function(timestep) {
    
    ### Read input
    if (timestep == "Monthly") {
        DF1 <- read.csv("R_other/met_air_flux_data_monthly.csv")
        DF2 <- read.csv("R_other/rainfall_data_monthly.csv")

    } else if (timestep == "Daily") {
        DF1 <- read.csv("R_other/met_air_flux_data_daily.csv")
        DF2 <- read.csv("R_other/rainfall_data_daily.csv")
        
    } else if (timestep == "Hourly") {
        DF1 <- read.csv("R_other/met_air_flux_data_hourly.csv")
        DF2 <- read.csv("R_other/rainfall_data_hourly.csv")
        
    }
    
    ### Update date column names to be consistent
    names(DF1)[1] <- names(DF2)[1] <- "Date"
    myDF <- merge(DF1, DF2, by.x="Date", all=T)
    colnames(myDF) <- c("Date", "Tair", "RH", "PAR", "Pressure", "Wind", "Rain")
    myDF$Date <- as.Date(myDF$Date)
    myDF$Yr <- year(myDF$Date)
    
    ### only include 2013-2017, full year data
    myDF <- myDF[myDF$Yr >= 2013 & myDF$Yr <= 2017, ]
    
    ### Calculate annual means and sums
    prec <- summaryBy(Rain~Yr, data=myDF, FUN=sum, keep.names=T, na.rm=T)
    tair <- summaryBy(Tair~Yr, data=myDF, FUN=mean, keep.names=T, na.rm=T)
    
    outDF <- cbind(tair, prec$Rain)
    colnames(outDF) <- c("Yr", "Tair", "Rain")
    
    return(outDF)
}