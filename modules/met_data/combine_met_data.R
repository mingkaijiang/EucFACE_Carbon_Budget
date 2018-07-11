combine_met_data <- function(timestep) {
    
    ### Read input
    if (timestep == "Monthly") {
        DF1 <- read.csv("R_other/pressure_data_monthly.csv")
        DF2 <- read.csv("R_other/rainfall_data_monthly.csv")
        DF3 <- read.csv("R_other/wind_data_monthly.csv")
        DF4 <- read.csv("R_other/tair_rh_par_data_monthly.csv")

    } else if (timestep == "Daily") {
        DF1 <- read.csv("R_other/pressure_data_daily.csv")
        DF2 <- read.csv("R_other/rainfall_data_daily.csv")
        DF3 <- read.csv("R_other/wind_data_daily.csv")
        DF4 <- read.csv("R_other/tair_rh_par_data_daily.csv")
        
    } else if (timestep == "Hourly") {
        DF1 <- read.csv("R_other/pressure_data_hourly.csv")
        DF2 <- read.csv("R_other/rainfall_data_hourly.csv")
        DF3 <- read.csv("R_other/wind_data_hourly.csv")
        DF4 <- read.csv("R_other/tair_rh_par_data_hourly.csv")
        
    }
    
    
    return(outDF)
}