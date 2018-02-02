make_understorey_respiration_flux <- function(c_pool,
                                              c_frac,
                                              assumption) {
    
    ### assumption options: 1: fixed - use the Rdmass value regardless of temperature and PAR
    ###                     2: q10 - a temperature dependent function
    
    ### This dataset contains dark respiration rate for
    ### Microlaena, obtained from Cumberlandplains by
    ### Leishman et al. (2010). Journal of Ecology, 98, 28-42.
    inDF <- read.csv("data/CPW_species_data.csv")
    
    ### select microlaena
    ### get a single Rdmass value, in unit of umol kg-1 s-1 of DW?
    ### aCO2 = 300 - 500 uL L-1, 
    ### CO2 reference at 375 ppm,
    ### relative humidity at 20-50%,
    ### leaf temperature at 20 C,
    ### PAR at 0 lmol m-2 s-1.
    rd.rate <- inDF[inDF$Species == "Microlaena stipoides", "Rdmass"]
    
    ### convert rd.rate from umol kg-1 s-1 to mg C g d-1
    rd <- - (rd.rate * 12/44 * 10^-6 * 24 * 3600)
    
    ### Calculate daily respiration rate
    if (assumption == "fixed") {
        ### Assume a fixed constant
        ### return in mg C m-2 d-1
        c_pool$respiration <- c_pool$Live_g_C_m2 / c_frac * rd
        
    } else if (assumption == "q10") {
        ### Assume a Q10 function with temperature
        
    }

    ### make it a flux to cover certain period of time
    date.list <- unique(as.character(c_pool$Date))
    s.date.list <- c("2014-08-01", date.list[1:length(date.list)-1])
    e.date.list <- date.list
    
    for (i in 1:length(date.list)) {
        c_pool[c_pool$Date == date.list[i], "Start_date"] <- s.date.list[i]
        c_pool[c_pool$Date == date.list[i], "End_date"] <- e.date.list[i]
    }
    
    c_pool$days <- as.numeric(as.Date(c_pool$End_date) - as.Date(c_pool$Start_date))
    
    out <- c_pool[, c("Date", "Start_date", "End_date", "Ring", "respiration", "days")]
    
    return(out)
    
    
}