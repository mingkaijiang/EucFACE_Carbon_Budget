make_understorey_respiration_flux <- function(c_pool,
                                              c_frac,
                                              gpp,
                                              assumption) {
    
    ### assumption options: 1: rd - use the Rdmass value regardless of temperature and PAR
    ###                     2: q10 - a temperature dependent function
    ###                     3: a fixed CUE approach
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
    if (assumption == "rd") {
        ### Assume a fixed constant
        ### return in mg C m-2 d-1
        c_pool$dark_respiration <- c_pool$Live_g_C_m2 / c_frac * rd
        
        ### Need a rdark to rday conversion coefficient
        c_pool$respiration <- c_pool$dark_respiration * 2.5
        
        ### make it a flux to cover certain period of time
        date.list <- unique(as.character(c_pool$Date))
        s.date.list <- c("2014-08-01", date.list[1:length(date.list)-1])
        e.date.list <- date.list
        
        for (i in 1:length(date.list)) {
            c_pool[c_pool$Date == date.list[i], "Start_date"] <- s.date.list[i]
            c_pool[c_pool$Date == date.list[i], "End_date"] <- e.date.list[i]
        }
        
        c_pool$ndays <- as.numeric(as.Date(c_pool$End_date) - as.Date(c_pool$Start_date)) + 1
        
        out <- c_pool[, c("Date", "Start_date", "End_date", "Ring", "respiration", "ndays")]
        
    } else if (assumption == "q10") {
        ### Assume a Q10 function with temperature

        
    } else if (assumption == "cue") {
        gpp$respiration <- gpp$GPP * under_cue
        gpp$Start_date <- paste0(gpp$year, "-01-01")
        gpp$End_date <- paste0(gpp$year, "-12-31")
        gpp$Date <- gpp$End_date
        gpp$respiration_mg_m2_d <- gpp$respiration / 365 * 1000
        
        out <- gpp[,c("Start_date", "End_date", "Date", "Ring", "respiration_mg_m2_d")]
        colnames(out) <- c("Start_date", "End_date", "Date", "Ring", "respiration")
        out$ndays <- as.numeric(as.Date(out$End_date) - as.Date(out$Start_date)) + 1
        
    } 
    
    out$Date <- as.Date(as.character(out$Date), "%Y-%m-%d")
    
    return(out)
    
}