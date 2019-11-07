calculate_coarse_root_respiration <- function(stem, croot, rstem) {
    
    # add year information
    stem$year <- year(stem$Date)
    croot$year <- year(croot$Date)
    rstem$year <- year(rstem$Date)
    
    rstem <- merge(rstem, stem, by=c("year", "Ring"))
    rstem <- merge(rstem, croot, by=c("year", "Ring"))
    
    # calculate mass of croot to stem
    rstem$frac <- rstem$coarse_root_pool / rstem$wood_pool
    
    # calculate rroot based on fraction
    rstem$coarse_root_respiration <- rstem$wood_respiration * rstem$frac
    
    out <- rstem[,c("Ring", "Date.x", 
                     "coarse_root_respiration")]
    colnames(out) <- c("Ring", "Date", "coarse_root_respiration")
    
    return(out)
    
}