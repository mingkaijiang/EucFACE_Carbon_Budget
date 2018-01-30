make_leaf_pool <- function(lai_variable, sla_variable){
  
    # Use average SLA over campaigns
    SLA <- mean(sla_variable$sla_variable, na.rm=TRUE)
    
    dfr <- lai_variable[,c("Date","Ring")]
    dfr$leaf_pool <- c_fraction * lai_variable$lai_variable / (10^-4 * SLA)
    
    return(dfr[,c("Date","Ring","leaf_pool")])

}
