make_leaf_pool <- function(lai_variable, sla_variable){
  
  dfr <- merge(lai_variable, sla_variable, by=c("Date","Ring"), all.x=TRUE, all.y=FALSE)
  dfr$leaf_pool <- 1000 * dfr$lai_variable / dfr$sla_variable # units may be wrong
  
return(dfr[,c("Date","Ring","leaf_pool")])

}
