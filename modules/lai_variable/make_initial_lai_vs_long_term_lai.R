make_initial_lai_vs_long_term_lai <- function(lai_variable){

  i.lai <- lai_variable[lai_variable$Date == "2012-10-26",]
  l.lai <- summaryBy(lai_variable~Ring, FUN=mean, data=lai_variable, keep.names=T, na.rm=T)
  
  laiDF <- cbind(l.lai, i.lai$lai_variable)
  colnames(laiDF) <- c("Ring", "l.lai", "i.lai")
  laiDF$trt[laiDF$Ring%in%c(2,3,6)] <- "aCO2"
  laiDF$trt[laiDF$Ring%in%c(1,4,5)] <- "eCO2"
  
  #with(laiDF, plot(l.lai~i.lai))
  mod <- lm(l.lai~i.lai, data=laiDF)
  #summary(mod)$r.squared
  
  ### return
  return(mod)
}

