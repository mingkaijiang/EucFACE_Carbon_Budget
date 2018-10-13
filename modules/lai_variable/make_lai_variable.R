make_lai_variable <- function(){

  res <- download_lai_variable()
  
  res <- subset(res, select=c(Date, Ring, LAI))
  names(res)[3] <- "lai_variable"
  
  #- return a number for ring
  res$Ring <- as.numeric(res$Ring)
  
  # Only use data period 2012-2016
  res <- res[res$Date<="2016-12-31",]
  
  
  ### Decision on what to return
  return(res)

}

