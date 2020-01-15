make_lai_variable <- function(){
  
  ### read data from local repo
  res <- readTOA5("download/FACE_P0037_RA_GAPFRACLAI_OPEN_L2.dat")
  
  res <- subset(res, select=c(Date, Ring, LAI))
  names(res)[3] <- "lai_variable"
  
  ### return a number for ring
  res$Ring <- as.numeric(res$Ring)
  
  ### Only use data period 2012-2016
  res <- res[res$Date<="2016-12-31",]
  
  return(res)
  
}

