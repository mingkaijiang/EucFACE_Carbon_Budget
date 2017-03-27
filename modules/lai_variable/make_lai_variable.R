make_lai_variable <- function(){

  res <- download_lai_variable()
  
  res <- subset(res, select=c(Date, Ring, LAI))
  names(res)[3] <- "lai_variable"
  
  #- return a number for ring
  res$Ring <- as.numeric(res$Ring)
  
  return(res)
}



download_lai_variable <- function(){
  
  downloadTOA5("FACE_P0037_RA_GAPFRACLAI_OPEN_L2.dat", quiet=TRUE)
  
}
