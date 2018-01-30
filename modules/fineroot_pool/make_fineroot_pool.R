#- Make the fine root C pool
make_fineroot_pool <- function(c_frac){
  
  #- download the data
  download_fineroot_data()
  
  #- read in the csv
  frb1 <- read.csv(file.path(getToPath(), 
                             "FACE_P0083_RA_FR-BIOMASS_L1_20140201-20150915.csv"))
  frb1$Date <- as.Date(frb1$Date)
  names(frb1)[2] <- "Ring"
  names(frb1)[8] <- "frb_tot"
  
  #- average across rings and dates
  frb.m <- summaryBy(frb_tot~Date+Ring,data=frb1,FUN=mean,keep.names=T)
  
  #- convert to g C m-2. Use fine-root specific c_fraction from Juan.
  frb.m$fineroot_pool <- frb.m$frb_tot*c_frac
  
  #- format dataframe to return
  frb.out <- frb.m[,c("Date","Ring","fineroot_pool")]
  return(frb.out)
}