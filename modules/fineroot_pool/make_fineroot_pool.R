#- Make the fine root C pool
make_fineroot_pool <- function(){
  
  #- download the data
  download_fineroot_data()
  
  #- read in the csv
  frb1 <- read.csv(file.path(getToPath(), 
                             "FACE_P0083_RA_FR-BIOMASS_L1_20140201-20150915.csv"))
  frb1$Date <- as.Date(frb1$Date)
  names(frb1)[2] <- "Ring"
  names(frb1)[6] <- "frb_top"
  names(frb1)[7] <- "frb_bot"
  names(frb1)[8] <- "frb_tot"
  
  #- average across rings and dates
  frb.m <- summaryBy(frb_tot+frb_top+frb_bot~Date+Ring,data=frb1,FUN=mean,keep.names=T)
  
  #- assign carbon fraction to each ring
  frb.m$c_frac[frb.m$Ring==1] <- 0.426
  frb.m$c_frac[frb.m$Ring==2] <- 0.413
  frb.m$c_frac[frb.m$Ring==3] <- 0.399
  frb.m$c_frac[frb.m$Ring==4] <- 0.415
  frb.m$c_frac[frb.m$Ring==5] <- 0.42
  frb.m$c_frac[frb.m$Ring==6] <- 0.401
  
  #- convert to g C m-2. Use fine-root specific c_fraction from Juan.
  frb.m$fineroot_pool <- frb.m$frb_tot*frb.m$c_frac
  frb.m$fineroot_0_10_cm <- frb.m$frb_top*frb.m$c_frac
  frb.m$fineroot_10_30_cm <- frb.m$frb_bot*frb.m$c_frac
  
  
  #- format dataframe to return
  frb.out <- frb.m[,c("Date","Ring","fineroot_pool", "fineroot_0_10_cm", "fineroot_10_30_cm")]
  
  # Only use data period 2012-2016
  frb.out <- frb.out[frb.out$Date<="2016-12-31",]
  
  ### return
  return(frb.out)
  
}
