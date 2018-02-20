
make_root_respiration_flux <- function(fr_pool){
  #### Estimate root respiration based on temperature-dependent function
  #### temperature data downloaded in soil reapiration module
  #### temperature function derived from Whole Tree Chamber experiment 3
  #### based on 1-year seedlings of Eucalyptus Tereticornis
  
  ### read in temperature data
  tempDF <- as.data.frame(data.table::fread(file.path(getToPath(), 
                                  "FACE_P0031_RA_Rsoil-PROCESSED_20120902-20151030_L2_v1.csv")))
  tempDF$DateTime <- as.POSIXct(tempDF$DateTime,format="%Y-%m-%d %T",tz="GMT")
  tempDF$Date <- as.Date(tempDF$Date)
  
  ### get ring-average root biomass data
  fr_biomass <- summaryBy(fineroot_pool~Ring, data=fr_pool, keep.names=T, FUN=mean)
  
  ### assign fr_biomass onto dataframe
  for (i in 1:6) {
      tempDF[tempDF$ring == i, "fr_biomass"] <- fr_biomass[fr_biomass$Ring == i, "fineroot_pool"]
  }
  
  ### define temperature function
  #Rcoef <- 1.73 # nmol CO2 m-2 s-1
  #Rbase <- 2.26
  
  ### Calculate R root
  tempDF$Rroot <- Rcoef * Rbase ^ ((tempDF$Tsoil - 15) / 10) * tempDF$fr_biomass
  
  ### convert from nmol CO2 g-1 s-1 to mg C m-2 d-1
  tempDF$Rroot_mg_m2_d <- tempDF$Rroot*60*60*24*1e-9*12.01*1000

  #- average across dates and plots
  tempDF.out <- summaryBy(Rroot_mg_m2_d~Date+ring,data=tempDF,FUN=mean,keep.names=T)
  
  names(tempDF.out) <- c("Start_date","Ring","root_respiration_flux")
  tempDF.out$End_date <- tempDF.out$Start_date
  tempDF.out$Ring <- as.numeric(tempDF.out$Ring)
  tempDF.out$Date <- tempDF.out$Start_date
  tempDF.out$ndays <- as.numeric(tempDF.out$End_date - tempDF.out$Start_date)
  tempDF.out <- tempDF.out[,c("Start_date","End_date","Date","Ring","root_respiration_flux","ndays")]
  
  return(tempDF.out)
}