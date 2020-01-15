
make_root_respiration_flux <- function(froot, iroot, croot, rstem, stem){
  #### Estimate root respiration based on temperature-dependent function
  #### temperature data downloaded in soil respiration module
  #### temperature function derived from EucFACE

  ### convert from g C to c DM
  iroot$c_frac <- c_fraction_croot
  
  froot$c_frac[froot$Ring==1] <- 0.426
  froot$c_frac[froot$Ring==2] <- 0.413
  froot$c_frac[froot$Ring==3] <- 0.399
  froot$c_frac[froot$Ring==4] <- 0.415
  froot$c_frac[froot$Ring==5] <- 0.42
  froot$c_frac[froot$Ring==6] <- 0.401
  
  iroot$biomass <- iroot$intermediate_root_pool/iroot$c_frac
  froot$biomass <- froot$fineroot_pool/iroot$c_frac
  
  irDF <- summaryBy(biomass~Ring, data=iroot, FUN=mean, keep.names=T)
  frDF <- summaryBy(biomass~Ring, data=froot, FUN=mean, keep.names=T)
  

  ### read in tsoil
  tempDF <- read.csv("download/EucFACE_Tsoil.csv")

  ### assign fr_biomass onto dataframe
  for (i in 1:6) {
      tempDF[tempDF$Ring == i, "fr_biomass"] <- frDF[frDF$Ring == i, "biomass"]
      tempDF[tempDF$Ring == i, "ir_biomass"] <- irDF[irDF$Ring == i, "biomass"] 
  }
  
  ### Calculate R root
  tempDF$a.fr <- 1.1378
  tempDF$b.fr <- 0.0479
  
  tempDF$a.ir <- 0.9764
  tempDF$b.ir <- 0.0461
  
  
  tempDF$Rfroot <- tempDF$a.fr * exp(tempDF$b.fr * tempDF$T5_avg) * tempDF$fr_biomass 
  tempDF$Riroot <- tempDF$a.ir * exp(tempDF$b.ir * tempDF$T5_avg) * tempDF$ir_biomass 
  tempDF$Rroot <- tempDF$Rfroot + tempDF$Riroot
  
  ### convert from nmol CO2 g-1 s-1 to mg C m-2 15min-1
  tempDF$Rroot_mg_m2 <- tempDF$Rroot*60*15*1e-9*12.01*1000

  ### sum across dates and plots
  tempDF.out <- summaryBy(Rroot_mg_m2~Date+Ring,data=tempDF,FUN=sum,keep.names=T)
  
  names(tempDF.out) <- c("Start_date","Ring","root_respiration_flux")
  tempDF.out$Start_date <- as.Date(as.character(tempDF.out$Start_date))
  tempDF.out$End_date <- as.Date(as.character(tempDF.out$Start_date))
  tempDF.out$Ring <- as.numeric(tempDF.out$Ring)
  tempDF.out$Date <- tempDF.out$Start_date
  tempDF.out$ndays <- as.numeric(tempDF.out$End_date - tempDF.out$Start_date) + 1
  tempDF.out <- tempDF.out[,c("Start_date","End_date","Date","Ring","root_respiration_flux","ndays")]
  
  ### add coarse root respiration
  croot_resp <- calculate_coarse_root_respiration(stem=stem, croot=croot, rstem=rstem)
  
  croot_resp <- subset(croot_resp, Date >= "2013-01-01")
  
  tempDF <- merge(tempDF.out, croot_resp, by=c("Ring", "Date"))
  tempDF$total_root_respiration <- tempDF$coarse_root_respiration + tempDF$root_respiration_flux
  
  out <- tempDF[,c("Start_date","End_date","Date","Ring","total_root_respiration","ndays")]
  colnames(out) <- c("Start_date","End_date","Date","Ring","root_respiration_flux","ndays")
  
  return(out)
}