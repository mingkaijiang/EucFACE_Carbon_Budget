
make_root_respiration_flux_2 <- function(froot, iroot){
  #### Estimate root respiration based on temperature-dependent function
  #### temperature data downloaded in soil respiration module
  #### temperature function derived from EucFACE
  #### based on NamJin's EucFACE data

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
  
  ### download soil temperature
  tempDF <- download_soil_temperature()
  
  ### read in temperature data
  tempDF$DateTime <- as.POSIXct(tempDF$DateTime,format="%Y-%m-%d %T",tz="GMT")
  tempDF$Date <- as.Date(tempDF$Date)
  
  ### only include period matches with Rsoil
  tempDF <- subset(tempDF, Date >= "2013-01-01")
  tempDF <- subset(tempDF, Date <= "2016-12-31")
  
  ### Get ring information
  tempDF$Ring <- sub("FACE_R", "", tempDF$Source)
  tempDF$Ring <- sub("_B1.*", "", tempDF$Ring)
  tempDF$Ring <- as.numeric(tempDF$Ring)  
  tempDF <- tempDF[order(tempDF$Date),]
  
  ### calculate mean soil temperature
  tempDF$T5_avg <- rowMeans(tempDF[,c("T20cm_1_Avg", "T20cm_2_Avg")], na.rm=T)

  ### extract useful columns
  tempDF2 <- tempDF[,c("DateTime", "Date", "Ring", "T5_avg")]
  
  ### remove unreasonable data points
  tempDF2$T5_avg[tempDF2$T5_avg <= 0] <- NA
  
  ### replace NA with row before
  tempDF2$T5_avg <- na.locf(tempDF2$T5_avg)

  ### 
  tempDF <- tempDF2

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
  tempDF.out$End_date <- tempDF.out$Start_date
  tempDF.out$Ring <- as.numeric(tempDF.out$Ring)
  tempDF.out$Date <- tempDF.out$Start_date
  tempDF.out$ndays <- as.numeric(tempDF.out$End_date - tempDF.out$Start_date) + 1
  tempDF.out <- tempDF.out[,c("Start_date","End_date","Date","Ring","root_respiration_flux","ndays")]
  

  return(tempDF.out)
}