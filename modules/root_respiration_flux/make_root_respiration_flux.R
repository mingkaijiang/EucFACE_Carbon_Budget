
make_root_respiration_flux <- function(fr_pool, cr_pool){
  #### Estimate root respiration based on temperature-dependent function
  #### temperature data downloaded in soil reapiration module
  #### temperature function derived from Whole Tree Chamber experiment 3
  #### based on 1-year seedlings of Eucalyptus Tereticornis
  
  #- download soil temperature
  tempDF <- download_soil_temperature()
  
  ### read in temperature data
  tempDF$DateTime <- as.POSIXct(tempDF$DateTime,format="%Y-%m-%d %T",tz="GMT")
  tempDF$Date <- as.Date(tempDF$Date)
  
  ### only include period matches with Rsoil
  tempDF <- subset(tempDF, Date >= "2012-09-07")
  tempDF <- subset(tempDF, Date <= "2016-12-31")
  
  ### Get ring information
  tempDF$Ring <- sub("FACE_R", "", tempDF$Source)
  tempDF$Ring <- sub("_B1.*", "", tempDF$Ring)
  tempDF$Ring <- as.numeric(tempDF$Ring)  
  tempDF <- tempDF[order(tempDF$Date),]
  
  ### calculate mean soil temperature
  tempDF$T5_avg <- rowMeans(tempDF[,c("T5cm_1_Avg", "T5cm_2_Avg")], na.rm=T)
  
  ### extract useful columns
  tempDF2 <- tempDF[,c("DateTime", "Date", "Ring", "T5_avg")]
  
  ### remove unreasonable data points
  tempDF2$T5_avg[tempDF2$T5_avg <= 0] <- NA
  
  ### replace NA with row before
  tempDF2$T5_avg <- na.locf(tempDF2$T5_avg)

  ### 
  tempDF <- tempDF2
  
  ### get ring-average root biomass data
  fr_biomass <- summaryBy(fineroot_pool~Ring, data=fr_pool, keep.names=T, FUN=mean)
  cr_biomass <- summaryBy(coarse_root_pool~Ring, data=cr_pool, keep.names=T, FUN=mean)
  
  ### assign fr_biomass onto dataframe
  for (i in 1:6) {
      tempDF[tempDF$Ring == i, "fr_biomass"] <- fr_biomass[fr_biomass$Ring == i, "fineroot_pool"]
      tempDF[tempDF$Ring == i, "cr_biomass"] <- cr_biomass[cr_biomass$Ring == i, "coarse_root_pool"] * cr_at_top_soil
      
  }
  
  ### Calculate R root
  tempDF$Rfroot <- Rcoef_fr * Rbase ^ ((tempDF$T5_avg - 15) / 10) * tempDF$fr_biomass
  tempDF$Rcroot <- Rcoef_cr * Rbase ^ ((tempDF$T5_avg - 15) / 10) * tempDF$cr_biomass
  tempDF$Rroot <- tempDF$Rfroot + tempDF$Rcroot
  
  ### convert from nmol CO2 g-1 s-1 to mg C m-2 15min-1
  tempDF$Rroot_mg_m2 <- tempDF$Rroot*60*15*1e-9*12.01*1000

  #- average across dates and plots
  tempDF.out <- summaryBy(Rroot_mg_m2~Date+Ring,data=tempDF,FUN=sum,keep.names=T)
  
  names(tempDF.out) <- c("Start_date","Ring","root_respiration_flux")
  tempDF.out$End_date <- tempDF.out$Start_date
  tempDF.out$Ring <- as.numeric(tempDF.out$Ring)
  tempDF.out$Date <- tempDF.out$Start_date
  tempDF.out$ndays <- as.numeric(tempDF.out$End_date - tempDF.out$Start_date) + 1
  tempDF.out <- tempDF.out[,c("Start_date","End_date","Date","Ring","root_respiration_flux","ndays")]
  
  
  return(tempDF.out)
}