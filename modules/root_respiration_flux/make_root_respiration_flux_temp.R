
make_root_respiration_flux_temp <- function(fr_pool, cr_pool){
  #### Estimate root respiration based on temperature-dependent function
  #### temperature data downloaded in soil reapiration module
  #### Fine root respiration based on WTC 3
  #### Coarseroot respiration based on wood respiration 

  
  #- download soil temperature
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
  
  ### get ring-average root biomass data
  fr_biomass <- summaryBy(fineroot_pool~Ring, data=fr_pool, keep.names=T, FUN=mean)
  cr_biomass <- summaryBy(coarse_root_pool~Ring, data=cr_pool, keep.names=T, FUN=mean)
  
  ### assign fr_biomass onto dataframe
  for (i in 1:6) {
      tempDF[tempDF$Ring == i, "fr_biomass"] <- fr_biomass[fr_biomass$Ring == i, "fineroot_pool"]
      tempDF[tempDF$Ring == i, "cr_biomass"] <- cr_biomass[cr_biomass$Ring == i, "coarse_root_pool"] * cr_at_top_soil
  }
  
  ### Calculate R root
  tempDF$Rfroot <- (Rcoef_fr * Rbase ^ ((tempDF$T5_avg - 15) / 10)) * tempDF$fr_biomass
  tempDF$Rcroot <- (Rcoef_cr * Rbase ^ ((tempDF$T5_avg - 15) / 10)) * tempDF$cr_biomass

  tempDF$Rroot <- tempDF$Rfroot + tempDF$Rcroot
  
  hDF$a <- 0.1789
  hDF$b <- 0.1042
  
  a.factor <- 1/mean(c(0.82, 0.96, 0.94))  
  e.factor <- 1/mean(c(1.11, 1.02, 0.97))
  hDF$scale_factor[hDF$Ring%in%c(2,3,6)] <- a.factor
  hDF$scale_factor[hDF$Ring%in%c(1,4,5)] <- e.factor
  
  ### Calculate respiration rate (umol CO2 m-2 h-1)
  hDF$Resp <- hDF$a * exp(hDF$b * hDF$AirTC_1_Avg) * hDF$SA * 3600
  
  ### Convert unit from umol CO2 m-2 h-1 to mg C m-2 h-1
  hDF$Resp_mg <- hDF$Resp * 1e-6 * 12.01 * 1000 #* hDF$scale_factor
  
  
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