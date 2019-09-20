
make_root_respiration_flux_2 <- function(fr_pool, cr_pool){
  #### Estimate root respiration based on temperature-dependent function
  #### temperature data downloaded in soil respiration module
  #### temperature function derived from EucFACE
  #### based on NamJin's EucFACE data
  
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
  
  fr_biomass$fineroot_dw[fr_biomass$Ring == 1] <- fr_biomass$fineroot_pool[fr_biomass$Ring==1] / 0.426
  fr_biomass$fineroot_dw[fr_biomass$Ring == 2] <- fr_biomass$fineroot_pool[fr_biomass$Ring==2] / 0.413
  fr_biomass$fineroot_dw[fr_biomass$Ring == 3] <- fr_biomass$fineroot_pool[fr_biomass$Ring==3] / 0.399
  fr_biomass$fineroot_dw[fr_biomass$Ring == 4] <- fr_biomass$fineroot_pool[fr_biomass$Ring==4] / 0.415
  fr_biomass$fineroot_dw[fr_biomass$Ring == 5] <- fr_biomass$fineroot_pool[fr_biomass$Ring==5] / 0.42
  fr_biomass$fineroot_dw[fr_biomass$Ring == 6] <- fr_biomass$fineroot_pool[fr_biomass$Ring==6] / 0.401
  
  
  cr_biomass$coarse_root_dw <- cr_biomass$coarse_root_pool / c_fraction_croot
  
  ### calculate the proportion of coarseroot that is intermediate root, bole and coarseroot
  ir.frac <- 0.24/0.88
  cr.frac <- 0.29/0.88
  br.frac <- 0.35/0.88
  
  ### assign fr_biomass onto dataframe
  for (i in 1:6) {
      tempDF[tempDF$Ring == i, "fr_biomass"] <- fr_biomass[fr_biomass$Ring == i, "fineroot_dw"]
      tempDF[tempDF$Ring == i, "cr_biomass"] <- cr_biomass[cr_biomass$Ring == i, "coarse_root_dw"] * cr.frac * 0.3#cr_at_top_soil  
      tempDF[tempDF$Ring == i, "ir_biomass"] <- cr_biomass[cr_biomass$Ring == i, "coarse_root_dw"] * ir.frac * 0.3#cr_at_top_soil
      tempDF[tempDF$Ring == i, "br_biomass"] <- cr_biomass[cr_biomass$Ring == i, "coarse_root_dw"] * br.frac * 0.3#cr_at_top_soil
      
      
  }
  
  ### Calculate R root
  for (i in c(2,3,6)) {
    tempDF$a.fr <- 1.1265
    tempDF$b.fr <- 0.0455
    
    tempDF$a.cr <- 0.6472
    tempDF$b.cr <- 0.0702
  }
  
  for (i in c(1,4,5)) {
    tempDF$a.fr <- 1.2303
    tempDF$b.fr <- 0.0451
    
    tempDF$a.cr <- 0.8697
    tempDF$b.cr <- 0.0581
  }
  
  
  tempDF$Rfroot <- tempDF$a.fr * exp(tempDF$b.fr * tempDF$T5_avg) * tempDF$fr_biomass 
  tempDF$Rcroot1 <- tempDF$a.cr * exp(tempDF$b.cr * tempDF$T5_avg) * tempDF$cr_biomass 
  tempDF$Riroot <- tempDF$a.cr * exp(tempDF$b.cr * tempDF$T5_avg) * tempDF$ir_biomass 
  #tempDF$Rbroot <- tempDF$a.cr * exp(tempDF$b.cr * tempDF$T5_avg) * tempDF$br_biomass 
  tempDF$Rcroot <- with(tempDF, Rcroot1 + Riroot )#+ Rbroot)
  tempDF$Rroot <- tempDF$Rfroot + tempDF$Rcroot
  
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