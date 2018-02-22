#- "main" module function for wood respiration. Needs temperature, sapwood mass, and branch wood mass as inputs
make_wood_respiration_flux <- function(Tair,Sapwood,Branchwood){
  
  #- download the data
  download_wood_respiration()
  
  #- read in the stem respiration data, calculate mean rate of ambient data. 
  Rstem.dat <- read.csv("download/WTC_TEMP_CM_WTCFLUX-STEM_20140528_L1_v1.csv")
  Rstem <- mean(Rstem.dat[which(Rstem.dat$T_treatment=="ambient"),"R_stem_nmol"]) # units are nmol CO2 g-1 s-1 at 15 deg C
  
  #- read in the leaf and branch respiration data, calculate mean rate of ambient data
  Rbranch.dat1 <- read.csv("download/WTC_TEMP_CM_GX-RBRANCH_20140513-20140522_L1_v1.csv")
  Rbranch.dat <- subset(Rbranch.dat1, date == "2014-05-13") # ignore post-girdling data
  Rleaf <- mean(Rbranch.dat[which(Rbranch.dat$T_treatment=="ambient"),"Rleaf"]) # units are nmol CO2 g-1 s-1 at 15 deg C
  Rbranch <- mean(Rbranch.dat[which(Rbranch.dat$T_treatment=="ambient"),"Rbranch"]) # units are nmol CO2 g-1 s-1 at 15 deg C
  
  #- So, Rstem and Rbranch are tissue-specific rates of R (nmol CO2 g-1 s-1) at 15 deg C.
  #   These need to be scaled to the correct temperature and multiplied by mass.
  
  

}