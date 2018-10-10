# The Dual Arrhenious Michaelis-Menten model (DAMM). See Davidson et al. 2012 GCB.
# This version of the DAMM model can be used for parameter optimization (type="optim")
#   or for prediction given set of provided parameters (type="predict").
# Uses a differntial evolution optimization function (DEoptim).
# Accepts a vector of 4 parameters, soil temperature (deg C), and soil moisture (%)
DAMM_optim <- function(par,soilT,soilM,flux,type="optim"){ 
  AlphaSx <- par[1]
  EaSx <- par[2]
  kMSx <- par[3]
  kMO2 <- par[4]
  R <- 8.314472e-3 #kJ K-1 mol-1
  O2airfrac <- 0.209 #L O2 L-1 air
  BD <- 0.8 #EucFACE 1.6, bulk density of soil. 0.8 in Davidson
  PD <- 2.66 #EucFACE 3.5, particle density of soil. Weight of an individual soil particle (g/cm3). 2.66 is a reasonable average.
  porosity <- 1-BD/PD #total porosity
  Sxtot <- 0.0125 # C content (g/cm3)
  psx <- 4.14e-4 # EucFACE 2.4e-2, fraction of available C, 4.14e-4 in Davidson.
  Dliq <- 3.17
  Dgas <- 1.67
  #kMO2 <- 0.121 #0.121 in Davidson et al. (2012)
  Soildepth <- 10 #effective soil depth in cm
  
  areaCflux2 <- rep(0,length(soilT))
  residual <- rep(0,length(soilT))
  
  Sx <- Sxtot*psx*Dliq*(soilM)^3
  O2 <- Dgas*O2airfrac*((porosity - soilM)^(4/3))
  MMSx <- Sx/(kMSx+Sx)
  MMO2 <- O2/(kMO2+O2)
  VmaxSx <- AlphaSx*exp(-EaSx/(R*(soilT+273.15)))
  Resp <- VmaxSx*MMSx*MMO2
  areaCflux <- 10000*Soildepth*Resp #in mgC m-2 hr-1
  areaCflux2 <- areaCflux/1000/12*1e6/60/60 #convert to umol CO2 m-2 s-1
  residual <- (areaCflux2-flux)^2
  
  
  residual_sum <- sum(residual)
  if(type=="optim") return(residual_sum)
  if(type=="predict") return(areaCflux2)
  
}