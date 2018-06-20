# The Dual Arrhenious Michaelis-Menten model (DAMM). See Davidson et al. 2012 GCB.
# This version of the DAMM model can be used for parameter optimization (type="optim")
#   or for prediction given set of provided parameters (type="predict").
# Uses a differntial evolution optimization function (DEoptim).
# Accepts a vector of 4 parameters, soil temperature (deg C), and soil moisture (%)
DAMM_optim_CH4 <- function(par,soilT,soilM,flux,type="optim"){ 
  AlphaCH4 <- par[1]
  EaCH4 <- par[2]
  kMCH4 <- par[3]
  kMO2 <- par[4]
  R <- 8.314472e-3 #kJ K-1 mol-1
  O2airfrac <- 0.209 #L O2 L-1 air
  BD <- 1.6 #bulk density of soil. 0.8 in Davidson
  PD <- 3.5 #particle density of soil. Weight of an individual soil particle (g/cm3). 2.66 is a reasonable average.
  porosity <- 1-BD/PD #total porosity
  CH4const <- 1.8 * 1e-6   # 1.8 uL of CH4 per L of air
  Dgas <- 1.67
  DCH4 <- 2.4
  Soildepth <- 10 #effective soil depth in cm
  
  areaCflux2 <- rep(0,length(soilT))
  residual <- rep(0,length(soilT))
  
  ### CH4
  CH4 <- DCH4 * CH4const * ((porosity - soilM)^(4/3))
  
  ### O2
  O2 <- Dgas * O2airfrac * ((porosity - soilM)^(4/3))
  
  ### CH4 / (kMCH4 + CH4)
  MMCH4 <- CH4 / (kMCH4+CH4)
  
  ### O2 / (kMO2 + O2)
  MMO2 <- O2 / (kMO2+O2)
  
  ### Vmax for CH4
  Vmax <- AlphaCH4 * exp(-EaCH4/(R*(soilT+273.15)))
  
  ### CH4 flux
  CH4flux <- Vmax * MMCH4 * MMO2
  
  ### Conversions
  areaCflux <- 10000*Soildepth*CH4flux #in mgC m-2 hr-1
  areaCflux2 <- areaCflux/1000/12*1e6/60/60 #convert to umol CH4 m-2 s-1
  residual <- (areaCflux-flux)^2
  
  
  residual_sum <- sum(residual, na.rm=T)
  if(type=="optim") return(residual_sum)
  if(type=="predict") return(areaCflux2)
  
}