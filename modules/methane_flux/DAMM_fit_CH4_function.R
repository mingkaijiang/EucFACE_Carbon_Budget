DAMM_fit_CH4_function <- function(params,
                                  soilT,
                                  soilM,
                                  flux,
                                  type="optim") {
    R <- 8.314472e-3 #kJ K-1 mol-1
    O2airfrac <- 0.209 #L O2 L-1 air
    BD <- 1.6 #bulk density of soil. 0.8 in Davidson
    PD <- 3.5 #particle density of soil. Weight of an individual soil particle (g/cm3). 
    porosity <- 1-BD/PD #total porosity
    CH4const <- 1.8 * 1e-6   # 1.8 uL of CH4 per L of air
    Dgas <- 1.67
    DCH4 <- 2.4
    Soildepth <- 10 #effective soil depth in cm
    akmch4 <- params[1]
    eakmch4 <- params[2]
    akmo2 <- params[3]
    eakmo2 <- params[4]
    avmaxch4 <- params[5]
    eavmaxch4 <- params[6]
    
    ### KMCH4
    kMCH4 <- akmch4 * exp(-eakmch4/(R*(soilT+273.15)))
    
    ### KMO2
    kMO2 <- akmo2 * exp(-eakmo2/(R*(soilT+273.15)))
    
    ### VmaxCH4
    Vmax <- avmaxch4 * exp(-eavmaxch4/(R*(soilT+273.15)))
    
    ### CH4
    CH4 <- DCH4 * CH4const * ((porosity - soilM)^(4/3))
    
    ### O2
    O2 <- Dgas * O2airfrac * ((porosity - soilM)^(4/3))
    
    ### CH4 / (kMCH4 + CH4)
    MMCH4 <- CH4 / (kMCH4+CH4)
    
    ### O2 / (kMO2 + O2)
    MMO2 <- O2 / (kMO2+O2)
    
    ### CH4 flux, in mgC cm-3 hr-1
    CH4flux <- Vmax * MMCH4 * MMO2
    
    ### Conversions
    # mgC m-2 hr-1
    areaCflux <- 10000*Soildepth*CH4flux 
    
    # umol CH4 m-2 s-1
    areaCflux2 <- areaCflux/1000/12*1e6/60/60 
    
    ### residual
    residual <- (areaCflux2-flux)^2
    residual_sum <- sum(residual)
    
    ### return
    if(type=="optim") return(residual_sum)
    if(type=="predict") return(areaCflux2)
}
