Johns_curve_fitting <- function() {
    
    set.seed(1234) # set seed for repeatablility
    
    
    
    #parameters
    AlphaSx <- 8e9      #mgC cm-3 hr-1. 5.38e10 in Davidson
    EaSx <- 70          #kJ mol-1. 72.26 in Davidson
    kMSx <-  5e-9        #gC cm-3 soil. 9.95e-7 in Davidson
    kMO2 <- 0.121 #0.121 in Davidson et al. (2012)
    par <- c(AlphaSx,EaSx,kMSx,kMO2)
    lower <- c(1e8,20,1e-9,0.0004)
    upper <- c(1e12,80,1e-5,0.5)
    
    ########################################################################################
    #fit each ring separately using DEoptim
    #preallocate list to catch the results
    #this takes a rather long time, but much less after vectorising the objective funciton
    out.ring <- as.list(1:6)
    #loop over each ring
    for (i in 1:length(nls.ring)){
        out <- DEoptim(fn=DAMM_optim,lower=lower,upper=upper,
                       control=list(NP=500,itermax=50,trace=TRUE,CR=0.9),
                       soilT=nls.ring[[i]]$Tsoil, soilM=nls.ring[[i]]$theta,flux=nls.ring[[i]]$Rsoil)
        out.ring[[i]] <- unname(out$optim$bestmem)
        
    }
    ########################################################################################
    
}
