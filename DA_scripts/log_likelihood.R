log_likelihood <- function(obs, pred) {
    
    #browser()
    
    
    ### NPP
    logLi <- -0.5*((pred$NPP.leaf - obs$NPP.leaf.mean)/obs$NPP.leaf.sd)^2 - log(obs$NPP.leaf.sd) - log(2*pi)^0.5
    logLi <- logLi - 0.5*((pred$NPP.wood - obs$NPP.wood.mean)/obs$NPP.wood.sd)^2 - log(obs$NPP.wood.sd) - log(2*pi)^0.5
    logLi <- logLi - 0.5*((pred$NPP.root - obs$NPP.root.mean)/obs$NPP.root.sd)^2 - log(obs$NPP.root.sd) - log(2*pi)^0.5

    #### delta
    logLi <- logLi - 0.5*(abs((pred$delta.Cleaf - obs$delta.C.leaf.mean))/obs$delta.C.leaf.sd)^2 - log(obs$delta.C.leaf.sd) - log(2*pi)^0.5
    logLi <- logLi - 0.5*(abs((pred$delta.Croot - obs$delta.C.root.mean))/obs$delta.C.root.sd)^2 - log(obs$delta.C.root.sd) - log(2*pi)^0.5
    logLi <- logLi - 0.5*(abs((pred$delta.Cmyco - obs$delta.C.myco.mean))/obs$delta.C.myco.sd)^2 - log(obs$delta.C.myco.sd) - log(2*pi)^0.5
    logLi <- logLi - 0.5*(abs((pred$delta.Cmicr - obs$delta.C.micr.mean))/obs$delta.C.micr.sd)^2 - log(obs$delta.C.micr.sd) - log(2*pi)^0.5
    logLi <- logLi - 0.5*(abs((pred$delta.Csoil - obs$delta.C.soil.mean))/obs$delta.C.soil.sd)^2 - log(obs$delta.C.soil.sd) - log(2*pi)^0.5
    logLi <- logLi - 0.5*(abs((pred$delta.Cwood - obs$delta.C.wood.mean))/obs$delta.C.wood.sd)^2 - log(obs$delta.C.wood.sd) - log(2*pi)^0.5
    
    ### delta litter pool should be none
    logLi <- logLi - 0.5*(abs((pred$delta.Cag - 0.0))/50.0)^2 - log(50.0) - log(2*pi)^0.5
    logLi <- logLi - 0.5*(abs((pred$delta.Cbg - 0.0))/50.0)^2 - log(50.0) - log(2*pi)^0.5
    
    
    ### Rhet
    logLi <- logLi - 0.5*((pred$Rhet - obs$Rhet.mean)/obs$Rhet.sd)^2 - log(obs$Rhet.sd) - log(2*pi)^0.5
    
    sumlogLi <- sum(logLi)

    return(sumlogLi)
}

