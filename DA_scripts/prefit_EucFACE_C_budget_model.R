prefit_EucFACE_C_budget_model <- function(params, 
                                          obs) {
  
  ######################################################################
  #### read in params and data
  ### params these are parameters we need to constrain
  alloc.leaf <- params[1]
  alloc.wood <- params[2]
  alloc.froot <- params[3]
  alloc.myco <- 1 - alloc.leaf - alloc.froot - alloc.wood
  
  tau.leaf <- params[4]
  tau.froot <- params[5]
  tau.myco <- params[6]
  
  ### get total NPP
  NPP.tot <- obs$GPP.mean - obs$Ra.mean
  
  ### CUE
  CUE <- NPP.tot / obs$GPP.mean
  
  ### individual NPP fluxes
  NPP.leaf <- NPP.tot * alloc.leaf
  NPP.wood <- NPP.tot * alloc.wood
  NPP.froot <- NPP.tot * alloc.froot
  NPP.myco <- NPP.tot * alloc.myco
  
  
  ### Pools
  C.leaf <- obs$C.leaf.mean 
  C.wood <- obs$C.wood.mean 
  C.froot <- obs$C.froot.mean 
  C.myco <- obs$C.myco.mean 
  C.micr <- obs$C.micr.mean 
  C.soil <- obs$C.soil.mean 
  
  ### write equations for change in pools
  delta.C.leaf <- NPP.leaf - tau.leaf * C.leaf
  delta.C.froot <- NPP.froot - tau.froot * C.froot
  delta.C.myco <- NPP.myco - tau.myco * C.myco
  
  
  ### prepare output
  outDF <- data.frame(alloc.myco, obs$GPP.mean, NPP.tot, CUE,
                      NPP.leaf, NPP.wood, NPP.froot, NPP.myco,
                      delta.C.leaf, delta.C.froot, delta.C.myco)
  
  colnames(outDF) <- c("alloc.myco", "GPP", "NPP", "CUE",
                       "NPP.leaf", "NPP.wood", "NPP.froot", "NPP.myco",
                       "delta.Cleaf", "delta.Cfroot", "delta.Cmyco")
  
  return(outDF)
  
}
