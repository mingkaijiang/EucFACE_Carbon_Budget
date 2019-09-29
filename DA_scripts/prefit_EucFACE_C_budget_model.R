prefit_EucFACE_C_budget_model <- function(params, 
                                          obs) {
  
  ######################################################################
  #### read in params and data
  ### params these are parameters we need to constrain
  alloc.leaf <- params[1]
  alloc.wood <- params[2]
  alloc.root <- params[3]
  alloc.myco <- 1 - alloc.leaf - alloc.root - alloc.wood
  
  tau.leaf <- params[4]
  tau.root <- params[5]
  tau.myco <- params[6]
  
  ### get total NPP
  NPP.tot <- obs$GPP.mean - obs$Ra.mean
  
  ### CUE
  CUE <- NPP.tot / obs$GPP.mean
  
  ### individual NPP fluxes
  NPP.leaf <- NPP.tot * alloc.leaf
  NPP.wood <- NPP.tot * alloc.wood
  NPP.root <- NPP.tot * alloc.root
  NPP.myco <- NPP.tot * alloc.myco
  
  
  ### Pools
  C.leaf <- obs$C.leaf.mean 
  C.wood <- obs$C.wood.mean 
  C.root <- obs$C.root.mean 
  C.myco <- obs$C.myco.mean 
  C.micr <- obs$C.micr.mean 
  C.soil <- obs$C.soil.mean 
  
  ### write equations for change in pools
  delta.C.leaf <- NPP.leaf - tau.leaf * C.leaf
  delta.C.root <- NPP.root - tau.root * C.root
  delta.C.myco <- NPP.myco - tau.myco * C.myco
  
  
  ### prepare output
  outDF <- data.frame(alloc.myco, obs$GPP.mean, NPP.tot, CUE,
                      NPP.leaf, NPP.wood, NPP.root, NPP.myco,
                      delta.C.leaf, delta.C.root, delta.C.myco)
  
  colnames(outDF) <- c("alloc.myco", "GPP", "NPP", "CUE",
                       "NPP.leaf", "NPP.wood", "NPP.root", "NPP.myco",
                       "delta.Cleaf", "delta.Croot", "delta.Cmyco")
  
  return(outDF)
  
}
