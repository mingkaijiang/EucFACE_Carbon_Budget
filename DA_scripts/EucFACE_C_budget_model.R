EucFACE_C_budget_model <- function(params, 
                                   obs) {
  
  ######################################################################
  #### read in params and data
  ### params these are parameters we need to constrain
  alloc.leaf <- params[1]
  alloc.wood <- params[2]
  alloc.root <- params[3]
  alloc.myco <- 1.0 - alloc.leaf - alloc.wood - alloc.root
  
  tau.leaf <- params[4]
  tau.root <- params[5]
  tau.myco <- params[6]
  
  tau.ag.lit <- obs$tau.ag.lit.mean
  
  tau.bg.lit <- params[7]
  tau.micr <- params[8] 
  tau.soil <- params[9]
  
  C.ag.lit <- params[10]
  C.bg.lit <- params[11]
  
  frac.ag <- params[12]
  frac.bg <- params[13]
  frac.micr <- params[14]
  
  ### get total NPP
  NPP.tot <- obs$GPP.mean - obs$Ra.mean
  
  ### CUE
  CUE <- NPP.tot / obs$GPP.mean
  
  ### individual NPP fluxes
  NPP.leaf <- NPP.tot * alloc.leaf
  NPP.wood <- NPP.tot * alloc.wood
  NPP.root <- NPP.tot * alloc.root
  NPP.myco <- NPP.tot * alloc.myco
  
  #browser()
  
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
  
  delta.C.ag.lit <- tau.leaf * C.leaf - tau.ag.lit * C.ag.lit
  
  ### this is unconstrained
  delta.C.bg.lit <- tau.myco + C.myco + tau.root * C.root - tau.bg.lit * C.bg.lit
  
  delta.C.micr <- frac.ag * tau.ag.lit * C.ag.lit + frac.bg * tau.bg.lit * C.bg.lit - tau.micr * C.micr
  
  delta.C.soil <- frac.micr * tau.micr * C.micr - tau.soil * C.soil
  
  #browser()
  
  ### total Rhet
  Rhet <- round(C.ag.lit * (1 - frac.ag) * tau.ag.lit +
                  C.bg.lit * (1 - frac.bg) * tau.bg.lit +
                  C.micr * (1 - frac.micr) * tau.micr +
                  C.soil * tau.soil, 2)
  
  #browser()
  
  ### prepare output
  outDF <- data.frame(obs$GPP.mean, NPP.tot, CUE,
                      NPP.leaf, NPP.wood, NPP.root, NPP.myco,
                      delta.C.leaf, delta.C.root, delta.C.myco, 
                      delta.C.ag.lit, delta.C.bg.lit, 
                      delta.C.micr, delta.C.soil, Rhet)
  
  colnames(outDF) <- c("GPP", "NPP", "CUE",
                       "NPP.leaf", "NPP.wood", "NPP.root", "NPP.myco",
                       "delta.Cleaf", "delta.Croot", "delta.Cmyco", 
                       "delta.Cag", "delta.Cbg",
                       "delta.Cmicr", "delta.Csoil", "Rhet")
  
  return(outDF)
  
}
