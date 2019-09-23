predict_final_output <- function(pChain, 
                                 obs, 
                                 return.option) {
    
    # Store the final parameter set values
    param.set = colMeans(pChain[, 1:no.var])
    param.SD = apply(pChain[ , 1:no.var], 2, sd)
    param.final = data.frame(matrix(ncol = (no.var)*2, nrow = 1))
    
    names(param.final) <- c("alloc.leaf", "alloc.wood", "alloc.froot",#"alloc.myco",
                            "tau.leaf", "tau.froot", "tau.myco", 
                            "tau.bg.lit", 
                            "tau.micr", "tau.soil", 
                            "C.ag.lit", "C.bg.lit",
                            "frac.myco", "frac.ag.lit", "frac.bg.lit", "frac.micr",
                            "alloc.leaf.sd", "alloc.wood.sd", "alloc.froot.sd",#"alloc.myco.sd",
                            "tau.leaf.sd", "tau.froot.sd", "tau.myco.sd",
                            "tau.bg.lit.sd", 
                            "tau.micr.sd", "tau.soil.sd", 
                            "C.ag.lit.sd", "C.bg.lit.sd",
                            "frac.myco.sd", "frac.ag.lit.sd", "frac.bg.lit.sd", "frac.micr.sd")
    
    param.final[,1:no.var] = param.set
    param.final[,(no.var+1):(no.var*2)] = param.SD
    
    param.set <- round(as.numeric(param.set),3)
    
    
    # Calculate final output set from the predicted parameter set
    output.final.set <- EucFACE_C_budget_model(params=param.set, 
                                                 obs=obs)
    
    if (return.option == "Check result") {
        print(param.final)
        print(paste0("Final predicted = ", output.final.set$Rhet))
    } else if (return.option == "Return final parameter") {
        return(param.final)
    } else {
        print("Dead end")
    }
    
    
}