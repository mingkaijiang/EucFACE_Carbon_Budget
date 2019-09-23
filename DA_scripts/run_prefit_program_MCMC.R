##### run prefit program to obtain prefit allocation parameters
########################################################################################
run_prefit_program_MCMC <- function(dist.type, obsDF, eco2DF,
                                    range.option) {
    
    ### Assign chain length for MCMC parameter fitting
    chainLength <- 5000
    
    ### step 1:
    ## this initial parameters explore prefit parameter space
    source("definitions/initialize_aCO2_prefit_parameters_wide.R")
    
    ### step 2:
    ### Run MCMC for prefit parameters - at aCO2 for each ring
    ## Ring 2
    step.size.aCO2 <- 0.006 
    pChain_aCO2_1 <- prefit_MCMC_model_fitting(params = prefit.params.aCO2.R2, 
                                               params.lower = prefit.params.aCO2.lower.R2,
                                               params.upper = prefit.params.aCO2.upper.R2,
                                               obs=obsDF[1,],
                                               chainLength=chainLength,
                                               dist.type=dist.type,
                                               step.size=step.size.aCO2)
    
    
    generate_most_likely_prefit_outcome(inDF=pChain_aCO2_1,
                                        obs=obsDF[1,])
    
    
    # Ring 3
    step.size.aCO2 <- 0.008 
    pChain_aCO2_2 <- prefit_MCMC_model_fitting(params = prefit.params.aCO2.R3, 
                                               params.lower = prefit.params.aCO2.lower.R3,
                                               params.upper = prefit.params.aCO2.upper.R3,
                                               obs=obsDF[2,],
                                               chainLength=chainLength,
                                               dist.type=dist.type,
                                               step.size=step.size.aCO2)
    
    generate_most_likely_prefit_outcome(inDF=pChain_aCO2_2,
                                        obs=obsDF[2,])
    
    
    # Ring 6
    step.size.aCO2 <- 0.008 
    pChain_aCO2_3 <- prefit_MCMC_model_fitting(params = prefit.params.aCO2.R6, 
                                               params.lower = prefit.params.aCO2.lower.R6,
                                               params.upper = prefit.params.aCO2.upper.R6,
                                               obs=obsDF[3,],
                                               chainLength=chainLength,
                                               dist.type=dist.type,
                                               step.size=step.size.aCO2)
    
    
    generate_most_likely_prefit_outcome(inDF=pChain_aCO2_3,
                                        obs=obsDF[3,])
    
    
    ########################################################################################
    ### step 3: 
    ### read in parameter space for eCO2 treatment
    source("definitions/initialize_eCO2_prefit_parameters_wide.R")
    
    
    ### step 4:
    ### fit the model with eCO2 parameter space to get parameter uncertainties
    # Ring 1
    step.size.eCO2 <- 0.0004
    pChain_eCO2_1 <- prefit_MCMC_model_fitting(params = prefit.params.eCO2.R1, 
                                               params.lower = prefit.params.eCO2.lower.R1,
                                               params.upper = prefit.params.eCO2.upper.R1,
                                               obs=eco2DF[1,],
                                               chainLength=chainLength,
                                               dist.type=dist.type,
                                               step.size=step.size.eCO2)
    
    generate_most_likely_prefit_outcome(inDF=pChain_eCO2_1,
                                        obs=eco2DF[1,])
    
    
    
    ## ring 4
    step.size.eCO2 <- 0.0004
    pChain_eCO2_2 <- prefit_MCMC_model_fitting(params = prefit.params.eCO2.R4, 
                                               params.lower = prefit.params.eCO2.lower.R4,
                                               params.upper = prefit.params.eCO2.upper.R4,
                                               obs=eco2DF[2,],
                                               chainLength=chainLength,
                                               dist.type=dist.type,
                                               step.size=step.size.eCO2)
    
    generate_most_likely_prefit_outcome(inDF=pChain_eCO2_2,
                                        obs=eco2DF[2,])
    
    
    
    
    # ring 5
    step.size.eCO2 <- 0.001
    pChain_eCO2_3 <- prefit_MCMC_model_fitting(params = prefit.params.eCO2.R5, 
                                               params.lower = prefit.params.eCO2.lower.R5,
                                               params.upper = prefit.params.eCO2.upper.R5,
                                               obs=eco2DF[3,],
                                               chainLength=chainLength,
                                               dist.type=dist.type,
                                               step.size=step.size.eCO2)
    
    generate_most_likely_prefit_outcome(inDF=pChain_eCO2_3,
                                        obs=eco2DF[3,])
    
    
    ########################################################################################
    outDF <- data.frame(c(1:6), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Ring", "alloc.leaf", "alloc.wood", "alloc.froot", "tau.leaf", "tau.froot", "tau.myco", "alloc.myco", 
                         "alloc.sd.leaf", "alloc.sd.wood", "alloc.sd.froot", "tau.sd.leaf", "tau.sd.froot", "tau.sd.myco", "alloc.sd.myco", 
                         "alloc.pos.leaf", "alloc.pos.wood", "alloc.pos.froot", "tau.pos.leaf", "tau.pos.froot", "tau.pos.myco", "alloc.pos.myco", 
                         "alloc.neg.leaf", "alloc.neg.wood", "alloc.neg.froot", "tau.neg.leaf", "tau.neg.froot", "tau.neg.myco", "alloc.neg.myco")
    
    ### ring 1
    outDF[outDF$Ring=="1", 2:(no.var+2)] <- colMeans(pChain_eCO2_1[, 1:(no.var+1)])
    outDF[outDF$Ring=="1", 9:15] <- apply(pChain_eCO2_1[, 1:(no.var+1)], 2, sd)
    
    ### ring 4
    outDF[outDF$Ring=="4", 2:(no.var+2)] <- colMeans(pChain_eCO2_2[, 1:(no.var+1)])
    outDF[outDF$Ring=="4", 9:15] <- apply(pChain_eCO2_2[, 1:(no.var+1)], 2, sd)
    
    ### ring 5
    outDF[outDF$Ring=="5", 2:(no.var+2)] <- colMeans(pChain_eCO2_3[, 1:(no.var+1)])
    outDF[outDF$Ring=="5", 9:15] <- apply(pChain_eCO2_3[, 1:(no.var+1)], 2, sd)
    
    ### ring 2
    outDF[outDF$Ring=="2", 2:(no.var+2)] <- colMeans(pChain_aCO2_1[, 1:(no.var+1)])
    outDF[outDF$Ring=="2", 9:15] <- apply(pChain_aCO2_1[, 1:(no.var+1)], 2, sd)
    
    ### ring 3
    outDF[outDF$Ring=="3", 2:(no.var+2)] <- colMeans(pChain_aCO2_2[, 1:(no.var+1)])
    outDF[outDF$Ring=="3", 9:15] <- apply(pChain_aCO2_2[, 1:(no.var+1)], 2, sd)
    
    ### ring 6
    outDF[outDF$Ring=="6", 2:(no.var+2)] <- colMeans(pChain_aCO2_3[, 1:(no.var+1)])
    outDF[outDF$Ring=="6", 9:15] <- apply(pChain_aCO2_3[, 1:(no.var+1)], 2, sd)
    
    
    ## assign range
    if (range.option == "sd") {
        outDF$alloc.pos.leaf <- outDF$alloc.leaf + outDF$alloc.sd.leaf
        outDF$alloc.neg.leaf <- outDF$alloc.leaf - outDF$alloc.sd.leaf
        
        outDF$alloc.pos.wood <- outDF$alloc.wood + outDF$alloc.sd.wood
        outDF$alloc.neg.wood <- outDF$alloc.wood - outDF$alloc.sd.wood
        
        outDF$alloc.pos.froot <- outDF$alloc.froot + outDF$alloc.sd.froot
        outDF$alloc.neg.froot <- outDF$alloc.froot - outDF$alloc.sd.froot
        
        outDF$alloc.pos.myco <- outDF$alloc.myco + outDF$alloc.sd.myco
        outDF$alloc.neg.myco <- outDF$alloc.myco - outDF$alloc.sd.myco
        
        
        outDF$tau.pos.leaf <- outDF$tau.leaf + outDF$tau.sd.leaf
        outDF$tau.neg.leaf <- outDF$tau.leaf - outDF$tau.sd.leaf
        
        outDF$tau.pos.froot <- outDF$tau.froot + outDF$tau.sd.froot
        outDF$tau.neg.froot <- outDF$tau.froot - outDF$tau.sd.froot
        
        outDF$tau.pos.myco <- outDF$tau.myco + outDF$tau.sd.myco
        outDF$tau.neg.myco <- outDF$tau.myco - outDF$tau.sd.myco
        
    } else if (range.option == "minmax") {
        print("not written yet")
    }
    
    return(outDF)
    
}



