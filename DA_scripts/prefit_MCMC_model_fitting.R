prefit_MCMC_model_fitting <- function(params,
                                      params.lower,
                                      params.upper,
                                      obs,
                                      chainLength,
                                      dist.type,
                                      step.size) {
    
    ### Discard the first 10% iterations for Burn-IN in MCMC (According to Oijen, 2008)
    burn_in <- chainLength * 0.1 
    
    ### prepare output df
    pChain <- matrix(0, nrow=chainLength, ncol = no.var+4+12)
    
    ### prepare model aic and bic comparison DF
    k1 <- 2 # k = 2 for the usual AIC
    npar <- no.var # npar = total number of parameters in the fitted model
    k2 <- log(1) # n being the number of observations for the so-called BIC
    
    #browser()
    
    ### Defining the variance-covariance matrix for proposal generation
    vcov <- (step.size*(params.upper-params.lower))^2
    
    vcovProposal <-  vcov 
    
    
    ### Find the Prior probability density
    if (dist.type == "normal") {
        ### normal gaussian distribution
        prior.dist <- vector("list", no.var)
        for (i in 1:no.var) {
            # Prior normal gaussian distribution
            prior.dist[i] <- list(log(dnorm(params[i], (params.lower[i] + params.upper[i])/2, 
                                            (params.upper[i] - params.lower[i])/3))) 
        }
        logPrior0 <- sum(unlist(prior.dist))
        Prior0 <- exp(logPrior0)
        
    } else if (dist.type == "uniform") {
        ### alternative - uniform distribution
        logPrior0 <- sum(log(dunif(params, min=params.lower, max=params.upper)))
        Prior0 <- exp(logPrior0)
    }
    
    ### Run the model, with initial parameter settings
    ### return initial output
    out.init <- prefit_EucFACE_C_budget_model(params=params, 
                                              obs=obs)
    
    
    #### Calculate log likelihood of starting point of the chain
    logL0 <- prefit_log_likelihood(obs = obs, pred = out.init) 
    
    #browser()
    
    aic <- -2*logL0 + k1*npar
    bic <- -2*logL0 + k2*npar
    
    pChain[1,] <- c(params, logL0, as.numeric(out.init), Prior0, aic, bic)
    
    
    ### Calculating the next candidate parameter vector, 
    ### as a multivariate normal jump away from the current point
    for (z in (2 : chainLength)) {
        candidatepValues = c()
        
        for (i in 1:no.var) {
            candidatepValues[i] = rmvnorm(n=1, mean=params[i],
                                          sigma=diag(vcovProposal[i],1)) 
        }
        
        ### Reflected back to generate another candidate value
        reflectionFromMin = pmin( unlist(matrix(0,nrow=1,ncol=no.var)), 
                                  unlist(candidatepValues-params.lower) )
        reflectionFromMax = pmax( unlist(list(rep(0, 1))), 
                                  unlist(candidatepValues-params.upper) )
        candidatepValues = candidatepValues - 2 * reflectionFromMin - 2 * reflectionFromMax 
        
        
        ### Calculating the prior probability density for the candidate parameter vector
        if (all(candidatepValues>params.lower) && all(candidatepValues<params.upper)){
            
            if (dist.type == "normal") {
                uni.dist = vector("list", no.var)
                # Prior normal gaussian distribution
                for (i in 1:no.var) {
                    uni.dist[i] = list(log(dnorm(candidatepValues[i], 
                                                 (params.lower[i] + params.upper[i])/2, 
                                                 (params.upper[i] - params.lower[i])/3))) 
                }
                logPrior1 <- sum(unlist(uni.dist))
                Prior1 <- exp(logPrior1)
            } else if (dist.type == "uniform") {
                
                # alternative - uniform distribution
                logPrior1 <- sum(log(dunif(candidatepValues, min=params.lower, max=params.upper)))
                Prior1 <- exp(logPrior0)
            }
            
        } else {
            Prior1 <- 0
        }
        
        ### Calculating the outputs for the candidate parameter vector and then log likelihood
        if (Prior1 > 0) {
            
            out.cand <- prefit_EucFACE_C_budget_model(params=candidatepValues, 
                                                      obs=obs)
            
            #browser()
            
            # Calculate log likelihood
            logL1 <- prefit_log_likelihood(obs = obs, pred = out.cand) 
            
            # Calculating the logarithm of the Metropolis ratio
            logalpha <- (logPrior1+logL1) - (logPrior0+logL0) 
            
            # Accepting or rejecting the candidate vector
            if ( log(runif(1, min = 0, max =1)) < logalpha && candidatepValues[1] + candidatepValues[2] + candidatepValues[3] <= 1
                 && candidatepValues[1] >= 0 && candidatepValues[2] >= 0 && candidatepValues[3] >= 0) {
                
                params <- candidatepValues
                logPrior0 <- logPrior1
                logL0 <- logL1
                
                aic <- -2*logL1 + k1*npar
                bic <- -2*logL1 + k2*npar
            }
        }
        
        pChain[z,] <- c(params, as.numeric(out.cand),  logL0, Prior1, aic, bic)
    }
    
    
    ### Discard the first 10% iterations for Burn-IN in MCMC
    pChain <- pChain[(burn_in+1):nrow(pChain),]
    pChain <- as.data.frame(pChain)
    
    ### assign names
    names(pChain) <- c("alloc.leaf", "alloc.wood", "alloc.root",
                       "tau.leaf", "tau.root", "tau.myco", 
                       "alloc.myco", 
                       "GPP", "NPP", "CUE",
                       "NPP.leaf", "NPP.wood", "NPP.root", "NPP.myco", 
                       "delta.Cleaf", "delta.Croot", "delta.Cmyco", "delta.Cwood",
                       "logli", "Prior","aic", "bic")
    
    
    nAccepted <- length(unique(pChain[,1]))
    
    acceptance = (paste("Total accepted: ", nAccepted, 
                        "out of ", chainLength-burn_in, 
                        "candidates accepted ( = ",
                        round(100*nAccepted/chainLength), "%)"))
    print(acceptance)
    
    
    return(pChain)
}