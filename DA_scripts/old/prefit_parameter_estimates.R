prefit_parameter_estimates <- function() {
    
    obs <- eco2DF[4,]
    params <- params.eCO2
    
    fit <- optim(params, EucFACE_C_budget_model_prefit, method="BFGS",hessian=T)
    fit$par
    
    params.lower <- params.eCO2.lower
    params.upper <- params.eCO2.upper
    
    fitted <- Nelder_Mead(EucFACE_C_budget_model_prefit, 
                          fit$par, 
                          lower=params.lower, 
                          upper=params.upper)
    
    fitted$feval
    fitted$par
    
    
}