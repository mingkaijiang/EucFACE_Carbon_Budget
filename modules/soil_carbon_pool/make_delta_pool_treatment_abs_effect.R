make_delta_pool_treatment_abs_effect <- function(inDF, 
                                                 var.col) {

    ### create delta DF
    deltaDF <- make_delta_pool_function(inDF, var.col)
    colnames(deltaDF) <- c("Start_date", "End_date", "Date", "Ring", "predicted")
    
    return(deltaDF)
}
