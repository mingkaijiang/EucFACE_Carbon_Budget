make_treatment_effect_df_2 <- function(inDF) {
    ### This function processes input variable by treatment
    ### so that they are in format for plotting
    ### inDF: the input variable
    ### v: the column number of the variable flux/pool
    ### This function simply add treatment labels to dataframe

    ac <- c(2,3,6)
    ec <- c(1,4,5)
    
    inDF$Treatment <- NA
    
    for (i in ac) {
        inDF[inDF$Ring == i, "Treatment"] <- "aCO2"
    } 
    
    for (i in ec) {
        inDF[inDF$Ring == i, "Treatment"] <- "eCO2"
    } 
    
    tr <- inDF

    ### return
    return(tr)
}