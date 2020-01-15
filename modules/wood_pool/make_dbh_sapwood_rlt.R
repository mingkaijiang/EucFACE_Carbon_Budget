make_dbh_sapwood_rlt <- function() {
    #### read in sapwood depth data
    myDF <- read.csv("data/EucFACE_data/FACE_P0018_R0_SAPWOOD_20130418.csv")
    
    myDF <- myDF[myDF$Sapwood.depth..mm.>0, ]
    
    #### Obtain a relationship between BDH (cm) and sapwood depth (mm)
    mod <- lm(Sapwood.depth..mm.~DBH..cm., data=myDF)
    
    ### out
    out <- c(coefficients(mod)[[1]], coefficients(mod)[[2]])
    
    return(out)
}