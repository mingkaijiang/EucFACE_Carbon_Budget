generate_most_likely_prefit_outcome <- function(inDF, obs) {
    
    ### prepare output
    outDF <- inDF[3,]
    ncol <- ncol(outDF)
    
    for (i in 1:ncol) {
        myden <- density(inDF[,i])
        outDF[1,i] <- myden$x[which.max(myden$y)]
        
        outDF[2,i] <- mean(inDF[,i])
        
        outDF[3,i] <- median(inDF[,i])
    }
    
    
    
    ### delete unused columns
    outDF$logli <- NULL
    outDF$Prior <- NULL
    outDF$aic <- NULL
    outDF$bic <- NULL
    
    outDF <- rbind(outDF, 1)
    outDF$Cat <- c("Pred_pdf", 
                   "Pred_mean",
                   "Pred_median",
                   "Observed")
    
    ### assign observed values
    outDF[outDF$Cat=="Observed", 1:(no.var+1)] <- NA
    
    outDF$GPP[outDF$Cat=="Observed"] <- obs$GPP.mean 
    
    
    outDF$NPP.leaf[outDF$Cat=="Observed"] <- obs$NPP.leaf.mean
    outDF$NPP.wood[outDF$Cat=="Observed"] <- obs$NPP.wood.mean
    outDF$NPP.froot[outDF$Cat=="Observed"] <- obs$NPP.froot.mean
    outDF$NPP.myco[outDF$Cat=="Observed"] <- obs$GPP.mean - obs$Ra.mean - obs$NPP.leaf.mean - obs$NPP.wood.mean - obs$NPP.froot.mean
    outDF$NPP[outDF$Cat=="Observed"] <- outDF$NPP.leaf[outDF$Cat=="Observed"] + outDF$NPP.wood[outDF$Cat=="Observed"] + outDF$NPP.froot[outDF$Cat=="Observed"] + outDF$NPP.myco[outDF$Cat=="Observed"]
    
    outDF$delta.Cleaf[outDF$Cat=="Observed"] <- obs$delta.C.leaf.mean
    outDF$delta.Cfroot[outDF$Cat=="Observed"] <- obs$delta.C.froot.mean
    outDF$delta.Cmyco[outDF$Cat=="Observed"] <- obs$delta.C.myco.mean
    
    outDF$CUE[outDF$Cat=="Observed"] <- 1 - (obs$Ra.mean/obs$GPP.mean) 
    
    
    print(outDF)
}