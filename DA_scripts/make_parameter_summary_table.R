make_parameter_summary_table <- function() {
    
    ### calculate CUE
    cue.aco2.old <- with(pChain.aCO2, (NPP.leaf + NPP.wood + NPP.froot) / GPP)
    cue.aco2.old.mean <- mean(cue.aco2.old)
    cue.aco2.old.sd <- sd(cue.aco2.old)
    
    cue.aco2.new.mean <- mean(pChain.aCO2$CUE)
    cue.aco2.new.sd <- sd(pChain.aCO2$CUE)
    
    cue.eco2.old <- with(pChain.eCO2, (NPP.leaf + NPP.wood + NPP.froot) / GPP)
    cue.eco2.old.mean <- mean(cue.eco2.old)
    cue.eco2.old.sd <- sd(cue.eco2.old)
    
    cue.eco2.new.mean <- mean(pChain.eCO2$CUE)
    cue.eco2.new.sd <- sd(pChain.eCO2$CUE)
    
    ## alloc myco
    alloc.myco.aco2 <- with(pChain.aCO2, NPP.myco/NPP)
    alloc.myco.eco2 <- with(pChain.eCO2, NPP.myco/NPP)
    
    alloc.myco.aco2.mean <- mean(alloc.myco.aco2)
    alloc.myco.eco2.mean <- mean(alloc.myco.eco2)
    
    alloc.myco.aco2.sd <- sd(alloc.myco.aco2)
    alloc.myco.eco2.sd <- sd(alloc.myco.eco2)
    
    ### final set of parameters
    r1 <- predict_final_output(pChain = pChain.aCO2, 
                                 obs = obsDF[4,],
                                 return.option = "Return final parameter")
    
    r2 <- predict_final_output(pChain = pChain.eCO2, 
                                 obs = eco2DF[4,],
                                 return.option = "Return final parameter")
    
    
    out <- rbind(r1, r2)
    out$Trt <- c("aCO2", "eCO2")
    
    out[out$Trt=="aCO2", "CUE.old"] <- cue.aco2.old.mean
    out[out$Trt=="aCO2", "CUE.new"] <- cue.aco2.new.mean
    
    out[out$Trt=="eCO2", "CUE.old"] <- cue.eco2.old.mean
    out[out$Trt=="eCO2", "CUE.new"] <- cue.eco2.new.mean
    
    out[out$Trt=="aCO2", "CUE.old.sd"] <- cue.aco2.old.sd
    out[out$Trt=="aCO2", "CUE.new.sd"] <- cue.aco2.new.sd
    
    out[out$Trt=="eCO2", "CUE.old.sd"] <- cue.eco2.old.sd
    out[out$Trt=="eCO2", "CUE.new.sd"] <- cue.eco2.new.sd
    
    out[out$Trt=="aCO2", "alloc.myco"] <- alloc.myco.aco2.mean
    out[out$Trt=="aCO2", "alloc.myco.sd"] <- alloc.myco.aco2.sd
    
    out[out$Trt=="eCO2", "alloc.myco"] <- alloc.myco.eco2.mean
    out[out$Trt=="eCO2", "alloc.myco.sd"] <- alloc.myco.eco2.sd
    
    ## arrange
    out <- out[,c("alloc.leaf", "alloc.wood", "alloc.froot", "alloc.myco",
                  "tau.leaf", "tau.froot", "tau.myco", "tau.bg.lit",
                  "tau.micr", "tau.soil", "C.ag.lit", "C.bg.lit",
                  "frac.ag.lit", "frac.bg.lit", "frac.micr", 
                  "alloc.leaf.sd", "alloc.wood.sd", "alloc.froot.sd", "alloc.myco.sd",
                  "tau.leaf.sd", "tau.froot.sd", "tau.myco.sd", "tau.bg.lit.sd",
                  "tau.micr.sd", "tau.soil.sd", "C.ag.lit.sd", "C.bg.lit.sd",
                  "frac.ag.lit.sd", "frac.bg.lit.sd", "frac.micr.sd", 
                  "Trt", "CUE.old", "CUE.new", "CUE.old.sd", "CUE.new.sd")]
    
    
    write.csv(out, "DA_output/parameter_summary_table.csv", row.names=F)
    
    print(out)
    
}