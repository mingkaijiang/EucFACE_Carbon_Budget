make_parameter_summary_table <- function() {
    
    ### calculate CUE
    cue.aco2.old <- with(pChain.aCO2, (mean(NPP.leaf) + mean(NPP.wood) + mean(NPP.froot)) / mean(GPP))
    cue.aco2.new <- mean(pChain.aCO2$CUE)
    
    cue.eco2.old <- with(pChain.eCO2, (mean(NPP.leaf) + mean(NPP.wood) + mean(NPP.froot)) / mean(GPP))
    cue.eco2.new <- mean(pChain.eCO2$CUE)
    
    
    r1 <- predict_final_output(pChain = pChain.aCO2, 
                                 obs = obsDF[4,],
                                 return.option = "Return final parameter")
    
    r2 <- predict_final_output(pChain = pChain.eCO2, 
                                 obs = eco2DF[4,],
                                 return.option = "Return final parameter")
    
    
    out <- rbind(r1, r2)
    out$Trt <- c("aCO2", "eCO2")
    
    out[out$Trt=="aCO2", "CUE_old"] <- cue.aco2.old
    out[out$Trt=="aCO2", "CUE_new"] <- cue.aco2.new
    
    out[out$Trt=="eCO2", "CUE_old"] <- cue.eco2.old
    out[out$Trt=="eCO2", "CUE_new"] <- cue.eco2.new
    
    
    
    
    write.csv(out, "DA_output/parameter_summary_table.csv", row.names=F)
    
    print(out)
    
}