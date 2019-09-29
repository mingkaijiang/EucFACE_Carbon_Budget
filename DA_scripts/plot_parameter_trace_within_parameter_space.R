plot_parameter_trace_within_parameter_space <- function(params,
                                                        params.upper,
                                                        params.lower,
                                                        inDF,
                                                        dist.type,
                                                        step.size,
                                                        chainLength,
                                                        Trt) {
    
    ### assign y value
    inDF$y <- 1
    
    mDF <- colMeans(inDF)
    
    
    ### define names
    names <- c("alloc.leaf", "alloc.root", "alloc.myco",
               "tau.leaf", "tau.root", "tau.myco",
               "tau.ag.lit", "tau.bg.lit", "tau.micr", "tau.soil", 
               "C.bg.lit", "frac.myco", "frac.ag.lit", "frac.bg.lit", "frac.micr")
    
    ### plotting
    pdf(paste0("DA_output/parameter_trace_", dist.type, "_", 
               step.size, "_", chainLength, "_", Trt,
               ".pdf"), width=8, height=4)
    
    par(mfrow=c(1,2))
    
    for (i in 1:no.var) {
        ### hist
        hist(inDF[,i], main = names[i],
             prob=TRUE,col="black",border="white")
        lines(density(inDF[,i],na.rm=T),col="red",lwd=4)
        abline(v=mDF[i], lwd = 6, col="blue")
        
        
        ### trace
        plot(inDF[,i], ylim = c(params.lower[i], params.upper[i]),
             #ylim = c(0.9,1.1), 
             ylab = names[i], xlab = " ", type="l")
    }
    
    dev.off()
    
    
}