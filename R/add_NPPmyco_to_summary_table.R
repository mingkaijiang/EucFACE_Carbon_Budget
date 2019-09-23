add_NPPmyco_to_summary_table <- function(inDF,
                                         pChain.aCO2,
                                         pChain.eCO2) {
    
    ### chian length
    l1 <- nrow(pChain.aCO2)/3
    l2 <- nrow(pChain.eCO2)/3
    
    ### get mycorrhizal production flux
    r2.myco <- mean(pChain.aCO2[1:l1,"NPP.myco"])
    r3.myco <- mean(pChain.aCO2[(l1+1):(l1*2),"NPP.myco"])
    r6.myco <- mean(pChain.aCO2[(l1*2+1):(l1*3),"NPP.myco"])
    
    r1.myco <- mean(pChain.eCO2[1:l1,"NPP.myco"])
    r4.myco <- mean(pChain.eCO2[(l1+1):(l1*2),"NPP.myco"])
    r5.myco <- mean(pChain.eCO2[(l1*2+1):(l1*3),"NPP.myco"])
    
    ### assign
    inDF$npp$Ring_1[inDF$npp$term=="Mycorrhizal production"] <- r1.myco
    inDF$npp$Ring_2[inDF$npp$term=="Mycorrhizal production"] <- r2.myco
    inDF$npp$Ring_3[inDF$npp$term=="Mycorrhizal production"] <- r3.myco
    inDF$npp$Ring_4[inDF$npp$term=="Mycorrhizal production"] <- r4.myco
    inDF$npp$Ring_5[inDF$npp$term=="Mycorrhizal production"] <- r5.myco
    inDF$npp$Ring_6[inDF$npp$term=="Mycorrhizal production"] <- r6.myco
    
    ### row means and sd
    inDF$npp$aCO2[inDF$npp$term=="Mycorrhizal production"] <- mean(c(r2.myco, r3.myco, r6.myco), na.rm=T)
    inDF$npp$eCO2[inDF$npp$term=="Mycorrhizal production"] <- mean(c(r1.myco, r4.myco, r5.myco), na.rm=T)
    
    inDF$npp$diff[inDF$npp$term=="Mycorrhizal production"] <- mean(c(r1.myco, r4.myco, r5.myco), na.rm=T) - mean(c(r2.myco, r3.myco, r6.myco), na.rm=T)
    inDF$npp$percent_diff[inDF$npp$term=="Mycorrhizal production"] <- (mean(c(r1.myco, r4.myco, r5.myco), na.rm=T) - mean(c(r2.myco, r3.myco, r6.myco), na.rm=T) ) / mean(c(r2.myco, r3.myco, r6.myco), na.rm=T)
    
    
    inDF$npp$aCO2_sd[inDF$npp$term=="Mycorrhizal production"] <- sd(c(r2.myco, r3.myco, r6.myco), na.rm=T)
    inDF$npp$eCO2_sd[inDF$npp$term=="Mycorrhizal production"] <- sd(c(r1.myco, r4.myco, r5.myco), na.rm=T)

    
    return(inDF)
    
}