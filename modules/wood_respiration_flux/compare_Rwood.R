compare_Rwood <- function(nDF, rDF) {
    ### nDF is Nam Jin's Rwood
    ### rDF is Roberto's data. 
    
    myDF <- merge(nDF, rDF, by=c("Date", "Ring"))
    myDF <- myDF[,c("Date", "Ring", "wood_respiration.x", "wood_respiration.y", "ndays.y")]
    colnames(myDF) <- c("Date", "Ring", "soil_resp_n", "soil_resp_r", "ndays")
    
    
    myDF$Trt[myDF$Ring%in%c(1,4,5)] <- "eCO2"
    myDF$Trt[myDF$Ring%in%c(2,3,6)] <- "aCO2"
    myDF$Yr <- year(myDF$Date)
    
    # calculate annual rate
    annDF <- summaryBy(soil_resp_n+soil_resp_r~Yr+Ring, data=myDF, keep.names=T, FUN=sum)
    for (i in 1:6) {
        for (j in unique(myDF$Yr)) {
            annDF$soil_resp_n[annDF$Yr==j&annDF$Ring==i] <- sum(myDF$soil_resp_n[myDF$Yr==j&myDF$Ring==i])/sum(myDF$ndays[myDF$Yr==j&myDF$Ring==i])*365/1000
            annDF$soil_resp_r[annDF$Yr==j&annDF$Ring==i] <- sum(myDF$soil_resp_r[myDF$Yr==j&myDF$Ring==i])/sum(myDF$ndays[myDF$Yr==j&myDF$Ring==i])*365/1000
            
        }
    }
    
    p1 <- ggplot(myDF) +
        geom_point(data=myDF, aes(x=Date, y=soil_resp_n, fill="N"),shape=21) +
        geom_point(data=myDF, aes(x=Date, y=soil_resp_r, fill="R"),shape=21)+
        scale_fill_manual(name="Method", 
                          values = c("N" = "yellow", "R" = "blue"),
                          labels = c("Nam Jin", "Roberto"))
    
    #plot(p1)
    
    annDF$Trt[annDF$Ring%in%c(1,4,5)] <- "eCO2"
    annDF$Trt[annDF$Ring%in%c(2,3,6)] <- "aCO2"
    
    trtDF <- summaryBy(soil_resp_n+soil_resp_r~Yr+Trt, data=annDF, keep.names=T, FUN=mean)
    
    p2 <- ggplot(trtDF)+
        geom_point(data=trtDF[trtDF$Trt=="aCO2",], aes(x=Yr, y=soil_resp_n, fill="N_aCO2"),shape=21, size=5) +
        geom_point(data=trtDF[trtDF$Trt=="eCO2",], aes(x=Yr, y=soil_resp_n, fill="N_eCO2"),shape=21, size=5) +
        geom_point(data=trtDF[trtDF$Trt=="aCO2",], aes(x=Yr, y=soil_resp_r, fill="R_aCO2"),shape=21, size=5)+
        geom_point(data=trtDF[trtDF$Trt=="eCO2",], aes(x=Yr, y=soil_resp_r, fill="R_eCO2"),shape=21, size=5)+
        scale_fill_manual(name="Method", 
                          values = c("N_aCO2" = "yellow", "N_eCO2" = "orange", "R_aCO2" = "blue", "R_eCO2" = "darkblue"),
                          labels = c("NamJin_aCO2", "NamJin_eCO2", "Roberto_aCO2", "Roberto_eCO2"))
    
    
    #plot(p2)
    
    pdf("R_other/Rwood_comparison.pdf")
    plot(p1)
    plot(p2)
    dev.off()
    
    
}