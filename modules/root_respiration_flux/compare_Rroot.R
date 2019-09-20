compare_Rroot <- function(nDF, oDF) {
    ### nDF is new
    ### oDF is old
    
    myDF <- merge(nDF, oDF, by=c("Date", "Ring"))
    myDF <- myDF[,c("Date", "Ring", "root_respiration_flux.x", "root_respiration_flux.y", "ndays.y")]
    colnames(myDF) <- c("Date", "Ring", "root_resp_n", "root_resp_o", "ndays")
    
    
    myDF$Trt[myDF$Ring%in%c(1,4,5)] <- "eCO2"
    myDF$Trt[myDF$Ring%in%c(2,3,6)] <- "aCO2"
    myDF$Yr <- year(myDF$Date)
    
    # calculate annual rate
    annDF <- summaryBy(root_resp_n+root_resp_o~Yr+Ring, data=myDF, keep.names=T, FUN=sum)
    for (i in 1:6) {
        for (j in unique(myDF$Yr)) {
            annDF$root_resp_n[annDF$Yr==j&annDF$Ring==i] <- sum(myDF$root_resp_n[myDF$Yr==j&myDF$Ring==i])/sum(myDF$ndays[myDF$Yr==j&myDF$Ring==i])*365/1000
            annDF$root_resp_o[annDF$Yr==j&annDF$Ring==i] <- sum(myDF$root_resp_o[myDF$Yr==j&myDF$Ring==i])/sum(myDF$ndays[myDF$Yr==j&myDF$Ring==i])*365/1000
            
        }
    }
    
    p1 <- ggplot(myDF) +
        geom_point(data=myDF, aes(x=Date, y=root_resp_n, fill="N"),shape=21) +
        geom_point(data=myDF, aes(x=Date, y=root_resp_o, fill="O"),shape=21)+
        scale_fill_manual(name="Method", 
                          values = c("N" = "yellow", "O" = "blue"),
                          labels = c("new", "old"))
    
    plot(p1)
    
    annDF$Trt[annDF$Ring%in%c(1,4,5)] <- "eCO2"
    annDF$Trt[annDF$Ring%in%c(2,3,6)] <- "aCO2"
    
    trtDF <- summaryBy(root_resp_n+root_resp_o~Yr+Trt, data=annDF, keep.names=T, FUN=mean)
    
    p2 <- ggplot(trtDF)+
        geom_point(data=trtDF[trtDF$Trt=="aCO2",], aes(x=Yr, y=root_resp_n, fill="N_aCO2"),shape=21, size=5) +
        geom_point(data=trtDF[trtDF$Trt=="eCO2",], aes(x=Yr, y=root_resp_n, fill="N_eCO2"),shape=21, size=5) +
        geom_point(data=trtDF[trtDF$Trt=="aCO2",], aes(x=Yr, y=root_resp_o, fill="O_aCO2"),shape=21, size=5)+
        geom_point(data=trtDF[trtDF$Trt=="eCO2",], aes(x=Yr, y=root_resp_o, fill="O_eCO2"),shape=21, size=5)+
        scale_fill_manual(name="Method", 
                          values = c("N_aCO2" = "yellow", "N_eCO2" = "orange", "O_aCO2" = "blue", "O_eCO2" = "darkblue"),
                          labels = c("New_aCO2", "New_eCO2", "Old_aCO2", "Old_eCO2"))+
        ylab("Rroot")
    
    
    plot(p2)
    
    pdf("R_other/Rroot_comparison.pdf")
    #plot(p1)
    plot(p2)
    dev.off()
    
    
}