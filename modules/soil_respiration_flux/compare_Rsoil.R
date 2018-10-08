compare_Rsoil <- function(aDF, jDF) {
    ### aDF is Alexis's Rsoil data
    ### jDF is John's rsoil data
    ### John uses the manuel survey collars (n = 8 per ring, 6*8 = 48) to fit 48 DAMM parameters set. 
    ### He then uses these 48 parameter sets to produce a model based dataset, 
    ### from which he calculate annual budget (result of model only, n = 48, manual survey)
    ### Alexis used the automated collars only (n = 1 per ring, 1*6 = 6) to fit 6 DAMM parameters set. 
    ### He then use 6 parameter set to fill data gaps. Data gaps are broadly 50%. 
    ### So 50% of Alexis's dataset is actual data, 50% is modelled. 
    ### (result of 50% quality checked data, 50% model. n = 6 (3 for ambient, 3 for elevated), automated chambers)

    
    myDF <- merge(aDF, jDF, by=c("Date", "Ring"))
    myDF <- myDF[,c("Date", "Ring", "soil_respiration_flux.x", "soil_respiration_flux.y", "ndays.y")]
    colnames(myDF) <- c("Date", "Ring", "soil_resp_a", "soil_resp_j", "ndays")
    
    
    myDF$Trt[myDF$Ring%in%c(1,4,5)] <- "eCO2"
    myDF$Trt[myDF$Ring%in%c(2,3,6)] <- "aCO2"
    myDF$Yr <- year(myDF$Date)
    
    # calculate annual rate
    annDF <- summaryBy(soil_resp_a+soil_resp_j~Yr+Ring, data=myDF, keep.names=T, FUN=sum)
    for (i in 1:6) {
        for (j in unique(myDF$Yr)) {
            annDF$soil_resp_a[annDF$Yr==j&annDF$Ring==i] <- sum(myDF$soil_resp_a[myDF$Yr==j&myDF$Ring==i])/sum(myDF$ndays[myDF$Yr==j&myDF$Ring==i])*365/1000
            annDF$soil_resp_j[annDF$Yr==j&annDF$Ring==i] <- sum(myDF$soil_resp_j[myDF$Yr==j&myDF$Ring==i])/sum(myDF$ndays[myDF$Yr==j&myDF$Ring==i])*365/1000
            
        }
    }
    
    p1 <- ggplot(myDF) +
        geom_point(data=myDF, aes(x=Date, y=soil_resp_a, fill="A"),shape=21) +
        geom_point(data=myDF, aes(x=Date, y=soil_resp_j, fill="J"),shape=21)+
        scale_fill_manual(name="Method", 
                          values = c("A" = "yellow", "J" = "blue"),
                          labels = c("Alexis", "John"))
    
    #plot(p1)
    
    annDF$Trt[annDF$Ring%in%c(1,4,5)] <- "eCO2"
    annDF$Trt[annDF$Ring%in%c(2,3,6)] <- "aCO2"
    
    trtDF <- summaryBy(soil_resp_a+soil_resp_j~Yr+Trt, data=annDF, keep.names=T, FUN=mean)
    
    p2 <- ggplot(trtDF)+
        geom_point(data=trtDF[trtDF$Trt=="aCO2",], aes(x=Yr, y=soil_resp_a, fill="A_aCO2"),shape=21, size=5) +
        geom_point(data=trtDF[trtDF$Trt=="eCO2",], aes(x=Yr, y=soil_resp_a, fill="A_eCO2"),shape=21, size=5) +
        geom_point(data=trtDF[trtDF$Trt=="aCO2",], aes(x=Yr, y=soil_resp_j, fill="J_aCO2"),shape=21, size=5)+
        geom_point(data=trtDF[trtDF$Trt=="eCO2",], aes(x=Yr, y=soil_resp_j, fill="J_eCO2"),shape=21, size=5)+
        scale_fill_manual(name="Method", 
                          values = c("A_aCO2" = "yellow", "A_eCO2" = "orange", "J_aCO2" = "blue", "J_eCO2" = "darkblue"),
                          labels = c("Alexis_aCO2", "Alexis_eCO2", "John_aCO2", "John_eCO2"))
    
    
    pdf("R_other/Rsoil_comparison.pdf")
    plot(p1)
    plot(p2)
    dev.off()
    
    
}