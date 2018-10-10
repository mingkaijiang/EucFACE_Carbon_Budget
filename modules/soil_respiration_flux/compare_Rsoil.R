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
            annDF$soil_resp_a[annDF$Yr==j&annDF$Ring==i] <- sum(myDF$soil_resp_a[myDF$Yr==j&myDF$Ring==i]*myDF$ndays[myDF$Yr==j&myDF$Ring==i])/sum(myDF$ndays[myDF$Yr==j&myDF$Ring==i])*365/1000
            annDF$soil_resp_j[annDF$Yr==j&annDF$Ring==i] <- sum(myDF$soil_resp_j[myDF$Yr==j&myDF$Ring==i]*myDF$ndays[myDF$Yr==j&myDF$Ring==i])/sum(myDF$ndays[myDF$Yr==j&myDF$Ring==i])*365/1000
            
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
    
    
    ### Investigate 2014 (by defining 2014 as 2013-09 - 2014-08)
    
    ### add new year information
    myDF$Bio.year <- ifelse(myDF$Date <= "2013-09-05", "2013", 
                            ifelse(myDF$Date <= "2014-09-05" & myDF$Date >= "2013-09-06", "2014",
                                   ifelse(myDF$Date <= "2015-09-05" & myDF$Date >= "2014-09-06", "2015", NA)))
    
    myDF <- myDF[complete.cases(myDF$Bio.year),]
    
    annDF2 <- summaryBy(soil_resp_a+soil_resp_j~Ring+Bio.year+Trt, FUN=sum, data=myDF, keep.names=T)
    annDF3 <- summaryBy(soil_resp_a+soil_resp_j~Trt+Bio.year, FUN=mean, data=annDF2, keep.names=T)
    annDF3$soil_resp_a <- annDF3$soil_resp_a/1000
    annDF3$soil_resp_j <- annDF3$soil_resp_j/1000
    
    ### Annual sum based on biological year
    p3 <- ggplot(annDF3)+
        geom_point(data=annDF3[annDF3$Trt=="aCO2",], aes(x=Bio.year, y=soil_resp_a, fill="A_aCO2"),shape=21, size=5) +
        geom_point(data=annDF3[annDF3$Trt=="eCO2",], aes(x=Bio.year, y=soil_resp_a, fill="A_eCO2"),shape=21, size=5) +
        geom_point(data=annDF3[annDF3$Trt=="aCO2",], aes(x=Bio.year, y=soil_resp_j, fill="J_aCO2"),shape=21, size=5)+
        geom_point(data=annDF3[annDF3$Trt=="eCO2",], aes(x=Bio.year, y=soil_resp_j, fill="J_eCO2"),shape=21, size=5)+
        scale_fill_manual(name="Method", 
                          values = c("A_aCO2" = "yellow", "A_eCO2" = "orange", "J_aCO2" = "blue", "J_eCO2" = "darkblue"),
                          labels = c("Alexis_aCO2", "Alexis_eCO2", "John_aCO2", "John_eCO2"))
    
    
    ### Overlay John and Alexis data by year
    myDF$DOY <- rep(rep(c(1:365), each=6), by=3)
    p4 <- ggplot(myDF)+
        geom_point(data=myDF[myDF$Bio.year=="2013",], aes(x=DOY, y=soil_resp_a, fill="A_2013"),shape=21, size=1) +
        geom_point(data=myDF[myDF$Bio.year=="2014",], aes(x=DOY, y=soil_resp_a, fill="A_2014"),shape=21, size=2) +
        geom_point(data=myDF[myDF$Bio.year=="2015",], aes(x=DOY, y=soil_resp_a, fill="A_2015"),shape=21, size=1)+
        scale_fill_manual(name="Method", 
                          values = c("A_2013" = "yellow", "A_2014" = "orange", "A_2015" = "lightblue"),
                          labels = c("A2013", "A2014", "A2015"))+
        geom_vline(aes(xintercept = 116))+
        ylim(0,8500)
    
    p5 <- ggplot(myDF)+
        geom_point(data=myDF[myDF$Bio.year=="2013",], aes(x=DOY, y=soil_resp_a, fill="A_2013"),shape=21, size=1) +
        scale_fill_manual(name="Method", 
                          values = c("A_2013" = "yellow"),
                          labels = c("A2013"))+
        geom_vline(aes(xintercept = 116))+
        ylim(0,8500)
    
    p6 <- ggplot(myDF)+
        geom_point(data=myDF[myDF$Bio.year=="2014",], aes(x=DOY, y=soil_resp_a, fill="A_2014"),shape=21, size=1) +
        scale_fill_manual(name="Method", 
                          values = c("A_2014" = "orange"),
                          labels = c("A2014"))+
        geom_vline(aes(xintercept = 116))+
        ylim(0,8500)
    
    p7 <- ggplot(myDF)+
        geom_point(data=myDF[myDF$Bio.year=="2015",], aes(x=DOY, y=soil_resp_a, fill="A_2015"),shape=21, size=1) +
        scale_fill_manual(name="Method", 
                          values = c("A_2015" = "lightblue"),
                          labels = c("A2015"))+
        geom_vline(aes(xintercept = 116))+
        ylim(0,8500)
    
    p8 <- ggplot(myDF)+
        geom_point(data=myDF[myDF$Bio.year=="2013",], aes(x=DOY, y=soil_resp_j, fill="J_2013"),shape=21, size=1) +
        geom_point(data=myDF[myDF$Bio.year=="2014",], aes(x=DOY, y=soil_resp_j, fill="J_2014"),shape=21, size=2) +
        geom_point(data=myDF[myDF$Bio.year=="2015",], aes(x=DOY, y=soil_resp_j, fill="J_2015"),shape=21, size=1)+
        scale_fill_manual(name="Method", 
                          values = c("J_2013" = "yellow", "J_2014" = "orange", "J_2015" = "lightblue"),
                          labels = c("J2013", "J2014", "J2015"))+
        geom_vline(aes(xintercept = 116))+
        ylim(0,8500)
    
    p9 <- ggplot(myDF)+
        geom_point(data=myDF[myDF$Bio.year=="2013",], aes(x=DOY, y=soil_resp_j, fill="J_2013"),shape=21, size=1) +
        scale_fill_manual(name="Method", 
                          values = c("J_2013" = "yellow"),
                          labels = c("J2013"))+
        geom_vline(aes(xintercept = 116))+
        ylim(0,8500)
    
    p10 <- ggplot(myDF)+
        geom_point(data=myDF[myDF$Bio.year=="2014",], aes(x=DOY, y=soil_resp_j, fill="J_2014"),shape=21, size=1) +
        scale_fill_manual(name="Method", 
                          values = c("J_2014" = "orange"),
                          labels = c("J2014"))+
        geom_vline(aes(xintercept = 116))+
        ylim(0,8500)
    
    p11 <- ggplot(myDF)+
        geom_point(data=myDF[myDF$Bio.year=="2015",], aes(x=DOY, y=soil_resp_j, fill="J_2015"),shape=21, size=1) +
        scale_fill_manual(name="Method", 
                          values = c("J_2015" = "lightblue"),
                          labels = c("J2015"))+
        geom_vline(aes(xintercept = 116))+
        ylim(0,8500)
    
    
    p12 <- ggplot(myDF)+
        geom_point(data=myDF[myDF$Bio.year=="2014",], aes(x=DOY, y=soil_resp_j, fill="J_2014"),shape=21, size=1) +
        geom_point(data=myDF[myDF$Bio.year=="2014",], aes(x=DOY, y=soil_resp_a, fill="A_2014"),shape=21, size=1)+
        scale_fill_manual(name="Method", 
                          values = c("A_2014" = "lightblue","J_2014" = "orange"),
                          labels = c("A2014","J2014"))+
        geom_vline(aes(xintercept = 116))+
        ylim(0,8500)
    
    
    pdf("R_other/Rsoil_comparison_extended.pdf")
    plot(p3)
    plot(p4)
    plot(p5)
    plot(p6)
    plot(p7)
    plot(p8)
    plot(p9)
    plot(p10)
    plot(p11)
    plot(p12)

    dev.off()
    
    
    ### Add moisture
    smc <- readRDS("temp_files/facesoilwater.RDS")
    myDF2 <- merge(myDF, smc, by=c("Date"))
    
    p13 <- ggplot(myDF2)+
        geom_point(data=myDF2[myDF2$Bio.year=="2014",], aes(x=DOY, y=soil_resp_j, fill="J_2014"),shape=21, size=1) +
        geom_vline(aes(xintercept = 116))+
        ylim(0,8500)+
        ylab("Rsoil_J_2014")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    p14 <- ggplot(myDF2)+
        geom_point(data=myDF2[myDF2$Bio.year=="2014",], aes(x=DOY, y=soil_resp_a, fill="J_2014"),shape=21, size=1) +
        geom_vline(aes(xintercept = 116))+
        ylim(0,8500)+
        ylab("Rsoil_A_2014")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    myDF3 <- subset(smc, Date>="2013-09-06" & Date <= "2014-09-05")
    myDF3$DOY <- c(1:365)
    
    p15 <- ggplot(myDF3)+
        geom_bar(mapping = aes(x = DOY, y = VWC), stat = "identity", fill = "grey") +
        geom_vline(aes(xintercept = 116))

    
    require(grid)
    require(cowplot)
    
    pdf("R_other/Rsoil_SWC.pdf")
    plot_grid(p13, p14, p15, 
              ncol=1, align="v", axis="l")
    dev.off()

}
