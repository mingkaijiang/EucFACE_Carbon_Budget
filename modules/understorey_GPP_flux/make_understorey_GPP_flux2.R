make_understorey_GPP_flux2 <- function(o.gpp) {
    
    ### Use 2015-2016 treatment GPP to calculate proportion of understorey to overstorey

    ### read understorey simulated gpp for 2015 - 2016, per ring
    myDF <- read.csv("data/underS_species2.gpp.csv")
    myDF <- subset(myDF, Species == 2)
    myDF$Ring <- gsub("R","", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)
    
    for (i in 1:6) {
        for (j in c(2015,2016)) {
            myDF$GPP.O[myDF$Ring==i&myDF$year==j] <- o.gpp$GPP[o.gpp$Ring==i&o.gpp$year==j]
        }
    }
    
    myDF$prop <- myDF$GPP.sum / myDF$GPP.O
 
    a.p1 <- mean(myDF$prop[myDF$Ring==1], na.rm=T)
    a.p2 <- mean(myDF$prop[myDF$Ring==2], na.rm=T)
    a.p3 <- mean(myDF$prop[myDF$Ring==3], na.rm=T)
    a.p4 <- mean(myDF$prop[myDF$Ring==4], na.rm=T)
    a.p5 <- mean(myDF$prop[myDF$Ring==5], na.rm=T)
    a.p6 <- mean(myDF$prop[myDF$Ring==6], na.rm=T)
    
    ### prepare understorey gpp dataframe
    u.gpp <- data.frame(rep(c(2013:2014), each=6), rep(c(1:6), by=2), NA, NA, NA, NA)
    colnames(u.gpp) <- c("Yr", "Ring", "Trt", "GPP", "GPP.O", "prop")
 
    for (i in 1:6) {
        for (j in 2013:2014) {
            u.gpp$GPP.O[u.gpp$Yr==j&u.gpp$Ring==i] <- o.gpp$GPP[o.gpp$year==j&o.gpp$Ring==i]
        }
    }
    
    u.gpp$prop[u.gpp$Ring==1] <- a.p1
    u.gpp$prop[u.gpp$Ring==2] <- a.p2
    u.gpp$prop[u.gpp$Ring==3] <- a.p3
    u.gpp$prop[u.gpp$Ring==4] <- a.p4
    u.gpp$prop[u.gpp$Ring==5] <- a.p5
    u.gpp$prop[u.gpp$Ring==6] <- a.p6
    
    u.gpp$GPP <- u.gpp$GPP.O * u.gpp$prop
    u.gpp2 <- myDF[,c("year", "Ring", "c.treat", "GPP.sum", "GPP.O", "prop")]
    colnames(u.gpp2) <- c("year", "Ring", "Trt", "GPP.U", "GPP.O", "prop")
    colnames(u.gpp) <- c("year", "Ring", "Trt", "GPP.U", "GPP.O", "prop")
    out <- rbind(u.gpp, u.gpp2)
    
    out$Trt[out$Ring%in%c(2,3,6)] <- "aCO2"
    out$Trt[out$Ring%in%c(1,4,5)] <- "eCO2"
    out$Date <- paste0(out$year, "-01-01")
    
    outDF <- out[,c("year", "Ring", "GPP.U", "Date", "Trt")]
    colnames(outDF) <- c("year", "Ring", "GPP", "Date", "Trt")
    
    
    return(outDF)
}