make_understorey_GPP_flux2 <- function(o.gpp) {
    
    ### Use 2015-2016 treatment GPP to calculate proportion of understorey to overstorey

    ### prepare understorey gpp dataframe
    u.gpp <- data.frame(rep(c(2013:2016), each=2), rep(c("aCO2", "eCO2"), by=4), NA)
    colnames(u.gpp) <- c("Yr", "Trt", "GPP")
    u.gpp$GPP[u.gpp$Yr=="2015"&u.gpp$Trt=="aCO2"] <- 293.7
    u.gpp$GPP[u.gpp$Yr=="2015"&u.gpp$Trt=="eCO2"] <- 402.5
    
    u.gpp$GPP[u.gpp$Yr=="2016"&u.gpp$Trt=="aCO2"] <- 292.4
    u.gpp$GPP[u.gpp$Yr=="2016"&u.gpp$Trt=="eCO2"] <- 363.3
    
    u.gpp$GPP.O[u.gpp$Yr=="2015"&u.gpp$Trt=="aCO2"] <- mean(o.gpp$GPP[o.gpp$year=="2015"&o.gpp$Trt=="aCO2"])
    u.gpp$GPP.O[u.gpp$Yr=="2016"&u.gpp$Trt=="aCO2"] <- mean(o.gpp$GPP[o.gpp$year=="2016"&o.gpp$Trt=="aCO2"])
    
    u.gpp$GPP.O[u.gpp$Yr=="2015"&u.gpp$Trt=="eCO2"] <- mean(o.gpp$GPP[o.gpp$year=="2015"&o.gpp$Trt=="eCO2"])
    u.gpp$GPP.O[u.gpp$Yr=="2016"&u.gpp$Trt=="eCO2"] <- mean(o.gpp$GPP[o.gpp$year=="2016"&o.gpp$Trt=="eCO2"])
    
    u.gpp$prop <- u.gpp$GPP / u.gpp$GPP.O
    
    a.p <- mean(u.gpp$prop[u.gpp$Trt=="aCO2"], na.rm=T)
    e.p <- mean(u.gpp$prop[u.gpp$Trt=="eCO2"], na.rm=T)
    
    u.gpp$pro[u.gpp$Yr%in%c("2013", "2014")&u.gpp$Trt=="aCO2"] <- a.p
    u.gpp$pro[u.gpp$Yr%in%c("2013", "2014")&u.gpp$Trt=="eCO2"] <- e.p
    
    for (i in c(2013:2016)) {
        o.gpp$prop[o.gpp$year==i&o.gpp$Trt=="aCO2"] <- u.gpp$pro[u.gpp$Yr==i&u.gpp$Trt=="aCO2"]
        o.gpp$prop[o.gpp$year==i&o.gpp$Trt=="eCO2"] <- u.gpp$pro[u.gpp$Yr==i&u.gpp$Trt=="eCO2"]
    }
    
    o.gpp$GPP.U <- o.gpp$GPP * o.gpp$prop
    
    outDF <- o.gpp[,c("year", "Ring", "GPP.U", "Date", "Trt")]
    colnames(outDF) <- c("year", "Ring", "GPP", "Date", "Trt")
    
    
    return(outDF)
}