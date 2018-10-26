make_stem_basal_respiration_rate <- function() {
    
    ### download the data
    download_stem_basal_respiration_data()

    ### read files
    myDF1 <- read.csv(file.path(getToPath(), 
                                  "FACE_A0089_RA_STEMCO2EFLUX_L1_20171218-20171220.csv"))
    myDF2 <- read.csv(file.path(getToPath(), 
                                  "FACE_A0089_RA_STEMCO2EFLUX_L1_20180115-20180117.csv"))
    myDF3 <- read.csv(file.path(getToPath(), 
                                  "FACE_A0089_RA_STEMCO2EFLUX_L1_20180205-20180207.csv"))
    
    myDF <- rbind(myDF2, myDF3)
    myDF <- rbind(myDF, myDF1)
    
    
    ### Read stem temperature files
    tDF <- read.csv(file.path(getToPath(), 
                                "FACE_A0089_RA_XYLEMCO2_L1_20171216-20180211.csv"))

    ### Add Ring information based on tree number
    require(plyr)
    myDF$Ring <- round_any(myDF$Label, 100, f=floor)/100
    
    ### exclude NAs
    myDF <- myDF[complete.cases(myDF$flux_corrected),]
    
    ### Process date and time
    myDF$date <- as.Date(myDF$date, format="%d/%m/%Y")
    myDF$datetime <- paste0(myDF$date, " ", myDF$time)
    myDF$datetime <- as.POSIXct(myDF$datetime, "%Y-%m-%d %h:%m:%s")
    myDF$time10 <- round_date(myDF$datetime, "10 mins")
    
    ### Process date and time for temperature
    tDF$date <- as.Date(as.character(tDF$date), format="%d/%m/%Y")
    tDF$datetime <- paste0(tDF$date, " ", tDF$time)
    tDF$datetime <- as.POSIXct(tDF$datetime, "%Y-%m-%d %h:%m:%s")
    
    ### Add Tair onto the data frame
    myDF2 <- merge(myDF, tDF, by.x=c("time10"), by.y=c("datetime"))
    
    ### exclude unnecessary columns
    myDF2 <- myDF2[,c("time10", "date.x", "time.x", "Label", "Area", "Vtotal", "Tcham", "coef_V", "coef_S", "flux_corrected",
                      "Ring", "R1_Temp", "R2_Temp", "R3_Temp", "R4_Temp", "R5_Temp", "R6_Temp")]
    
    ### Add temperature
    myDF2[,"Temp"] <- ifelse(myDF2$Ring == 1, myDF2$R1_Temp, 
                             ifelse(myDF2$Ring == 2, myDF2$R2_Temp,
                                    ifelse(myDF2$Ring == 3, myDF2$R3_Temp,
                                           ifelse(myDF2$Ring == 4, myDF2$R4_Temp,
                                                  ifelse(myDF2$Ring == 5, myDF2$R5_Temp, myDF2$R6_Temp)))))
    
    ### generate temperature function for each ring
    myDF2$ln_flux <- log(myDF2$flux_corrected)
    
    mod1 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==1,])
    mod2 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==2,])
    mod3 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==3,])
    mod4 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==4,])
    mod5 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==5,])
    mod6 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==6,])
    
    ### Combine all coefficients
    out <- data.frame(c(1:6), NA, NA)
    colnames(out) <- c("Ring", "coef", "int")
    out$coef[out$Ring==1] <- coefficients(mod1)[[2]]
    out$coef[out$Ring==2] <- coefficients(mod2)[[2]]
    out$coef[out$Ring==3] <- coefficients(mod3)[[2]]
    out$coef[out$Ring==4] <- coefficients(mod4)[[2]]
    out$coef[out$Ring==5] <- coefficients(mod5)[[2]]
    out$coef[out$Ring==6] <- coefficients(mod6)[[2]]
    
    out$int[out$Ring==1] <- coefficients(mod1)[[1]]
    out$int[out$Ring==2] <- coefficients(mod2)[[1]]
    out$int[out$Ring==3] <- coefficients(mod3)[[1]]
    out$int[out$Ring==4] <- coefficients(mod4)[[1]]
    out$int[out$Ring==5] <- coefficients(mod5)[[1]]
    out$int[out$Ring==6] <- coefficients(mod6)[[1]]
    
    
    #ggplot(myDF2, aes(x=Temp, flux_corrected, color=as.factor(Ring)))+
    #    geom_point() +
    #    geom_smooth(method="lm")
    
    
    myDF2$Trt[myDF2$Ring%in%c(2,3,6)] <- "aCO2"
    myDF2$Trt[myDF2$Ring%in%c(1,4,5)] <- "eCO2"
    mod7 <- lm(ln_flux~Temp, data=myDF2[myDF2$Trt=="aCO2",])
    mod8 <- lm(ln_flux~Temp, data=myDF2[myDF2$Trt=="eCO2",])
    
    pdf("R_other/Rstem_temperature_function.pdf")
    
    with(myDF2, plot(ln_flux~Temp)) 
    abline(b=out$coef[out$Ring==1],a=out$int[out$Ring==1])
    abline(b=out$coef[out$Ring==2],a=out$int[out$Ring==2], col="red")
    abline(b=out$coef[out$Ring==3],a=out$int[out$Ring==3], col="blue")
    abline(b=out$coef[out$Ring==4],a=out$int[out$Ring==4], col="orange")
    abline(b=out$coef[out$Ring==5],a=out$int[out$Ring==5], col="green")
    abline(b=out$coef[out$Ring==6],a=out$int[out$Ring==6], col="yellow")
    
    with(myDF2[myDF2$Ring==1,], plot(ln_flux~Temp, main="R1")) 
    abline(b=out$coef[out$Ring==1],a=out$int[out$Ring==1])
    
    with(myDF2[myDF2$Ring==2,], plot(ln_flux~Temp, main="R2")) 
    abline(b=out$coef[out$Ring==2],a=out$int[out$Ring==2])
    
    with(myDF2[myDF2$Ring==3,], plot(ln_flux~Temp, main="R3")) 
    abline(b=out$coef[out$Ring==3],a=out$int[out$Ring==3])
    
    with(myDF2[myDF2$Ring==4,], plot(ln_flux~Temp, main="R4")) 
    abline(b=out$coef[out$Ring==4],a=out$int[out$Ring==4])
    
    with(myDF2[myDF2$Ring==5,], plot(ln_flux~Temp, main="R5")) 
    abline(b=out$coef[out$Ring==5],a=out$int[out$Ring==5])
    
    with(myDF2[myDF2$Ring==6,], plot(ln_flux~Temp, main="R6")) 
    abline(b=out$coef[out$Ring==6],a=out$int[out$Ring==6])
    
    
    ggplot(myDF2, aes(x=Temp, flux_corrected, color=as.factor(Trt)))+
           geom_point() +
           geom_smooth(method="lm")
    
    with(myDF2, plot(ln_flux~Temp)) 
    abline(b=coefficients(mod7)[[2]],a=coefficients(mod7)[[1]])
    abline(b=coefficients(mod8)[[2]],a=coefficients(mod8)[[1]], col="red")

    dev.off()
    
    
    ### Get soil moisture into the comparison
    smc <- readRDS("temp_files/facesoilwater.RDS")
    
    myDF2$Date <- as.Date(myDF2$date.x)
    myDF2$Date <- gsub("0018", "2018", myDF2$Date)
    myDF2$Date <- as.Date(myDF2$Date)
    
    myDF3 <- merge(myDF2, smc, by=c("Date"))
    myDF3$Campaign <- month(myDF3$Date)
    
    #with(myDF3, plot(flux_corrected~VWC, col=factor(Campaign)))
    #legend("topright", legend=c("DEC", "JAN", "FEB"),
    #       col=c("red", "green", "black"), pch=1)
    
    p <- ggplot(myDF3, aes(x=VWC, y=flux_corrected, color=factor(Campaign))) +
        geom_point()
    
    pdf("R_other/stem_efflux_swc.pdf")
    
    plot(p)
    
    dev.off()
    
    ### Q10
    ### Dec 1.52
    ### Jan 1.45
    ### Feb 1.25
    ### ln (EA_S) = a + b T; Q10 = e (10 b) 
    ### a is the intercept and b is the slope between EA_S and Tstem
    
    ### To obtain continuous EA_V data from discrete EA_S measurements, 
    ### EA_S was firstly modelled as a function of temperature as described in Eqns. 1 and 2. 
    ### Secondly, continuous EA_S was expressed on a volume basis according to allometric
    ### properties of the tree obtained from cut sections of trees used for sap flow calculations (Gimeno et al., 2018)
    ### following Rodríguez-Calcerrada et al. (2015):
    ### EA_V =  2 × EA_S  × rt / (rt2- rh2)							
    ### where rt is the tree radius (including heartwood, sapwood and bark), 
    ### and rh is heartwood radius.  
    
    return(out)
    
}