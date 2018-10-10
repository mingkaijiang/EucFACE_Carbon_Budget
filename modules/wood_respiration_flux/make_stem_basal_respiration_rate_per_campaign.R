make_stem_basal_respiration_rate_per_campaign <- function() {
    
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
    
    ### add month information
    myDF2$Date <- as.Date(myDF2$date.x)
    myDF2$Date <- gsub("0018", "2018", myDF2$Date)
    myDF2$Date <- as.Date(myDF2$Date)
    myDF2$Campaign <- month(myDF2$Date)
    
    ### generate linear model 
    mod_jan_1 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==1&myDF2$Campaign=="1",])
    mod_jan_2 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==2&myDF2$Campaign=="1",])
    mod_jan_3 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==3&myDF2$Campaign=="1",])
    mod_jan_4 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==4&myDF2$Campaign=="1",])
    mod_jan_5 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==5&myDF2$Campaign=="1",])
    mod_jan_6 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==6&myDF2$Campaign=="1",])
    
    mod_feb_1 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==1&myDF2$Campaign=="2",])
    mod_feb_2 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==2&myDF2$Campaign=="2",])
    mod_feb_3 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==3&myDF2$Campaign=="2",])
    mod_feb_4 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==4&myDF2$Campaign=="2",])
    mod_feb_5 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==5&myDF2$Campaign=="2",])
    mod_feb_6 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==6&myDF2$Campaign=="2",])
    
    mod_dec_1 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==1&myDF2$Campaign=="12",])
    mod_dec_2 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==2&myDF2$Campaign=="12",])
    mod_dec_3 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==3&myDF2$Campaign=="12",])
    mod_dec_4 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==4&myDF2$Campaign=="12",])
    mod_dec_5 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==5&myDF2$Campaign=="12",])
    mod_dec_6 <- lm(ln_flux~Temp, data=myDF2[myDF2$Ring==6&myDF2$Campaign=="12",])
    
    ### Combine all coefficients
    out <- data.frame(rep(c("Jan", "Feb", "Dec"), each=6), rep(c(1:6),3), NA, NA)
    colnames(out) <- c("Campaign", "Ring", "coef", "int")
    out$coef[out$Campaign=="Jan"&out$Ring==1] <- coefficients(mod_jan_1)[[2]]
    out$coef[out$Campaign=="Jan"&out$Ring==2] <- coefficients(mod_jan_2)[[2]]
    out$coef[out$Campaign=="Jan"&out$Ring==3] <- coefficients(mod_jan_3)[[2]]
    out$coef[out$Campaign=="Jan"&out$Ring==4] <- coefficients(mod_jan_4)[[2]]
    out$coef[out$Campaign=="Jan"&out$Ring==5] <- coefficients(mod_jan_5)[[2]]
    out$coef[out$Campaign=="Jan"&out$Ring==6] <- coefficients(mod_jan_6)[[2]]
    
    out$int[out$Campaign=="Jan"&out$Ring==1] <- coefficients(mod_jan_1)[[1]]
    out$int[out$Campaign=="Jan"&out$Ring==2] <- coefficients(mod_jan_2)[[1]]
    out$int[out$Campaign=="Jan"&out$Ring==3] <- coefficients(mod_jan_3)[[1]]
    out$int[out$Campaign=="Jan"&out$Ring==4] <- coefficients(mod_jan_4)[[1]]
    out$int[out$Campaign=="Jan"&out$Ring==5] <- coefficients(mod_jan_5)[[1]]
    out$int[out$Campaign=="Jan"&out$Ring==6] <- coefficients(mod_jan_6)[[1]]
    
    out$coef[out$Campaign=="Feb"&out$Ring==1] <- coefficients(mod_feb_1)[[2]]
    out$coef[out$Campaign=="Feb"&out$Ring==2] <- coefficients(mod_feb_2)[[2]]
    out$coef[out$Campaign=="Feb"&out$Ring==3] <- coefficients(mod_feb_3)[[2]]
    out$coef[out$Campaign=="Feb"&out$Ring==4] <- coefficients(mod_feb_4)[[2]]
    out$coef[out$Campaign=="Feb"&out$Ring==5] <- coefficients(mod_feb_5)[[2]]
    out$coef[out$Campaign=="Feb"&out$Ring==6] <- coefficients(mod_feb_6)[[2]]
    
    out$int[out$Campaign=="Feb"&out$Ring==1] <- coefficients(mod_feb_1)[[1]]
    out$int[out$Campaign=="Feb"&out$Ring==2] <- coefficients(mod_feb_2)[[1]]
    out$int[out$Campaign=="Feb"&out$Ring==3] <- coefficients(mod_feb_3)[[1]]
    out$int[out$Campaign=="Feb"&out$Ring==4] <- coefficients(mod_feb_4)[[1]]
    out$int[out$Campaign=="Feb"&out$Ring==5] <- coefficients(mod_feb_5)[[1]]
    out$int[out$Campaign=="Feb"&out$Ring==6] <- coefficients(mod_feb_6)[[1]]
    
    out$coef[out$Campaign=="Dec"&out$Ring==1] <- coefficients(mod_dec_1)[[2]]
    out$coef[out$Campaign=="Dec"&out$Ring==2] <- coefficients(mod_dec_2)[[2]]
    out$coef[out$Campaign=="Dec"&out$Ring==3] <- coefficients(mod_dec_3)[[2]]
    out$coef[out$Campaign=="Dec"&out$Ring==4] <- coefficients(mod_dec_4)[[2]]
    out$coef[out$Campaign=="Dec"&out$Ring==5] <- coefficients(mod_dec_5)[[2]]
    out$coef[out$Campaign=="Dec"&out$Ring==6] <- coefficients(mod_dec_6)[[2]]
    
    out$int[out$Campaign=="Dec"&out$Ring==1] <- coefficients(mod_dec_1)[[1]]
    out$int[out$Campaign=="Dec"&out$Ring==2] <- coefficients(mod_dec_2)[[1]]
    out$int[out$Campaign=="Dec"&out$Ring==3] <- coefficients(mod_dec_3)[[1]]
    out$int[out$Campaign=="Dec"&out$Ring==4] <- coefficients(mod_dec_4)[[1]]
    out$int[out$Campaign=="Dec"&out$Ring==5] <- coefficients(mod_dec_5)[[1]]
    out$int[out$Campaign=="Dec"&out$Ring==6] <- coefficients(mod_dec_6)[[1]]
    
    pdf("R_other/Rstem_reproduction_Roberto.pdf")
    p1 <- ggplot(myDF2, aes(x=Temp, flux_corrected, color=as.factor(Campaign)))+
        geom_point() +
        geom_smooth(method="lm")
    plot(p1)
    dev.off()
    
    
    return(out)
    
}