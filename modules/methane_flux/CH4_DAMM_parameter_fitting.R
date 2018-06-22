CH4_DAMM_parameter_fitting <- function() {
    ### download the data
    download_methane_data()
    
    ### read in the csv
    myDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_P0027_RA_GHG-FLUXES_L3_20130101-20131231 V3.csv"))
    myDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_P0027_RA_GHG-FLUXES_L3_20140101-20141231.csv"))
    myDF3 <- read.csv(file.path(getToPath(), 
                                "FACE_P0027_RA_GHG-FLUXES_L3_20150101-20151231.csv"))
    myDF4 <- read.csv(file.path(getToPath(), 
                                "FACE_P0027_RA_GHG-FLUXES_L3_20160113.csv"))
    myDF5 <- read.csv(file.path(getToPath(), 
                                "FACE_P0027_RA_GHG-FLUXES_L3_20160218.csv"))
    myDF6 <- read.csv(file.path(getToPath(), 
                                "FACE_P0027_RA_GHG-FLUXES_L3_20160314.csv"))
    myDF7 <- read.csv(file.path(getToPath(), 
                                "FACE_P0027_RA_GHG-FLUXES_L3_20160420.csv"))
    
    ### combine all data
    myDF <- do.call("rbind", list(myDF1[,c("Date", "Ring", "CO2_trtmt", "Collar", "Av_SoilTemp", "Av_SoilMoisture", "Final_CH4_flux")], 
                                  myDF2[,c("Date", "Ring", "CO2_trtmt", "Collar", "Av_SoilTemp", "Av_SoilMoisture", "Final_CH4_flux")], 
                                  myDF3[,c("Date", "Ring", "CO2_trtmt", "Collar", "Av_SoilTemp", "Av_SoilMoisture", "Final_CH4_flux")], 
                                  myDF4[,c("Date", "Ring", "CO2_trtmt", "Collar", "Av_SoilTemp", "Av_SoilMoisture", "Final_CH4_flux")], 
                                  myDF5[,c("Date", "Ring", "CO2_trtmt", "Collar", "Av_SoilTemp", "Av_SoilMoisture", "Final_CH4_flux")], 
                                  myDF6[,c("Date", "Ring", "CO2_trtmt", "Collar", "Av_SoilTemp", "Av_SoilMoisture", "Final_CH4_flux")], 
                                  myDF7[,c("Date", "Ring", "CO2_trtmt", "Collar", "Av_SoilTemp", "Av_SoilMoisture", "Final_CH4_flux")]))
    
    ### make sure they are real numbers
    myDF$Final_CH4_flux <- as.numeric(as.character(myDF$Final_CH4_flux))
    myDF$Av_SoilMoisture <- as.numeric(as.character(myDF$Av_SoilMoisture)) / 100
    myDF$Av_SoilTemp <- as.numeric(as.character(myDF$Av_SoilTemp))
    myDF$Collar <- as.character(myDF$Collar)
    
    ### Convert CH4 flux from ug C m-2 h-1 to umol CH4 m-2 s-1
    myDF$CH4_flux <- myDF$Final_CH4_flux / 12 / 3600
    
    ### get unique collar factors
    collar.factor <- as.numeric(unique(myDF$Collar))
    
    ### Create a df to store fitted parameters
    paramDF <- data.frame(collar.factor, NA, NA, NA, NA, NA, NA)
    colnames(paramDF) <- c("ring_collar", "akmo2", "eakmo2", 
                           "akmch4", "eakmch4", "avmaxch4", "eavmaxch4")
    
    ### Do Fitting 
    set.seed(1234)
    
    myDF$Collar.factor <- as.numeric(myDF$Collar)
    colnames(myDF) <- c("Date", "Ring", "CO2_trtmt", "Collar", "SoilT",
                        "SoilM", "Final_CH4_flux", "CH4_flux", "Collar.factor")
    myDF <- myDF[complete.cases(myDF),]
    
    ## Set initial parameter values
    akmch4 <- -100000
    eakmch4 <- -0.1
    akmo2 <- -1.0
    eakmo2 <- -0.1
    avmaxch4 <- -40
    eavmaxch4 <- -0.1
    params <- c(akmch4, eakmch4, akmo2, eakmo2, avmaxch4, eavmaxch4)
    
    ## Set boundaries
    lower <- c(-100000,-0.1,-1.0,-0.1,-40,-0.1)
    upper <- c(100000,0.1,1.0,0.1,40,0.1)
    
    ## Create fitted parameter storage unit
    out.fit.params <- data.frame(collar.factor, NA, NA, NA, NA, NA, NA)
    colnames(out.fit.params) <- c("collar.factor", "akmch4", "eakmch4", "akmo2",
                                  "eakmo2", "avmaxch4", "eavmaxch4")
    
    ## Loop through collars to do fitting
    for (i in collar.factor) {
        ### subsetting collar
        fitDF <- subset(myDF, Collar.factor == i)
        
        ### Calls the DAMM function
        mod.fit <- DEoptim(fn=DAMM_fit_CH4_function,lower=lower,upper=upper,
                           control=list(NP=500,itermax=100,trace=TRUE,CR=0.9),
                           soilT=fitDF$SoilT, soilM=fitDF$SoilM, flux=fitDF$CH4_flux)
        out.fit.params[out.fit.params$collar.factor == i, 2:7] <- unname(mod.fit$optim$bestmem)
    }

    ### Check fitting performance
    pdf("R_other/fitting_CH4_parameters_evaluation.pdf")
    for (i in collar.factor) {
        obsDF <- subset(myDF, Collar.factor == i)
        fit.params <- as.numeric(unname(out.fit.params[out.fit.params$collar.factor==i,2:7]))
        fit.data <- DAMM_fit_CH4_function(params=fit.params,soilT=obsDF$SoilT, soilM=obsDF$SoilM,
                                          flux=obsDF$CH4_flux, type="predict")
        
        plot(obsDF$CH4_flux, fit.data, main = paste0("1:1 line, Collar = ", i), xlab = "obs", ylab = "pred")
        abline(0,1)
        
        obsDF$fit <- fit.data
        obsDF$Date <- as.Date(as.character(obsDF$Date), "%d-%b-%y")
        with(obsDF, plot(CH4_flux~Date,col="black", type="b", ylab = "CH4 flux (umol CH4 m-2 s-1)"))
        with(obsDF, points(fit~Date, col="red", type="b"))
        legend("topleft", c("obs", "pred"), fill=c("black", "red"))
        
    }

    dev.off()
    
}
