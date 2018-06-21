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
    myDF$Collar.factor <- as.numeric(myDF$Collar)
    colnames(myDF) <- c("Date", "Ring", "CO2_trtmt", "Collar", "SoilT",
                        "SoilM", "Final_CH4_flux", "CH4_flux", "Collar.factor")
    
    for (i in collar.factor) {
        ### subsetting collar
        fitDF <- subset(myDF, Collar.factor == i)
        
        obs.ch4 <- fitDF$CH4_flux
        
        ### Calls the DAMM function
        mod.fit <- nls(obs.ch4~DAMM_fit_CH4_function(#akmch4,eakmch4,
                                                     #akmo2,eakmo2,
                                                     #avmaxch4,eavmaxch4,
                                                     kMCH4,kMO2,Vmax,
                                                     SoilT,
                                                     SoilM), 
                       data=fitDF,
                       start=list(kMCH4=7,kMO2=0.1,Vmax=1.0))
        
       
        
    }
    

  
    
}
