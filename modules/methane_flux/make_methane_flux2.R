#- Make the methane flux
#### This is a script to gap-fill CH4 data based on DAMM model
make_methane_flux2 <- function() {
    #### returns methane flux (mg m-2 d-1)
    
    #### final flux data is the yearly aggregated data
    #### Information on the data processing is available on HIEv
  
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
    
    ### Model prediction
    #- read in the DAMM parameter estimates from fitting done outside this repo
    params_all_collars <- read.csv("modules/methane_flux/DAMM_parameters_all_collars_CH4.csv")
    params_all_collars$ring_collar <- sapply(as.character(params_all_collars$ring_collar),gsub,pattern="_",replacement="\\.")
    params_all_collars$ring_collar <- as.factor(as.numeric(params_all_collars$ring_collar))
    
    ### get unique collar factors
    collar.factor <- as.numeric(unique(myDF$Collar))
    params_all_collars <- params_all_collars[which(params_all_collars$ring_collar %in% collar.factor),]
    params_all_collars$ring_collar <- droplevels(params_all_collars$ring_collar)
    params_all_collars$AlphaCH4 <- -900000

    #-----
    #- apply the DAMM model given the collar-specific parameters to predict CH4 for each collar on each day
    
    #- get daily average met drivers for each collar
    myDF$ring_collar <-factor(myDF$Collar)
    myDF.m <- data.frame(dplyr::summarize(dplyr::group_by(myDF, Date, CO2_trtmt, Ring, ring_collar), 
                                        CH4=mean(CH4_flux,na.rm=T),    
                                        theta=mean(Av_SoilMoisture,na.rm=T),
                                        Tsoil=mean(Av_SoilTemp,na.rm=T)))
    myDF.m.collar <- split(myDF.m,myDF.m$ring_collar)
    

    #run the model forward with the optimized parameter set
    for (i in 1:length(myDF.m.collar)){
        
        #- find the correct row to use in the parameter dataframe, extract the DAMM model parameters
        row <- which(params_all_collars$ring_collar==myDF.m.collar[[i]]$ring_collar[1])
        pars <- unname(as.matrix(params_all_collars[row,c("AlphaCH4","EaCH4","kMCH4","kMO2")]))
        myDF.m.collar[[i]]$DAMM <- 0
        myDF.m.collar[[i]]$DAMM <- DAMM_optim_CH4(par=pars,soilT=myDF.m.collar[[i]]$Tsoil,
                                            soilM=myDF.m.collar[[i]]$theta,flux=myDF.m.collar[[i]]$CH4,
                                            type="predict") 
        
    }
    myDF.m.collar.all <- do.call(rbind,myDF.m.collar)
    myDF.m.collar.all$ring <- factor(substr(myDF.m.collar.all$ring_collar,1,1))
    #-----
    head(myDF.m.collar.all)
    with(myDF.m.collar.all, plot(CH4~DAMM))
    
    
    
    #- convert from umol CH4 m-2 s-1 to mg C m-2 day-1
    myDF.m.collar.all$methane_flux <- myDF.m.collar.all$DAMM*60*60*24*1e-6*12.01*1000

    ### average across rings and dates
    myDF.out <- summaryBy(methane_flux~Date*Ring,data=myDF.m.collar.all,FUN=mean,keep.names=T,na.rm=T)
    
    myDF.out$Date <- as.Date(as.character(myDF.out$Date), format = "%d-%b-%y")
    
    ### Start and end date are the same
    myDF.out$Start_date <- myDF.out$Date
    myDF.out$End_date <- myDF.out$Date
    myDF.out$ndays <- as.numeric(myDF.out$End_date - myDF.out$Start_date) + 1
    
    return(myDF.out)
    
}