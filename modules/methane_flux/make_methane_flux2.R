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
    # predict DAMM model parameters
    CH4_DAMM_parameter_fitting(myDF)
    
    
    ### Download soil T and M data
    RE <- as.data.frame(data.table::fread(file.path(getToPath(), 
                                                    "FACE_P0031_RA_Rsoil-PROCESSED_20120902-20151030_L2_v1.csv")))
    RE$DateTime <- as.POSIXct(RE$DateTime,format="%Y-%m-%d %T",tz="GMT")
    RE$Date <- as.Date(RE$Date)
    
    
    #- read in the DAMM parameter estimates from fitting done outside this repo
    params_all_collars <- read.csv("modules/methane_flux/DAMM_parameters_all_collars_CH4.csv")
    params_all_collars$ring_collar <- sapply(as.character(params_all_collars$collar.factor),gsub,pattern="_",replacement="\\.")
    params_all_collars$ring_collar <- as.factor(as.numeric(params_all_collars$ring_collar))
    
    ### get unique collar factors
    collar.factor <- as.numeric(unique(myDF$Collar))
    params_all_collars <- params_all_collars[which(params_all_collars$ring_collar %in% collar.factor),]
    params_all_collars$ring_collar <- droplevels(params_all_collars$ring_collar)

    #-----
    #- apply the DAMM model given the collar-specific parameters to predict CH4 for each collar on each day
    
    #- get daily average met drivers for each collar
    RE$ring_collar <-factor(as.numeric(paste(RE$ring,RE$collar,sep=".")))
    RE.m <- data.frame(dplyr::summarize(dplyr::group_by(RE, Date, ctreat,ring,ring_collar), 
                                        Rsoil=mean(Rsoil,na.rm=T),    
                                        theta=mean(VWC,na.rm=T),
                                        Tsoil=mean(Tsoil,na.rm=T)))
    
    # shorten the collar list
    collar.list <- unique(RE.m$ring_collar)
    
    # out df
    outDF <- RE.m
    outDF$DAMM <- 0

    #run the model forward with the optimized parameter set
    for (i in collar.list){
        
        #- find the correct row to use in the parameter dataframe, extract the DAMM model parameters
        pars <- unname(as.matrix(params_all_collars[params_all_collars$ring_collar==i,
                                                    c("akmch4","eakmch4","akmo2","eakmo2","avmaxch4","eavmaxch4")]))

        inDF <- subset(RE.m, ring_collar==i)
        outDF[outDF$ring_collar==i,"DAMM"] <- DAMM_fit_CH4_function(params=pars,soilT=inDF$Tsoil,
                                                                    soilM=inDF$theta,flux=inDF$Rsoil,
                                                                    type="predict") 
        
    }
    
    
    #- convert from umol CH4 m-2 s-1 to mg C m-2 day-1
    outDF$methane_flux <- outDF$DAMM*60*60*24*1e-6*12.01*1000
    outDF$Date <- as.Date(as.character(outDF$Date), "%Y-%m-%d")
    
    ### average across rings and dates
    myDF.out <- summaryBy(methane_flux~Date*ring,data=outDF,FUN=mean,keep.names=T,na.rm=T)
    
    ### Start and end date are the same
    myDF.out$Start_date <- myDF.out$Date
    myDF.out$End_date <- myDF.out$Date
    myDF.out$ndays <- as.numeric(myDF.out$End_date - myDF.out$Start_date) + 1
    
    colnames(myDF.out) <- c("Date", "Ring", "methane_flux", "Start_date", "End_date", "ndays")
        
    return(myDF.out)
    
}