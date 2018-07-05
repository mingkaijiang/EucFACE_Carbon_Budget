#- "main" module function for soil respiration (i.e., soil CO2 efflux).

make_soil_respiration_flux <- function(return.decision="data",
                                       trt.effect="abs",
                                       stat.model="interaction"){
    
    #- download the data
    download_soil_respiration()
    
    #- read in the data
    RE <- as.data.frame(data.table::fread(file.path(getToPath(), 
                                                    "FACE_P0031_RA_Rsoil-PROCESSED_20120902-20151030_L2_v1.csv")))
    RE$DateTime <- as.POSIXct(RE$DateTime,format="%Y-%m-%d %T",tz="GMT")
    RE$Date <- as.Date(RE$Date)
    
    #- read in the DAMM parameter estimates from fitting done outside this repo
    params_all_collars <- read.csv("modules/soil_respiration_flux/DAMM_parameters_all_collars.csv")
    
    
    #-----
    #- apply the DAMM model given the collar-specific parameters to predict Rsoil for each collar on each day
    
    #- get daily average met drivers for each collar
    RE$ring_collar <-factor(paste(RE$ring,RE$collar,sep="_"))
    RE.m <- data.frame(dplyr::summarize(dplyr::group_by(RE, Date, ctreat,ring,ring_collar), 
                                        Rsoil=mean(Rsoil,na.rm=T),    
                                        theta=mean(VWC,na.rm=T),
                                        Tsoil=mean(Tsoil,na.rm=T)))
    RE.m.collar <- split(RE.m,RE.m$ring_collar)
    
    
    #run the model forward with the optimized parameter set
    for (i in 1:length(RE.m.collar)){
        
        #- find the correct row to use in the parameter dataframe, extract the DAMM model parameters
        row <- which(params_all_collars$ring_collar==RE.m.collar[[i]]$ring_collar[1])
        pars <- unname(as.matrix(params_all_collars[row,c("AlphaSx","EaSx","kMSx","kMO2")]))
        RE.m.collar[[i]]$DAMM <- 0
        RE.m.collar[[i]]$DAMM <- DAMM_optim(par=pars,soilT=RE.m.collar[[i]]$Tsoil,
                                            soilM=RE.m.collar[[i]]$theta,flux=RE.m.collar[[i]]$Rsoil,
                                            #EaSx=EaSx[RE.m.collar[[i]]$ring[1]],
                                            #kMSx=kMSx[RE.m.collar[[i]]$ring[1]],
                                            #kMO2=kMO2[RE.m.collar[[i]]$ring[1]],
                                            type="predict") 
        
    }
    RE.m.collar.all <- do.call(rbind,RE.m.collar)
    RE.m.collar.all$ring <- factor(substr(RE.m.collar.all$ring_collar,1,1))
    #-----
    
    
    #- convert from umol CO2 m-2 s-1 to mg C m-2 day-1
    RE.m.collar.all$soil_respiration_flux <- RE.m.collar.all$DAMM*60*60*24*1e-6*12.01*1000
    RE.m.collar.all$ndays <- 1
    
    #- average across dates and plots
    Rsoil.out <- summaryBy(soil_respiration_flux~Date+ring,data=RE.m.collar.all,FUN=c(mean),keep.names=T)
    
    #- prepare output df
    names(Rsoil.out) <- c("Start_date","Ring","soil_respiration_flux")
    Rsoil.out$Date <- Rsoil.out$Start_date
    Rsoil.out$End_date <- Rsoil.out$Start_date
    Rsoil.out$Ring <- as.numeric(Rsoil.out$Ring)
    Rsoil.out$ndays <- as.numeric(Rsoil.out$End_date - Rsoil.out$Start_date) + 1
    Rsoil.out <- Rsoil.out[,c("Start_date","End_date","Date","Ring","soil_respiration_flux", "ndays")]
    
    colnames(RE.m.collar.all) <- c("Date","ctreat", "Ring", "Ring_collar", "Rsoil", "theta", "Tsoil", "DAMM", 
                                   "soil_respiration_flux", "ndays")
    
    ### Compute statistical analyses
    if (trt.effect == "abs") {
        if (stat.model == "dynamic") {
            source("R/stats/treatment_effect_abs_statistics_dynamic.R")
        } else if (stat.model == "no_interaction") {
            source("R/stats/treatment_effect_abs_statistics_no_interaction.R")
        } else if (stat.model == "interaction") {
            source("R/stats/treatment_effect_abs_statistics_interaction.R")
        } else {
            source("R/stats/treatment_effect_abs_statistics_no_random_effect.R")
        }
        
        s.stats <- treatment_effect_abs_statistics(inDF=RE.m.collar.all, 
                                                   var.cond="flux", var.col=9,
                                                   date.as.factor=T)
    } else if (trt.effect == "ratio") {
        if (stat.model == "dynamic") {
            source("R/stats/treatment_effect_ratio_statistics_dynamic.R")
        } else if (stat.model == "no_interaction") {
            source("R/stats/treatment_effect_ratio_statistics_no_interaction.R")
        } else if (stat.model == "interaction") {
            source("R/stats/treatment_effect_ratio_statistics_interaction.R")
        } else {
            source("R/stats/treatment_effect_ratio_statistics_no_random_effect.R")
        }
        
        s.stats <- treatment_effect_ratio_statistics(inDF=RE.m.collar.all, 
                                                     var.cond="flux", var.col=9,
                                                     date.as.factor=T)
    }
    
    ### Decision on what to return
    if (return.decision == "data") {
        return(Rsoil.out)
    } else if (return.decision == "stats") {
        return(s.stats)
    }
}