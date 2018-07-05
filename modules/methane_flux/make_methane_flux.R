#- Make the methane flux
#### This is a simplified approach without gap-filling
make_methane_flux <- function(return.decision="data",
                              trt.effect="abs",
                              stat.model="interaction") {
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
    myDF <- do.call("rbind", list(myDF1[,c("Date", "Ring", "Final_CH4_flux", "Collar")], 
                                  myDF2[,c("Date", "Ring", "Final_CH4_flux", "Collar")], 
                                  myDF3[,c("Date", "Ring", "Final_CH4_flux", "Collar")], 
                                  myDF4[,c("Date", "Ring", "Final_CH4_flux", "Collar")], 
                                  myDF5[,c("Date", "Ring", "Final_CH4_flux", "Collar")], 
                                  myDF6[,c("Date", "Ring", "Final_CH4_flux", "Collar")], 
                                  myDF7[,c("Date", "Ring", "Final_CH4_flux", "Collar")]))
    
    ### average across rings and dates
    myDF$Final_CH4_flux <- as.numeric(as.character(myDF$Final_CH4_flux))
    myDF$methane_flux <- myDF$Final_CH4_flux / 1000 * 24
    myDF$ndays <- 1
    
    myDF.m <- summaryBy(Final_CH4_flux~Date*Ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    ### need a unit conversion here (the current unit is ug C/m2/h)
    myDF.m$methane_flux <- myDF.m$Final_CH4_flux / 1000 * 24
    
    ### format dataframe to return
    myDF.out <- myDF.m[,c("Date","Ring","methane_flux")]

    myDF.out$Date <- as.Date(as.character(myDF.out$Date), format = "%d-%b-%y")
    
    ### Start and end date are the same
    myDF.out$Start_date <- myDF.out$Date
    myDF.out$End_date <- myDF.out$Date
    myDF.out$ndays <- as.numeric(myDF.out$End_date - myDF.out$Start_date) + 1
    
    
    ### Compute statistical analyses
    if (return.decision=="stats") {
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
            
            s.stats <- treatment_effect_abs_statistics(inDF=myDF, 
                                                       var.cond="flux", var.col=5,
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
            
            s.stats <- treatment_effect_ratio_statistics(inDF=myDF, 
                                                         var.cond="flux", var.col=5,
                                                         date.as.factor=T)
        }
    }

    
    ### Decision on what to return
    if (return.decision == "data") {
        return(myDF.out)
    } else if (return.decision == "stats") {
        return(s.stats)
    }
    
}