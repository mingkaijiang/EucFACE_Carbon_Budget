#- Make the methane flux
make_methane_flux <- function(){
    #### returns methane flux (mg m-2 d-1)
  
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
    myDF <- do.call("rbind", list(myDF1[,c("Date", "Ring", "Final_CH4_flux")], 
                                  myDF2[,c("Date", "Ring", "Final_CH4_flux")], 
                                  myDF3[,c("Date", "Ring", "Final_CH4_flux")], 
                                  myDF4[,c("Date", "Ring", "Final_CH4_flux")], 
                                  myDF5[,c("Date", "Ring", "Final_CH4_flux")], 
                                  myDF6[,c("Date", "Ring", "Final_CH4_flux")], 
                                  myDF7[,c("Date", "Ring", "Final_CH4_flux")]))
    
    ### average across rings and dates
    myDF.m <- summaryBy(Final_CH4_flux~Date*Ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    ### need a unit conversion here (the current unit is ng cm-3, I believe)
    myDF.m$methane_flux <- myDF.m$Final_CH4_flux
    
    ### format dataframe to return
    myDF.out <- myDF.m[,c("Date","Ring","methane_flux")]
    
    return(myDF.out)
    
}