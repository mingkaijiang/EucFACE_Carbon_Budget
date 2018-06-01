#- Make the methane flux
make_methane_flux <- function(){
    #### returns methane flux (mg m-2 d-1)
    
    #### final flux data is the yearly aggregated data
    #### Information on the data processing is available on HIEv
    #### not 100% sure about the unit, I think it's ng C cm-3 yr-1
    #### This cm-3 is tricky, it is most likely the volume of the soil collar,
    #### which has ~5 cm in soil and ~5 cm aboveground, but also, 
    #### it needs to include the volume of the chamber!!!
    #### We just don't have an exact number for this volume!
    #### Also, this yearly aggregated thing is also problematic:
    #### Is it scaled with soil temperature, or is it simply a daily rate multiplied by 365?
  
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
    myDF$Final_CH4_flux <- as.numeric(as.character(myDF$Final_CH4_flux))
    
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
    
    return(myDF.out)
    
}