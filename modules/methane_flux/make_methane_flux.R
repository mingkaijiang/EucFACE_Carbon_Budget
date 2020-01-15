#- Make the methane flux
#### This is a simplified approach without gap-filling
make_methane_flux <- function() {
    #### returns methane flux (mg m-2 d-1)
    
    #### final flux data is the yearly aggregated data
    #### Information on the data processing is available on HIEv

    ### read in the csv
    myDF1 <- read.csv("data/EucFACE_data/FACE_P0027_RA_GHG-FLUXES_L3_20130101-20131231 V3.csv")
    myDF2 <- read.csv("data/EucFACE_data/FACE_P0027_RA_GHG-FLUXES_L3_20140101-20141231.csv")
    myDF3 <- read.csv("data/EucFACE_data/FACE_P0027_RA_GHG-FLUXES_L3_20150101-20151231.csv")
    myDF4 <- read.csv("data/EucFACE_data/FACE_P0027_RA_GHG-FLUXES_L3_20160113.csv")
    myDF5 <- read.csv("data/EucFACE_data/FACE_P0027_RA_GHG-FLUXES_L3_20160218.csv")
    myDF6 <- read.csv("data/EucFACE_data/FACE_P0027_RA_GHG-FLUXES_L3_20160314.csv")
    myDF7 <- read.csv("data/EucFACE_data/FACE_P0027_RA_GHG-FLUXES_L3_20160420.csv")
    
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
    myDF$Date <- as.Date(as.character(myDF$Date), format="%d-%b-%y")
    
    myDF.m <- summaryBy(Final_CH4_flux~Date*Ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    ### need a unit conversion here (the current unit is ug C/m2/h)
    myDF.m$methane_flux <- myDF.m$Final_CH4_flux / 1000 * 24
    
    ### format dataframe to return
    myDF.out <- myDF.m[,c("Date","Ring","methane_flux")]

    #myDF.out$Date <- as.Date(as.character(myDF.out$Date), format = "%d-%b-%y")
    
    ### Start and end date are the same
    myDF.out$Start_date <- myDF.out$Date
    myDF.out$End_date <- myDF.out$Date
    myDF.out$ndays <- as.numeric(myDF.out$End_date - myDF.out$Start_date) + 1
    
    ### Fill the two NAs
    myDF.out$methane_flux[is.na(myDF.out$methane_flux)] <- 0
    
    # Only use data period 2012-2016
    myDF.out <- myDF.out[myDF.out$Date<="2016-12-31",]
 
    
    ### Decision on what to return
    return(myDF.out)

    
}