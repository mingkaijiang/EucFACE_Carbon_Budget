#- Alexis's Rsoil 

make_soil_respiration_flux_2 <- function(){
    
    ### Rsoil unit is umol CO2 m-2 s-1
    ### "qcRsoil" of 0 is model (DAMM filling) 
    ### "qcRsoil" of 1 is data (good qc data is available)
    
    # read in data
    myDF <- read.csv("data/EucFACE_Rsoil_2012_2018_filled_ind_qc.csv")
    
    # enter date info
    myDF$DateTime <- as.character(myDF$DateTime)
    myDF$Date <- as.Date(myDF$DateTime, format = "%d-%b-%Y %H:%M:%S")
    
    # only include pre 2017 data
    myDF2 <- subset(myDF, Date < "2017-01-01")
    myDF2 <- subset(myDF2, Date >"2012-09-06")
    
    # correct for unit from umol CO2 m-2 s-1 to mg C m-2 30 mins
    myDF2$R1<- myDF2$Rsoil_R1*60*30*1e-6*12.01*1000
    myDF2$R2<- myDF2$Rsoil_R2*60*30*1e-6*12.01*1000
    myDF2$R3<- myDF2$Rsoil_R3*60*30*1e-6*12.01*1000
    myDF2$R4<- myDF2$Rsoil_R4*60*30*1e-6*12.01*1000
    myDF2$R5<- myDF2$Rsoil_R5*60*30*1e-6*12.01*1000
    myDF2$R6<- myDF2$Rsoil_R6*60*30*1e-6*12.01*1000
    
    # generate daily Rsoil
    myDF3 <- summaryBy(R1+R2+R3+R4+R5+R6~Date, FUN=sum, data=myDF2, keep.names=T)
    
    # conver into long format
    myDF4 <- melt(myDF3, id.vars = c("Date"))
    
    # make Ring
    myDF4$variable <- gsub("R", "", myDF4$variable)
    myDF4$variable <- as.numeric(myDF4$variable)
    
    #- average across dates and plots
    names(myDF4)[2] <- "Ring"
    names(myDF4)[3] <- "soil_respiration_flux"
    myDF4$Start_date <- myDF4$Date
    myDF4$End_date <- myDF4$Date
    myDF4$ndays <- as.numeric(myDF4$Date-myDF4$Start_date+1)
    myDF4 <- myDF4[,c("Start_date", "End_date", "Date", "Ring", "soil_respiration_flux", "ndays")]

    ### Decision on what to return
    return(myDF4)

}