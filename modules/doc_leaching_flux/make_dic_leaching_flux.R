#- Make the dic leaching flux
make_dic_leaching_flux <- function(depth){
    
    ### Information to know: 
    ### two layers (shallow and deep), with deep assumes to be 
    ### immediately above the impermeable layer (~ 35 - 75 cm)
    ### shallow depth refer to 0 - 15 cm
    ### Assumes all DOC reaching this depth are all lost from the system
    ### May add a modeling component later, because the data were collected when soils were wet!
    ### Needs a drainage number to plug in!!!!
    
    #- download the data. 
    download_doc_data()
    
    #- read in the data 
    inDF <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILLYSIMETERNUTRIENTS_L3_20120710-20140402.csv"))
    
    inDF$date <- as.Date(inDF$date)

    #- average across rings, dates and depths
    outDF <- summaryBy(inorganic_carbon~ring+date+depth,data=inDF,FUN=mean,keep.names=T, na.rm=T)
    
    # only keep the deep depth data
    # Shun's paper suggests DOC at deep layer is constant over time
    outDF <- subset(outDF, depth == "deep")
    
    # dic leaching term converted from mg/l to mg m-2 d-1
    outDF$dic_leaching_flux <- outDF$inorganic_carbon * 0.02 # leaching estimate is simplified! 20 ml m-2 d-1
    
    #- drop NA rows
    outDF <- outDF[complete.cases(outDF),]
    
    #- add start date, currently only use the end date for represent start date
    #- because leaching should be precipitation-dependent
    outDF$Start_date <- outDF$date
    
    #- format dataframe to return
    out <- outDF[,c("Start_date", "date", "date", "ring", "dic_leaching_flux")]
    colnames(out) <- c("Start_date", "End_date", "Date", "Ring", "dic_leaching_flux")

    out$ndays <- as.numeric(out$End_date - out$Start_date) + 1
    
    # Only use data period 2012-2016
    out <- out[out$Date<="2016-12-31",]
    
    inDF$ndays <- 1
    sDF <- subset(inDF, depth == "deep")
    colnames(sDF) <- c("Date", "Ring", "Plot", "Depth", "nitrate", "ammonium",
                       "phosphate", "organic_carbon", "total_carbon", "inorganic_carbon",
                       "total_nitrogen", "ndays")

    
    ### Decision on what to return
    return(out)
 
}