make_doc_leaching_flux <- function(depth){
    
    ### Information to know: 
    ### two layers (shallow and deep), with deep assumes to be 
    ### immediately above the impermeable layer (~ 35 - 75 cm)
    ### shallow depth refer to 0 - 15 cm
    ### Assumes all DOC reaching this depth are all lost from the system

    #- read in the data 
    inDF <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILLYSIMETERNUTRIENTS_L3_20120710-20140402.csv"))
    
    inDF$date <- as.Date(inDF$date)

    #- average across rings, dates and depths
    outDF <- summaryBy(organic_carbon~ring+date+depth,data=inDF,FUN=mean,keep.names=T, na.rm=T)
    
    # only keep the deep depth data
    # Shun's paper suggests DOC at deep layer is constant over time
    outDF <- subset(outDF, depth == "deep")
    
    # doc leaching term converted from mg/l to mg m-2 d-1
    outDF$doc_leaching_flux <- outDF$organic_carbon * 0.02 # leaching estimate is simplified! 20 ml m-2 d-1
    
    #- drop NA rows
    outDF <- outDF[complete.cases(outDF),]
    
    #- add start date, currently only use the end date for represent start date
    #- because leaching should be precipitation-dependent
    outDF$Start_date <- outDF$date
    
    #- format dataframe to return
    out <- outDF[,c("Start_date", "date", "date", "ring", "doc_leaching_flux")]
    colnames(out) <- c("Start_date", "End_date", "Date", "Ring", "doc_leaching_flux")

    out$ndays <- as.numeric(out$End_date - out$Start_date) + 1
    
    # Only use data period 2012-2016
    out <- out[out$Date<="2016-12-31",]
    
    inDF$ndays <- 1
    
    ### Decision on what to return
    return(out)

}