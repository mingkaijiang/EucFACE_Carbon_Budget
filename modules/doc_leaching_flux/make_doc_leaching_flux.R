#- Make the doc leaching flux
make_doc_leaching_flux <- function(){
    
    ### Information to know: 
    ### two layers (shallow and deep), with deep assumes to be 
    ### immediately above the impermeable layer (~ 35 - 75 cm)
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
    outDF <- summaryBy(organic_carbon~ring+date+depth,data=inDF,FUN=mean,keep.names=T, na.rm=T)
    
    # only keep the deep depth data
    outDF <- subset(outDF, depth == "deep")
    
    #- count number of days between two dates  -- this could be a function itself
    #d <- unique(outDF$date)
    #first <- c()
    #for (i in seq_along(d))
    #    first[i] <- d[i] - d[1]
    
    #between <- c(0, diff(d))
    
    #- convert into ml l-1 d-1
    #outDF$ndays <- rep(between, each = 6)  # the length is unequal for rings
    #outDF$doc_leaching_flux <- outDF$organic_carbon/outDF$ndays # a unit conversion term is needed here  
    
    # doc leaching term as per day
    outDF$doc_leaching_flux <- outDF$organic_carbon   # Shun's paper suggests DOC at deep layer is constant over time
  
    # need to add soil water data from Neutron Probe datasets and soil bulk density
    
    #- drop NA rows
    outDF <- outDF[complete.cases(outDF),]
    
    #- format dataframe to return
    out <- outDF[,c("date","ring","doc_leaching_flux")]
    colnames(out) <- c("Date", "Ring", "doc_leaching_flux")

    return(out)
}