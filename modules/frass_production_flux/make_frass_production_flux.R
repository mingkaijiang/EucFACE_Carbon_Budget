#- Make the frass C pool
make_frass_pool <- function(){
    
    # unit conversion factor
    conv <- 0.001   # not sure, check back
    
    #- download the data. 
    download_frass_data()
    
    #- read in the data.
    inDF <- read.csv("download/FACE_P0017_RA_FRASSCHEMISTRY_L2_20121112-20141016.csv")
    inDF$DATE <- as.Date(inDF$DATE)

    #- average across rings and dates
    outDF <- summaryBy(CARBON~DATE+RING,data=inDF,FUN=mean,keep.names=T)
    
    #- convert to g C m-2
    outDF$frass_pool <- outDF$CARBON * conv
    
    #- format dataframe to return
    out <- outDF[,c("DATE","RING","frass_pool")]
    colnames(out) <- c("Date", "Ring", "frass_pool")
    
    return(out)
}