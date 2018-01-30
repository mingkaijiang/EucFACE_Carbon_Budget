#- Make the frass C flux
make_frass_production_flux <- function() {
    
    # Fo use frassfall data and frass carbon content data to obtain frass production flux.
    # Frassfall data has longer temporal coverage,
    # frass carbon content is quite constant over time and across rings.
    # Currently only time points that have both frassfall and frass C data are included.
    # May need to consider just using one frass C content coefficient across time,
    # so that more temporal coverage can be provided. 
    # returns: frass production per day (mg/d), averaged across start and date dates
    
    #- download the data. 
    download_frass_data()
    
    #- read in the data - frassfall data (in unit of g/0.1979 m2)
    inDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_P0017_RA_FRASSFALL_L2_20120914-20150209.csv"))
    inDF1$DATE <- as.Date(inDF1$DATE)
    
    #- read in the data - frass chemistry data (for C, in unit of %)
    inDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_P0017_RA_FRASSCHEMISTRY_L2_20121112-20141016.csv"))
    inDF2$DATE <- as.Date(inDF2$DATE)
    
    #- average across rings and dates - frassfall data
    outDF1 <- summaryBy(FRASSFALL~DATE+RING,data=inDF1,FUN=mean,keep.names=T)
    
    #- average across rings and dates
    outDF2 <- summaryBy(CARBON~DATE+RING,data=inDF2,FUN=mean,keep.names=T)
    
    #- merge by dates
    outDF <- merge(outDF1, outDF2, by=c("DATE","RING"), all.x=TRUE, all.y=FALSE)
    
    #- convert to g C m-2 (area of the basket = 0.1979 m2)
    outDF$frass_production <- outDF$CARBON /100.0 * outDF$FRASSFALL / frass_basket_area
    
    #- drop the last 6 entries as they are NA
    outDF <- outDF[1:174,]
    
    #- count number of days between two dates  
    d <- unique(outDF$DATE)
    b <- count_ndays(d)
    
    #- convert into mg m-2 d-1
    outDF$ndays <- rep(b, each = 6)
    
    out <- dplyr::mutate(outDF, 
                         Date = as.Date(outDF$DATE, format = "%d/%m/%Y"),
                         Start_date = Date - ndays,
                         End_date = Date,
                         frass_production_flux = frass_production * g_to_mg / ndays)
    
    #- drop NA rows
    out <- out[complete.cases(out),]
    
    #- format dataframe to return
    out <- out[,c("Start_date", "End_date", "DATE", "RING","frass_production_flux")]
    colnames(out) <- c("Start_date", "End_date", "Date", "Ring", "frass_production_flux")
    
    return(out)
}

