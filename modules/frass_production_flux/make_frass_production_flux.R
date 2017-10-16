#- Make the frass C flux
make_frass_production_flux <- function(){
    
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
    outDF$frass_production_flux <- outDF$frass_production/outDF$ndays * g_to_mg
    
    # add start and end date
    outDF$End_date <- rep(d[1:length(d)], each=6)
    outDF$Start_date <- outDF$End_date
    outDF[7:174, "Start_date"] <- rep(d[1:(length(d)-1)], each=6)
    outDF$Start_date[1:6] <- NA
    
    #- drop NA rows
    outDF <- outDF[complete.cases(outDF),]
    
    #- format dataframe to return
    out <- outDF[,c("Start_date", "End_date", "RING","frass_production_flux")]
    colnames(out) <- c("Start_date", "End_date", "Ring", "frass_production_flux")

    return(out)
}

# function to count number of days in between the dates
count_ndays <- function(d) {
    
    # d: list of dates in the input dataframe
    # f: number of days difference from the first date
    # b: number of days in between the dates
    
    f <- c()
    
    for (i in seq_along(d))
    {
        f[i] <- d[i] - d[1]
    }
    
    b <- c(0, diff(d))
    
    return(b)
}
