#- Make the lerp C prodution flux
make_lerp_production_flux <- function(c_frac) {
    
    ### The life cycle of lerp is ~ 1 month, and the frass baskets were emptied every 1 month,
    ### So this flux calculated from frass basket measurements is the lerp biomass as well,
    ### Exception: the life cycle of lerp is a bit longer in July. 
    ### Data were collected on a monthly basis, but the sample processings were conducted quarterly.

    #- download the data. 
    download_lerp_data()
    
    #- read in the data - abundance data (in unit of individual/0.1979 m2)
    abDF <- read.csv(file.path(getToPath(), 
                                "FACE_P0017_RA_psyllid_abundance_L3_20121101-20141219.csv"))

    #- read in the data - lerp weight data (in unit of mg/individual)
    wtDF <- read.csv(file.path(getToPath(), 
                                "FACE_P0017_RA_psyllid_lerp_weight_L3_20121201-20141219.csv"))
    names(wtDF) <- c("species", "tube", "trap", "date", "co2", "ring", "weight")
    

    #- average across rings, traps, species and dates - abundance data
    outDF1 <- summaryBy(counts ~ ring+trap+species+date,data=abDF,FUN=mean,keep.names=TRUE)
    
    #- average across rings, traps, species and dates - weight data
    outDF2 <- summaryBy(weight ~ ring+trap+species+date,data=wtDF,FUN=mean,keep.names=TRUE)

    #- merge by dates
    outDF3 <- merge(outDF1, outDF2, by=c("date","ring", "species", "trap"), all.x=TRUE, all.y=FALSE)
    
    #- multiply by carbon content (0.78 estimated by Andrew)
    #- unit: mg C m-2 d-1
    #- question: area of the trap needed or not?
    outDF3$lerp_production_flux <- outDF3$counts * outDF3$weight * c_frac / frass_basket_area / ndays_in_month
        
    #- drop NA rows
    outDF3 <- outDF3[complete.cases(outDF3),]
    
    #- sum all species for each trap
    outDF4 <- summaryBy(lerp_production_flux ~ ring+trap+date,data=outDF3,FUN=sum,keep.names=T)
    
    #- average across all traps for each ring and date
    outDF5 <- summaryBy(lerp_production_flux ~ ring+date,data=outDF4,FUN=mean,keep.names=T)
    
    out <- outDF5
    
    # Fix Date format:
    # Assume that e.g. 'Jan-13' means the last day of that month (2013-01-31).
    out$End_date <- as.Date(paste0("1-", out$date), format = "%d-%b-%y") + months(1) - days(1)
    out$Start_date <- as.Date(paste0("1-", out$date), format = "%d-%b-%y") 
    
    #- format dataframe to return
    out <- out[,c("Start_date", "End_date","date", "ring","lerp_production_flux")]
    colnames(out) <- c("Start_date", "End_date", "Date", "Ring", "lerp_production_flux")
    
    return(out)
}

