#- Make the fineroot c production flux
make_fineroot_production_flux <- function(c_frac,
                                          return.decision="data",
                                          trt.effect="abs",
                                          stat.model="interaction"){
    
    # returns fine root production flux (mg m-2 d-1)
    # 5 time points, possibly measured at quarterly timesteps

    download_fineroot_data()

    #- read in the csv
    frp1 <- read.csv(file.path(getToPath(), 
                               "FACE_P0083_RA_FR-PRODUCTION_L1_20140601-20150915.csv"))
    
    # Fix Date format:
    # Assume that e.g. 'Jan-13' means the last day of that month (2013-01-31).
    frp1$Date <- as.Date(paste0("1-", frp1$Date), format = "%d-%b-%y") + months(1) - days(1)
    
    names(frp1)[2] <- "Ring"
    names(frp1)[8] <- "frp_tot"
    
    #- average across rings and dates
    frp.m <- summaryBy(frp_tot~Date+Ring,data=frp1,FUN=mean,keep.names=T,na.rm=T)
    
    #- convert to mg C m-2 day-1
    frp.m$fineroot_production_flux <- frp.m$frp_tot*c_frac*1000
    
    #- add 3 months before the first date
    frp.m$End_date <- frp.m$Date
    time.list1 <- unique(frp.m$Date)
    time.list2 <- c("2014-03-30", as.character(unique(frp.m$Date)))
    for (i in c(1: length(time.list1))) {
        frp.m[frp.m$Date == time.list1[i], "Start_date"] <- time.list2[i]
    }
    
    frp.m$Start_date <- as.Date(frp.m$Start_date)
    frp.m$End_date <- as.Date(frp.m$End_date)
    frp.m$ndays <- as.numeric(frp.m$End_date - frp.m$Start_date) + 1
    
    #- format dataframe to return
    frp.out <- frp.m[,c("Date","Start_date", "End_date", "Ring","fineroot_production_flux", "ndays")]
    
    # Only use data period 2012-2016
    frp.out <- frp.out[frp.out$Date<="2016-12-31",]
    
    return(frp.out)
    
}