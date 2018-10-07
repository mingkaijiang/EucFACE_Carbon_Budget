make_soil_carbon_pool <- function(bk_density, return){
    
    ### bk_density: in the unit of kg m-3, 3 depths
    ### return: return soil c for which depth
    ###         options are shallow and all_depths
    
    #- download the data
    download_soil_carbon_data()
    
    #------
    #- read in the soil C content data
    files <- list.files(path=getToPath(),pattern="BasicSoilProperties",full.names=T)
    dat1 <- list()
    for (i in 1:length(files)){
        dat1[[i]] <- read.csv(files[i])
        dat1[[i]] <- dat1[[i]][,1:10] # exclude crap columns
        names(dat1[[i]]) <- c("Date","SampleNumber","Ring","Plot","Depth","ph","gravSoilMoistper","totCper","totNper","totPppm") # force equivalent names
    }
    C_dat <- do.call(rbind,dat1) # note that totCper has units of percent (%)
    C_dat$Date <- as.Date(C_dat$Date,format="%d/%m/%Y")
    
    #- get rid of spaces in the variable "Depth"
    C_dat$Depth <- as.character(C_dat$Depth)
    C_dat$Depth <- factor(gsub(" ", "", C_dat$Depth, fixed = TRUE)) # 3 levels of Depth (0-10cm, 10-20cm, 20-30cm)
    
    
    #- note that all data in 2015 are missing. Remove them.
    C_dat <- subset(C_dat,Date<as.Date("2015-01-01"))
    #------
    
    
    
    
    #------
    #- need to merge with bulk density data to estimate total soil C. 
    #- merge soil C with bulk density
    dat <- merge(C_dat,bk_density,by.x=c("Depth", "Ring"), by.y=c("Depth", "ring"))
    #------
    
    
    
    #------
    #- calculate soil C content of each layer. Units of g C m-2 for each 10-cm long depth increment
    #  Note that the 10-20cm and 20-30cm layers were only measured on 3 of the 15 dates.
    #   These deeper layers have less C than the 0-10cm layer.
    dat$totCgm2 <- with(dat,totCper/100*bulk_density_kg_m3*0.1*1000) # convert to gC m-2
    
    #- get averages for the deeper depths
    dat.m.deep <- summaryBy(totCgm2~Ring+Plot+Depth,data=dat,FUN=mean,keep.names=T,na.rm=T)
    
    #- set up an empty dataframe with all potential levels of Date, Plot, Ring, and Depth in "dat"
    dat.empty <- expand.grid(Depth=levels(dat$Depth),Plot=levels(as.factor(dat$Plot)),
                             Ring=levels(as.factor(dat$Ring)),Date=levels(as.factor(dat$Date)))
    dat <- merge(dat,dat.empty,by=c("Depth","Plot","Ring","Date"),all.y=T)
    
    #- loop over the data, if deeper data are missing, gapfill with the average for that plot
    naflag <- NA
    for (i in 1:nrow(dat)){
        naflag <- is.na(dat[i,"totCgm2"]) # is the datum missing?
        if(naflag){
            Depth_id <- dat[i,"Depth"]
            Ring_id <- dat[i,"Ring"]
            Plot_id <- dat[i,"Plot"]
            
            id <- which(dat.m.deep$Depth==Depth_id & dat.m.deep$Ring==Ring_id & dat.m.deep$Plot==Plot_id)
            dat[i,"totCgm2"] <- dat.m.deep[id,"totCgm2"]
        }
    }
    #------
    
    #- Calculate CN and CP ratios where possible
    dat$CN <- dat$totCper / dat$totNper
    dat$CP <- dat$totCper / (dat$totPppm / 10000)
    
    
    #------
    #- sum across layers on each date, if "return" is "all_depths"
    if(return=="all_depths"){
        dat.s <- summaryBy(totCgm2~Plot+Ring+Date,data=dat,FUN=sum,keep.names=T)
        names(dat.s)[4] <- "soil_carbon_pool"
        #- average across plots within each ring
        dat.s.m <- summaryBy(soil_carbon_pool~Date+Ring,data=dat.s,FUN=mean,keep.names=T)
        dat.s.m$Ring <- as.numeric(dat.s.m$Ring)
        
    }
    
    #- return only the shallow layer on each date, if "return" is "shallow"
    if(return=="shallow"){
        dat.s <- summaryBy(totCgm2~Plot+Ring+Date,data=subset(dat,Depth=="0-10cm"),FUN=sum,keep.names=T)
        names(dat.s)[4] <- "soil_carbon_pool"
        #- average across plots within each ring
        dat.s.m <- summaryBy(soil_carbon_pool~Date+Ring,data=dat.s,FUN=mean,keep.names=T)
        dat.s.m$Ring <- as.numeric(dat.s.m$Ring)
        
    }
    
    #- return by depth, ring, if "return" is "by_depths"
    if(return=="by_depths"){
        dat.s <- summaryBy(totCgm2~Plot+Ring+Date+Depth,data=dat,FUN=sum,keep.names=T)
        names(dat.s)[5] <- "soil_carbon_pool"
        
        dat.ph <- summaryBy(ph~Date+Ring+Depth, data=dat, FUN=mean, keep.names=T, na.rm=T)
        dat.cn <- summaryBy(CN~Date+Ring+Depth, data=dat, FUN=mean, keep.names=T, na.rm=T)
        dat.cp <- summaryBy(CP~Date+Ring+Depth, data=dat, FUN=mean, keep.names=T, na.rm=T)
        
        
        dat.s.m <- summaryBy(soil_carbon_pool~Date+Ring+Depth,data=dat.s,FUN=mean,keep.names=T)
        dat.s.m$Ring <- as.numeric(dat.s.m$Ring)
        
        dat.s.m <- dat.s.m[,c("Date", "Ring", "soil_carbon_pool", "Depth")]
        
        for (i in 1:6) {
            for (j in unique(dat.s.m$Date)) {
                for (k in unique(dat.s.m$Depth)) {
                    dat.s.m$ph[dat.s.m$Ring==i&dat.s.m$Date==j&dat.s.m$Depth==k] <- dat.ph$ph[dat.ph$Ring==i&dat.ph$Date==j&dat.ph$Depth==k]
                    dat.s.m$cn[dat.s.m$Ring==i&dat.s.m$Date==j&dat.s.m$Depth==k] <- dat.cn$CN[dat.cn$Ring==i&dat.cn$Date==j&dat.cn$Depth==k]
                    dat.s.m$cp[dat.s.m$Ring==i&dat.s.m$Date==j&dat.s.m$Depth==k] <- dat.cp$CP[dat.cp$Ring==i&dat.cp$Date==j&dat.cp$Depth==k]
                    
                }
            }
        }
        
    }
    
    # Only use data period 2012-2016    
    #dat.s.m <- dat.s.m[dat.s.m$Date>"2012-06-17",]
    dat.s.m <- dat.s.m[dat.s.m$Date<="2016-12-31",]
    return(dat.s.m)
    #------
    
    
}
