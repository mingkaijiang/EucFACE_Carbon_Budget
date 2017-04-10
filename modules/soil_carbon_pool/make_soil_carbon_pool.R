make_soil_carbon_pool <- function(return="all_depths"){
  
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
  #- need to merge with bulk density data to estimate total soil C. I can't find good bulk density data on HIEv.
  #  The following data were e-mailed to me by Teresa in 2012. The data come from the installation of deep neutron probe tubes.
  #  Obviously this needs to be improved!
  #  These data have units of g cm-3.
  bd_dat <- data.frame(depth_cm =c(0,25,50,75,100,125,150,200,250,300,350,400,450),
                       bulk_density = c(1.471382946,1.570031379,1.7008125,1.773617371,1.820908387,1.690893733,
                    1.824676849,1.763475366,1.743997907,1.624457541,1.701598475,1.583228323,1.658029033))
  
  #- average for the three depths of soil C measurement
  bd_dat_2 <- data.frame(Depth=levels(C_dat$Depth),
                       bulk_density=c(bd_dat$bulk_density[1],mean(bd_dat$bulk_density[1:2]),mean(bd_dat$bulk_density[1:2])))
  
  #- merge soil C with bulk density
  dat <- merge(C_dat,bd_dat_2,by="Depth")
  #------
  
  
  
  
  #------
  #- calculate soil C content of each layer. Units of g C m-2 for each 10-cm long depth increment
  #  Note that the 10-20cm and 20-30cm layers were only measured on 3 of the 15 dates.
  #   These deeper layers have less C than the 0-10cm layer.
  dat$totCgm2 <- with(dat,totCper/100*bulk_density*10*10000) # convert to gC m-2
  
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
  
  
  
  #------
  #- sum across layers on each date, if "return" is "all_depths"
  if(return=="all_depths"){
    dat.s <- summaryBy(totCgm2~Plot+Ring+Date,data=dat,FUN=sum,keep.names=T)
    names(dat.s)[4] <- "soil_carbon_pool"
    
  }
  
  #- return only the shallow layer on each date, if "return" is "shallow"
  if(return=="shallow"){
    dat.s <- summaryBy(totCgm2~Plot+Ring+Date,data=subset(dat,Depth=="0-10cm"),FUN=sum,keep.names=T)
    names(dat.s)[4] <- "soil_carbon_pool"
    
  }
  
  #- average across plots within each ring
  dat.s.m <- summaryBy(soil_carbon_pool~Date+Ring,data=dat.s,FUN=mean,keep.names=T)
  dat.s.m$Ring <- as.numeric(dat.s.m$Ring)
  return(dat.s.m)
  #------
  
  
}
