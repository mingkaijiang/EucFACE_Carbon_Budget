make_overstorey_gpp_flux <- function() {
    ### read in MAESPA GPP output
    inDF <- read.csv("data/overstorey_gpp_annual fluxes.csv")
    
    colnames(inDF) <- c("year", "Ring", "GPP", "Rfoliage", "Trans", "Soil")
    
    ### swap ring characters
    inDF$Ring <- gsub("R1","1", inDF$Ring)
    inDF$Ring <- gsub("R2","2", inDF$Ring)
    inDF$Ring <- gsub("R3","3", inDF$Ring)
    inDF$Ring <- gsub("R4","4", inDF$Ring)
    inDF$Ring <- gsub("R5","5", inDF$Ring)
    inDF$Ring <- gsub("R6","6", inDF$Ring)
    
    outDF <- summaryBy(GPP~year+Ring, data=inDF, FUN=mean, keep.names=T, na.rm=T)

    # Only use data period 2012-2016
    outDF <- outDF[outDF$year<="2016",]
    
    outDF$Date <- as.Date(paste0(outDF$year, "-01-01"), format = "%Y-%m-%d")
    
    ## Assign treatment
    outDF$Trt[outDF$Ring%in%c(2,3,6)] <- "aCO2"
    outDF$Trt[outDF$Ring%in%c(1,4,5)] <- "eCO2"
    
   #test <- summaryBy(GPP~year+Trt, data=outDF, FUN=mean, keep.names=T, na.rm=T)
   #
   #
   #ggplot(outDF, aes(x=Ring, y=GPP, fill=Trt))+
   #    geom_bar(stat="identity", position="stack")+facet_grid(~year)
   #
   #ggplot(outDF, aes(x=as.character(year),y=GPP,color=Trt))+
    #   geom_bar(stat="identity", position="stack")+facet_grid(~year)
   #
    
    
    return(outDF)
}