make_insect_pool2 <- function(c_frac){
    
    download_insect_data()  
    
    myDF1 <- read.csv("download/FACE_P0051_RA_ARTHROPODS-2_L1_20131101-20150114.csv")
    myDF2 <- read.csv("download/FACE_P0051_RA_ARTHROPODS-3_L1_20131101-20150114.csv")

    myDF1 <- myDF1[,c("RUN", "RING", "PLOT", "GROUP", "ABUNDANCE", "WEIGHT.MG.")]
    myDF2 <- myDF2[,c("Run", "Ring", "Plot", "Group", "Abundance", "Weight.mg.")]
    colnames(myDF1) <- colnames(myDF2) <- c("Run", "Ring", "Plot", "Group", "Abundance", "Weight.mg.")
    myDF <- rbind(myDF1, myDF2)
    
    # correct date information
    myDF$Date <- paste0("01-", as.character(myDF$Run))
    myDF$Date <- gsub("-", "/", myDF$Date)
    myDF$Date <- as.Date(myDF$Date, format="%d/%b/%y")
    
    # sum across plot
    myDF.sum <- summaryBy(Weight.mg.~Date+Ring+Plot, FUN=sum, data=myDF, keep.names=T, na.rm=T)

    # need a plot area to convert to per ground area
    pitfall_area <- 0.1
    myDF.sum$weight.mg.C <- myDF.sum$Weight.mg. * c_frac
    myDF.sum$weight.mg.C.m2 <- myDF.sum$weight.mg.C / pitfall_area
    
    # average across plot to get per ring mass, g C m-2
    myDF.avg <- summaryBy(weight.mg.C.m2~Date+Ring, FUN=mean, data=myDF.sum, keep.names=T, na.rm=T)
    myDF.avg$weight.g.C.m2 <- myDF.avg$weight.mg.C.m2 / 1000
    
    ### out
    out <- myDF.avg[,c("Date", "Ring", "weight.g.C.m2")]
    colnames(out) <- c("Date", "Ring", "insect_pool")
    
    ### return
    return(out)
}

