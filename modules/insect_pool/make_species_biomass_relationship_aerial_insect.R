make_species_biomass_relationship_aerial_insects <- function(){
    
    download_insect_data()  
    
    ### pitfall to collect ground-dwelling arthopods
    myDF1 <- read.csv("download/FACE_P0051_RA_ARTHROPODS-2_L1_20131101-20150114.csv")
    
    ## suction sampling to collect understorey arthropods
    myDF2 <- read.csv("download/FACE_P0051_RA_ARTHROPODS-3_L1_20131101-20150114.csv")
    
    ## aerial samples
    myDF3 <- read.csv("download/FACE_P0051_RA_ARTHROPODS-5_L1_20130930-20141121.csv.csv")

    myDF1 <- myDF1[,c("RUN", "RING", "PLOT", "GROUP", "ABUNDANCE", "WEIGHT.MG.")]
    myDF2 <- myDF2[,c("Run", "Ring", "Plot", "Group", "Abundance", "Weight.mg.")]
    colnames(myDF1) <- colnames(myDF2) <- c("Run", "Ring", "Plot", "Group", "Abundance", "Weight.mg.")
    
    ## add method
    myDF1$Method <- "pitfall"
    myDF2$Method <- "suction"
    
    myDF <- rbind(myDF1, myDF2)
    
    ### calculate individual mass
    myDF$weight_individual <- myDF$Weight.mg. / myDF$Abundance
    
    # average across groups
    myDF.mass <- summaryBy(weight_individual~Group, FUN=mean, data=myDF, keep.names=T, na.rm=T)

    # add individual mass information onto aerial dataset
    myDF.merge <- merge(myDF3, myDF.mass, by.x = c("GROUP"), by.y = c("Group"), all.x=T)
    
    # fil NA values with all means
    m.value <- mean(myDF.mass$weight_individual, na.rm=T)
    myDF.merge$weight_individual <- ifelse(is.na(myDF.merge$weight_individual), m.value, myDF.merge$weight_individual)
    
    # convert into total mass per collectin, and convert into g from mg
    myDF.merge$weight <- myDF.merge$weight_individual * myDF.merge$ABUNDANCE / 1000
    
    # sum all insect at each height and direction within a ring together
    myDF.sum <- summaryBy(weight~RUN+RING, FUN=sum, data=myDF.merge, na.rm=T, keep.names=T)
    
    myDF.sum$Date <- paste0("01-", as.character(myDF.sum$RUN))
    myDF.sum$Date <- gsub("-", "/", myDF.sum$Date)
    myDF.sum$Date <- as.Date(myDF.sum$Date, format="%d/%b/%y")
    myDF.sum$Method <- "aerial"
    
    out <- myDF.sum[,c("Date", "RING", "weight")]
    colnames(out) <- c("Date", "Ring", "weight")
    
    
    ### return
    return(out)
}

