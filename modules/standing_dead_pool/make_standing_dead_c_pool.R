
# Make the standing dead wood C pool
make_standing_dead_c_pool <- function(ring_area, c_frac) {
    
    ### download the data from HIEv
    download_mortality_data()
    
    ### read in mortality information
    morDF <- read.csv(file.path(getToPath(), "dendro_mortality_updated.txt"))
    morDF$date_last_observed_alive <- as.Date(morDF$date_last_observed_alive,format="%d/%m/%Y")
    
    ### Read in additional files
    classif <- read.csv("download/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv",stringsAsFactors = FALSE)

    ### Merge the files
    all <- merge(classif,morDF,by=c("Tree"))
    
    ### add biomass to long-form dataframe
    all$biom <- allom_agb(all$last_observed_diameter)  # in kg DM
    
    ### sum across rings and dates
    data.tot <- summaryBy(biom~date_last_observed_alive+Ring,data=all,FUN=sum,keep.names=T,na.rm=T)
    
    ## divide by ring area to get biomass per m2
    data.tot$wood_pool <- data.tot$biom / ring_area
    
    ### convert from kg DM m-2 to g C m-2
    data.tot$wood_pool <- data.tot$wood_pool * c_frac * 1000
    colnames(data.tot)<- c("Date", "Ring", "biom","wood_pool")
    
    ### Calculate cumulative mass per ring per date
    outDF <- data.frame(rep(c(1:6), length(unique(data.tot$Date))),
                        NA, NA)
    colnames(outDF) <- c("Ring", "Date", "wood_pool")
    outDF$Date <- rep(unique(data.tot$Date), each=6)
    outDF$wood_pool <- 0
    
    ### tmp DF
    tmpDF <- outDF
    date.list <- unique(data.tot$Date)
    for (i in unique(data.tot$Ring[data.tot$Date==date.list[1]])) {
        tmpDF$wood_pool[tmpDF$Ring==i&tmpDF$Date==date.list[1]] <- data.tot$wood_pool[data.tot$Ring==i&data.tot$Date==date.list[1]]
    }
    for (i in unique(data.tot$Ring[data.tot$Date==date.list[2]])) {
        tmpDF$wood_pool[tmpDF$Ring==i&tmpDF$Date==date.list[2]] <- data.tot$wood_pool[data.tot$Ring==i&data.tot$Date==date.list[2]]
    }
    for (i in unique(data.tot$Ring[data.tot$Date==date.list[3]])) {
        tmpDF$wood_pool[tmpDF$Ring==i&tmpDF$Date==date.list[3]] <- data.tot$wood_pool[data.tot$Ring==i&data.tot$Date==date.list[3]]
    }
    for (i in unique(data.tot$Ring[data.tot$Date==date.list[4]])) {
        tmpDF$wood_pool[tmpDF$Ring==i&tmpDF$Date==date.list[4]] <- data.tot$wood_pool[data.tot$Ring==i&data.tot$Date==date.list[4]]
    }
    for (i in unique(data.tot$Ring[data.tot$Date==date.list[5]])) {
        tmpDF$wood_pool[tmpDF$Ring==i&tmpDF$Date==date.list[5]] <- data.tot$wood_pool[data.tot$Ring==i&data.tot$Date==date.list[5]]
    }
    for (i in unique(data.tot$Ring[data.tot$Date==date.list[6]])) {
        tmpDF$wood_pool[tmpDF$Ring==i&tmpDF$Date==date.list[6]] <- data.tot$wood_pool[data.tot$Ring==i&data.tot$Date==date.list[6]]
    }
    for (i in unique(data.tot$Ring[data.tot$Date==date.list[7]])) {
        tmpDF$wood_pool[tmpDF$Ring==i&tmpDF$Date==date.list[7]] <- data.tot$wood_pool[data.tot$Ring==i&data.tot$Date==date.list[7]]
    }
    
    
    ### assign cumulative result
    for (i in 1:6) {
        outDF[outDF$Ring==i&outDF$Date==date.list[1], "wood_pool"] <- tmpDF$wood_pool[tmpDF$Date==date.list[1]&tmpDF$Ring==i]
    }

    for (i in 1:6) {
        for (j in 2:7) {
            outDF[outDF$Ring==i&outDF$Date==date.list[j], "wood_pool"] <- tmpDF$wood_pool[tmpDF$Date==date.list[j]&tmpDF$Ring==i]+outDF[outDF$Ring==i&outDF$Date==date.list[j-1], "wood_pool"]
        }
    }
    
    outDF <- outDF[outDF$wood_pool>0, ]

    return(outDF)
}
