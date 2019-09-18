
make_sla_variable <- function() {
    
    ### Generate ring-specific SLA data per date
    download_lma_data()
    
    inDF1 <- read.csv(file.path(getToPath(), "FACE_P0020_RA_LMA_L2_20130213-20131115.csv"), stringsAsFactors=F)
    inDF2 <- read.csv(file.path(getToPath(), "FACE_P0020_RA_LMA_20140130-20141016_L2.csv"), stringsAsFactors=F)
    inDF3 <- read.csv(file.path(getToPath(), "FACE_P0020_RA_LMA_20150129-20150416_L2.csv"), stringsAsFactors=F)
    inDF4 <- read.csv(file.path(getToPath(), "FACE_P0020_RA_LMA_20160201-20161018_L2.csv"), stringsAsFactors=F)
    
    lma_raw <- rbind(inDF1, inDF2, inDF3, inDF4)
    lma_raw$Date <- as.Date(lma_raw$Date, format="%d/%m/%Y")
    lma <- droplevels(subset(lma_raw, TREE != "outs R6"))  # outside ring trees
    lma <- mutate(lma, 
                  Ring = as.numeric(substr(TREE,1,1)),
                  LMA = as.numeric(LMA),
                  SLA = 10000 / LMA)  # cm2 g-1
    lma <- subset(lma, !is.na(Ring), select =c(Date,Ring, SLA))  # missing ring is for tree 'outs R6' - ignore
    
    lma_a <- summaryBy(SLA ~ Ring + Date, FUN=mean, na.rm=TRUE, data=lma, keep.names=TRUE)
    
    lma.out <- dplyr::rename(lma_a, sla_variable = SLA)
    
    # Only use data period 2012-2016
    lma.out <- lma.out[lma.out$Date<="2016-12-31",]
    
    
    ### Decision on what to return
    return(lma.out)
    
    
}
