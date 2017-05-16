download_lma_data <- function(){
  downloadCSV("FACE_P0020_RA_LMA_20150129-20150416_L2.csv")
}

make_sla_variable <- function(){
  
  
  lma_raw <- download_lma_data()
  lma_raw$Date <- as.Date(lma_raw$Date, format="%d/%m/%Y")
  lma <- droplevels(subset(lma_raw, TREE != "outs R6"))  # outside ring trees
  lma <- mutate(lma, 
                Ring = as.numeric(substr(TREE,1,1)),
                LMA = as.numeric(LMA),
                SLA = 10000 / LMA)  # cm2 g-1
  lma <- subset(lma, !is.na(Ring), select =c(Date,Ring, SLA))  # missing ring is for tree 'outs R6' - ignore
  
  lma_a <- summaryBy(SLA ~ Ring + Date, FUN=mean, na.rm=TRUE, data=lma, keep.names=TRUE)
  
  lma <- dplyr::rename(lma, sla_variable = SLA)
  
return(lma)
}
