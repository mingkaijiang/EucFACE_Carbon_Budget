compare_voc_fluxes <- function() {
    
    downloadHIEv(hiev=searchHIEv("FACE_P0044_RA_ISOPRENE_2013-15_RAW_V1.csv"))
    
    # read in the data 
    myDF <- read.csv("download/FACE_P0044_RA_ISOPRENE_2013-15_RAW_V1.csv")
    
    # summary
    smDF <- summaryBy(Isoprene_emiss+a.thujene+a.pinene+camphene+b.pinene+X3.carene+limonene+b.phellandrene+terpinolene+eucalyptole~Area,
                      FUN=c(mean,sd), data=myDF, keep.names=T, na.rm=T)
    
    # write csv
    write.csv(smDF, "output/voc_flux_comparison.csv", row.names=F)
    
}