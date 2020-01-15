make_leaflitter_decomposition_rate <- function() {
    ### Download leaf litter decomposition rate
    ### This data is based on leaf collected outside the ring
    ### Put into mesh bags of different sizes inside the ring
    ### Bag sizes: 2mm (to exclude macroinvertebrates) or 4mm (to allow macroinvertebrates)
    ### Here I am using 2 mm because rates are statistically the same
    ### and 2mm data has more temporal coverage
    

    ### read in data
    ### InitialMass.g -- Initial dry mass of litter, in grams, when bag constructed 
    ### RemainingMass.g -- Final dry mass of litter, in grams, when bag harvested
    myDF <- read.csv("data/EucFACE_data/FACE_P0030_RA_LITTER_L2_20130517-20150517.csv")
    
    ### include only mesh bag of 2 mm
    myDF <- subset(myDF, Mesh == "2mm")
    
    ### Convert into per day
    myDF$Time_d <- myDF$Time * 30
    
    ### Add initial mass
    aDF <- data.frame(rep(1:6, each=8), rep(1:4, each=2), rep(0, 48), rep(1:2, 24), NA, NA, NA, NA)
    colnames(aDF) <- names(myDF)
    aDF$Mesh <- "2mm"    
    aDF$RemainingMass.g <- aDF$InitialMass.g <- myDF[1:48, "InitialMass.g"]
    aDF$Time_d <- 0
    myDF <- rbind(aDF, myDF)
        
    ### Fit power model (Lt = L0e(-kt) with mass loss
    for (i in 1:6) {
        for (j in unique(myDF$Plot)) {
            for (m in unique(myDF$Rep)) {
                testDF <- subset(myDF, Ring == i & Plot == j & Rep == m)
                mod <- nls(RemainingMass.g~InitialMass.g * exp(-k * Time_d), start = list(k = 0.001),data=testDF)
                
                myDF$k[myDF$Ring == i & myDF$Plot == j & myDF$Rep == m] <- coefficients(mod)[[1]]
            }
        }
    }
    
    
    ### Summary across rings (should test for statistical significance if do it properly)
    outDF <- summaryBy(k~Ring, data=myDF, FUN=mean, na.rm=T, keep.names=T)
    
    
    return(outDF)
}
