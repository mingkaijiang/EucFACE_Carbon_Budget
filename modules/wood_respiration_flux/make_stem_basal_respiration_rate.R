make_stem_basal_respiration_rate <- function() {

    ### read files
    myDF1 <- read.table("temp_files/stemCO2efflux_Dec2017.txt",header=T)
    myDF2 <- read.table("temp_files/stemCO2efflux_Jan2018.txt",header=T)
    myDF3 <- read.table("temp_files/stemCO2efflux_Feb2018.txt",header=T)
    
    myDF <- rbind(myDF1, myDF2)
    myDF <- rbind(myDF, myDF3)
    
    ### Read stem temperature files
    tDF <- read.table("temp_files/Xylem.CO2.Temp_20180211.txt",header=T)

    test <- subset(myDF1, Label == 207)
    with(test, plot(flux_corrected~Tcham))
    
    
    ### Q10
    ### Dec 1.52
    ### Jan 1.45
    ### Feb 1.25
    ### ln (EA_S) = a + b T; Q10 = e (10 b) 
    ### a is the intercept and b is the slope between EA_S and Tstem
    
    ### To obtain continuous EA_V data from discrete EA_S measurements, 
    ### EA_S was firstly modelled as a function of temperature as described in Eqns. 1 and 2. 
    ### Secondly, continuous EA_S was expressed on a volume basis according to allometric
    ### properties of the tree obtained from cut sections of trees used for sap flow calculations (Gimeno et al., 2018)
    ### following Rodríguez-Calcerrada et al. (2015):
    ### EA_V =  2 × EA_S  × rt / (rt2- rh2)							
    ### where rt is the tree radius (including heartwood, sapwood and bark), 
    ### and rh is heartwood radius.  
    
    return(out)
    
}