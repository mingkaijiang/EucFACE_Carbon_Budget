cue_calculation <- function(inDF) {
    
    ### subseting DF
    temDF1 <- inDF$npp[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6", "aCO2", "eCO2")]
    temDF2 <- inDF$inout[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6", "aCO2", "eCO2")]
    temDF3 <- inDF$pool[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6", "aCO2", "eCO2")]
    temDF4 <- inDF$delta_pool[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6", "aCO2", "eCO2")]
    
    
    ### create data frames to store ring data for aCO2 and eCO2
    ## aCO2
    errDF1 <- data.frame(c("NPP", "GPP", "CUE"), NA, NA, NA)
    colnames(errDF1) <- c("cat", "R2", "R3", "R6")
    ## eCO2
    errDF2 <- data.frame(c("NPP", "GPP", "CUE"), NA, NA, NA)
    colnames(errDF2) <- c("cat", "R1", "R4", "R5")

    ### calculate sum of NPP for each ring
    ## aCO2
    errDF1[errDF1$cat=="NPP", "R2"] <- sum(temDF1[temDF1$term == "Leaf NPP","Ring_2"],
                                                temDF1[temDF1$term == "Stem NPP","Ring_2"],
                                                temDF1[temDF1$term == "Fine Root NPP","Ring_2"],
                                                temDF1[temDF1$term == "Coarse Root NPP","Ring_2"],
                                           temDF1[temDF1$term == "Bole Root NPP","Ring_2"],
                                                temDF1[temDF1$term == "Other NPP","Ring_2"],
                                                temDF1[temDF1$term == "Understorey NPP","Ring_2"],
                                                temDF1[temDF1$term == "Leaf consumption","Ring_2"])
    
    
    errDF1[errDF1$cat=="NPP", "R3"] <- sum(temDF1[temDF1$term == "Leaf NPP","Ring_3"],
                                                temDF1[temDF1$term == "Stem NPP","Ring_3"],
                                                temDF1[temDF1$term == "Fine Root NPP","Ring_3"],
                                                temDF1[temDF1$term == "Coarse Root NPP","Ring_3"],
                                           temDF1[temDF1$term == "Bole Root NPP","Ring_3"],
                                                temDF1[temDF1$term == "Other NPP","Ring_3"],
                                                temDF1[temDF1$term == "Understorey NPP","Ring_3"],
                                                temDF1[temDF1$term == "Leaf consumption","Ring_3"])
    
    
    errDF1[errDF1$cat=="NPP", "R6"] <- sum(temDF1[temDF1$term == "Leaf NPP","Ring_6"],
                                                temDF1[temDF1$term == "Stem NPP","Ring_6"],
                                                temDF1[temDF1$term == "Fine Root NPP","Ring_6"],
                                                temDF1[temDF1$term == "Coarse Root NPP","Ring_6"],
                                           temDF1[temDF1$term == "Bole Root NPP","Ring_6"],
                                                temDF1[temDF1$term == "Other NPP","Ring_6"],
                                                temDF1[temDF1$term == "Understorey NPP","Ring_6"],
                                                temDF1[temDF1$term == "Leaf consumption","Ring_6"])
    
    
    ## eCO2
    errDF2[errDF2$cat=="NPP", "R1"] <- sum(temDF1[temDF1$term == "Leaf NPP","Ring_1"],
                                                temDF1[temDF1$term == "Stem NPP","Ring_1"],
                                                temDF1[temDF1$term == "Fine Root NPP","Ring_1"],
                                                temDF1[temDF1$term == "Coarse Root NPP","Ring_1"],
                                           temDF1[temDF1$term == "Bole Root NPP","Ring_1"],
                                                temDF1[temDF1$term == "Other NPP","Ring_1"],
                                                temDF1[temDF1$term == "Understorey NPP","Ring_1"],
                                                temDF1[temDF1$term == "Leaf consumption","Ring_1"])
    
    
    errDF2[errDF2$cat=="NPP", "R4"] <- sum(temDF1[temDF1$term == "Leaf NPP","Ring_4"],
                                                temDF1[temDF1$term == "Stem NPP","Ring_4"],
                                                temDF1[temDF1$term == "Fine Root NPP","Ring_4"],
                                                temDF1[temDF1$term == "Coarse Root NPP","Ring_4"],
                                           temDF1[temDF1$term == "Bole Root NPP","Ring_4"],
                                                temDF1[temDF1$term == "Other NPP","Ring_4"],
                                                temDF1[temDF1$term == "Understorey NPP","Ring_4"],
                                                temDF1[temDF1$term == "Leaf consumption","Ring_4"])
    
    
    errDF2[errDF2$cat=="NPP", "R5"] <- sum(temDF1[temDF1$term == "Leaf NPP","Ring_5"],
                                                temDF1[temDF1$term == "Stem NPP","Ring_5"],
                                                temDF1[temDF1$term == "Fine Root NPP","Ring_5"],
                                                temDF1[temDF1$term == "Coarse Root NPP","Ring_5"],
                                           temDF1[temDF1$term == "Bole Root NPP","Ring_5"],
                                                temDF1[temDF1$term == "Other NPP","Ring_5"],
                                                temDF1[temDF1$term == "Understorey NPP","Ring_5"],
                                                temDF1[temDF1$term == "Leaf consumption","Ring_5"])
    
    ### calculate sum of GPP for each ring
    ## aCO2
    errDF1[errDF1$cat=="GPP", "R2"] <- sum(temDF2[temDF2$term == "GPP overstorey","Ring_2"],
                                                temDF2[temDF2$term == "GPP understorey","Ring_2"])
    
    errDF1[errDF1$cat=="GPP", "R3"] <- sum(temDF2[temDF2$term == "GPP overstorey","Ring_3"],
                                                temDF2[temDF2$term == "GPP understorey","Ring_3"])
    
    errDF1[errDF1$cat=="GPP", "R6"] <- sum(temDF2[temDF2$term == "GPP overstorey","Ring_6"],
                                                temDF2[temDF2$term == "GPP understorey","Ring_6"])
    
    ## eCO2
    errDF2[errDF2$cat=="GPP", "R1"] <- sum(temDF2[temDF2$term == "GPP overstorey","Ring_1"],
                                                temDF2[temDF2$term == "GPP understorey","Ring_1"])
    
    errDF2[errDF2$cat=="GPP", "R4"] <- sum(temDF2[temDF2$term == "GPP overstorey","Ring_4"],
                                                temDF2[temDF2$term == "GPP understorey","Ring_4"])
    
    errDF2[errDF2$cat=="GPP", "R5"] <- sum(temDF2[temDF2$term == "GPP overstorey","Ring_5"],
                                                temDF2[temDF2$term == "GPP understorey","Ring_5"])
    
    errDF1[errDF1$cat=="CUE", "R2"] <- errDF1[errDF1$cat=="NPP", "R2"]/errDF1[errDF1$cat=="GPP", "R2"]
    errDF1[errDF1$cat=="CUE", "R3"] <- errDF1[errDF1$cat=="NPP", "R3"]/errDF1[errDF1$cat=="GPP", "R3"]
    errDF1[errDF1$cat=="CUE", "R6"] <- errDF1[errDF1$cat=="NPP", "R6"]/errDF1[errDF1$cat=="GPP", "R6"]
    
    
    errDF2[errDF2$cat=="CUE", "R1"] <- errDF2[errDF2$cat=="NPP", "R1"]/errDF2[errDF2$cat=="GPP", "R1"]
    errDF2[errDF2$cat=="CUE", "R4"] <- errDF2[errDF2$cat=="NPP", "R4"]/errDF2[errDF2$cat=="GPP", "R4"]
    errDF2[errDF2$cat=="CUE", "R5"] <- errDF2[errDF2$cat=="NPP", "R5"]/errDF2[errDF2$cat=="GPP", "R5"]
    
    ## calculate means and sd, convert unit from g C to kg C
    errDF1$aCO2 <- rowMeans(subset(errDF1, select=c(R2, R3, R6)), na.rm=T)
    errDF1$aCO2_sd <- rowSds(as.matrix(subset(errDF1, select=c(R2, R3, R6)), na.rm=T))
    
    errDF2$eCO2 <- rowMeans(subset(errDF2, select=c(R1, R4, R5)), na.rm=T)
    errDF2$eCO2_sd <- rowSds(as.matrix(subset(errDF2, select=c(R1, R4, R5)), na.rm=T))
    
    return(list(aDF = data.table(errDF1), 
                eDF = data.table(errDF2)))
    
}
