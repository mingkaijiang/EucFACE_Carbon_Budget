read_in_cabl <- function() {

    ##################################################
    ##Read in output data and prepare data format
    ##################################################
    ## read cn amb obs
    inDF1 <- read.csv("data/model_output/CABL/D1CABLEUCAMBAVG.csv",
                      skip = 7, header=T)
    
    inDF2 <- read.csv("data/model_output/CABL/D1CABLEUCELEAVG.csv",
                      skip = 7, header=T)
    
    inDF1[inDF1<=-900] <- 0
    inDF2[inDF2<=-900] <- 0
    
    ### prepare a output df
    yr <- unique(inDF1$YEAR)
    yr2 <- yr[1:11]
    
    ### Move coarseroot to fineroot because cable doesn't distinguish the two
    inDF1$GR <- inDF1$GCR
    inDF2$GR <- inDF2$GCR
    
    inDF1$GCR <- 0
    inDF2$GCR <- 0
    
    inDF1$CFR <- inDF1$CCR
    inDF2$CFR <- inDF2$CCR
    
    inDF1$CCR <- 0
    inDF2$CCR <- 0
    
    inDF1$CFRLIN <- inDF1$CCRLIN
    inDF2$CFRLIN <- inDF2$CCRLIN
    
    inDF1$CCRLIN <- 0
    inDF2$CCRLIN <- 0
    
    annDF1 <- summaryBy(NEP+GPP+NPP+RECO+RAUTO+RLEAF+RWOOD+RROOT+RGROW+RHET+RSOIL+GL+GW+GCR+GR+GREPR+CLLFALL+CCRLIN+CFRLIN+CWIN+CVOC~YEAR,
                        data=inDF1, keep.names=T, FUN=sum)
    
    annDF2 <- summaryBy(NEP+GPP+NPP+RECO+RAUTO+RLEAF+RWOOD+RROOT+RGROW+RHET+RSOIL+GL+GW+GCR+GR+GREPR+CLLFALL+CCRLIN+CFRLIN+CWIN+CVOC~YEAR,
                        data=inDF2, keep.names=T, FUN=sum)
    
    ### pools
    for (i in yr) {
        annDF1$CL[annDF1$YEAR == i] <- inDF1$CL[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$CW[annDF1$YEAR == i] <- inDF1$CW[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$CCR[annDF1$YEAR == i] <- inDF1$CCR[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$CFR[annDF1$YEAR == i] <- inDF1$CFR[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$CLIT1[annDF1$YEAR == i] <- inDF1$CLIT1[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$CLIT2[annDF1$YEAR == i] <- inDF1$CLIT2[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$CLIT3[annDF1$YEAR == i] <- inDF1$CLIT3[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$CLIT4[annDF1$YEAR == i] <- inDF1$CLIT4[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$CSOIL[annDF1$YEAR == i] <- inDF1$CSOIL[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$TNC[annDF1$YEAR == i] <- inDF1$TNC[inDF1$YEAR==i&inDF1$DOY==1]
    }
    
    
    for (i in yr) {
        annDF2$CL[annDF2$YEAR == i] <- inDF2$CL[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CW[annDF2$YEAR == i] <- inDF2$CW[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CCR[annDF2$YEAR == i] <- inDF2$CCR[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CFR[annDF2$YEAR == i] <- inDF2$CFR[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CLIT1[annDF2$YEAR == i] <- inDF2$CLIT1[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CLIT2[annDF2$YEAR == i] <- inDF2$CLIT2[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CLIT3[annDF2$YEAR == i] <- inDF2$CLIT3[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CLIT4[annDF2$YEAR == i] <- inDF2$CLIT4[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CSOIL[annDF2$YEAR == i] <- inDF2$CSOIL[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$TNC[annDF2$YEAR == i] <- inDF2$TNC[inDF2$YEAR==i&inDF2$DOY==1]
    }
    
    ### delta pools
    for (i in yr2) {
        annDF1$delta_CL[annDF1$YEAR == i] <- inDF1$CL[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CL[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CW[annDF1$YEAR == i] <- inDF1$CW[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CW[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CCR[annDF1$YEAR == i] <- inDF1$CCR[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CCR[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CFR[annDF1$YEAR == i] <- inDF1$CFR[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CFR[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CLIT1[annDF1$YEAR == i] <- inDF1$CLIT1[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CLIT1[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CLIT2[annDF1$YEAR == i] <- inDF1$CLIT2[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CLIT2[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CLIT3[annDF1$YEAR == i] <- inDF1$CLIT3[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CLIT3[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CLIT4[annDF1$YEAR == i] <- inDF1$CLIT4[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CLIT4[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CSOIL[annDF1$YEAR == i] <- inDF1$CSOIL[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CSOIL[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_TNC[annDF1$YEAR == i] <- inDF1$TNC[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$TNC[inDF1$YEAR==i&inDF1$DOY==1]
    }
    
    for (i in yr2) {
        annDF2$delta_CL[annDF2$YEAR == i] <- inDF2$CL[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CL[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CW[annDF2$YEAR == i] <- inDF2$CW[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CW[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CCR[annDF2$YEAR == i] <- inDF2$CCR[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CCR[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CFR[annDF2$YEAR == i] <- inDF2$CFR[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CFR[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CLIT1[annDF2$YEAR == i] <- inDF2$CLIT1[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CLIT1[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CLIT2[annDF2$YEAR == i] <- inDF2$CLIT2[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CLIT2[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CLIT3[annDF2$YEAR == i] <- inDF2$CLIT3[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CLIT3[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CLIT4[annDF2$YEAR == i] <- inDF2$CLIT4[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CLIT4[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CSOIL[annDF2$YEAR == i] <- inDF2$CSOIL[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CSOIL[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_TNC[annDF2$YEAR == i] <- inDF2$TNC[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$TNC[inDF2$YEAR==i&inDF2$DOY==1]
    }
    
    i <- "2023"
    
    annDF1$delta_CL[annDF1$YEAR == i] <- inDF1$CL[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CL[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CW[annDF1$YEAR == i] <- inDF1$CW[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CW[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CCR[annDF1$YEAR == i] <- inDF1$CCR[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CCR[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CFR[annDF1$YEAR == i] <- inDF1$CFR[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CFR[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CLIT1[annDF1$YEAR == i] <- inDF1$CLIT1[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CLIT1[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CLIT2[annDF1$YEAR == i] <- inDF1$CLIT2[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CLIT2[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CLIT3[annDF1$YEAR == i] <- inDF1$CLIT3[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CLIT3[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CLIT4[annDF1$YEAR == i] <- inDF1$CLIT4[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CLIT4[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CSOIL[annDF1$YEAR == i] <- inDF1$CSOIL[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CSOIL[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_TNC[annDF1$YEAR == i] <- inDF1$TNC[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$TNC[inDF1$YEAR==i&inDF1$DOY==1]
    
    
    annDF2$delta_CL[annDF2$YEAR == i] <- inDF2$CL[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CL[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CW[annDF2$YEAR == i] <- inDF2$CW[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CW[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CCR[annDF2$YEAR == i] <- inDF2$CCR[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CCR[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CFR[annDF2$YEAR == i] <- inDF2$CFR[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CFR[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CLIT1[annDF2$YEAR == i] <- inDF2$CLIT1[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CLIT1[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CLIT2[annDF2$YEAR == i] <- inDF2$CLIT2[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CLIT2[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CLIT3[annDF2$YEAR == i] <- inDF2$CLIT3[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CLIT3[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CLIT4[annDF2$YEAR == i] <- inDF2$CLIT4[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CLIT4[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CSOIL[annDF2$YEAR == i] <- inDF2$CSOIL[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CSOIL[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_TNC[annDF2$YEAR == i] <- inDF2$TNC[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$TNC[inDF2$YEAR==i&inDF2$DOY==1]
    
    ### assign CO2 treatment
    annDF1$CO2 <- "aCO2"
    annDF2$CO2 <- "eCO2"
    
    outDF1 <- rbind(annDF1, annDF2)
    
    names(outDF1)[names(outDF1) == "CLIT1"] <- "CFLIT"
    names(outDF1)[names(outDF1) == "CLIT2"] <- "CFLITA"
    names(outDF1)[names(outDF1) == "CLIT3"] <- "CFLITB"
    names(outDF1)[names(outDF1) == "CLIT4"] <- "CCLITB"
    
    names(outDF1)[names(outDF1) == "delta_CLIT1"] <- "delta_CFLIT"
    names(outDF1)[names(outDF1) == "delta_CLIT2"] <- "delta_CFLITA"
    names(outDF1)[names(outDF1) == "delta_CLIT3"] <- "delta_CFLITB"
    names(outDF1)[names(outDF1) == "delta_CLIT4"] <- "delta_CCLITB"
    
    ### allocation coefficients
    outDF1$ALEAF <- round(outDF1$GL / outDF1$NPP, 3)
    outDF1$AWOOD <- round(outDF1$GW / outDF1$NPP,3)
    outDF1$AFROOT <- round(outDF1$GR / outDF1$NPP,3)
    outDF1$ACROOT <- round(outDF1$GCR / outDF1$NPP,3)
    outDF1$AOTHER <- round(1 - outDF1$ALEAF - outDF1$AWOOD - outDF1$AFROOT - outDF1$ACROOT, 2)
    
    ### turnover rates
    outDF1$tau_LEAF <- round(outDF1$GL/outDF1$CL,3)
    outDF1$tau_WOOD <- round(outDF1$GW/outDF1$CW,3)
    outDF1$tau_FROOT <- round(outDF1$GR/outDF1$CFR,3)
    outDF1$tau_CROOT <- round(outDF1$GCR/outDF1$CCR,3)
    outDF1$tau_LIT <- round(outDF1$CLLFALL/outDF1$CFLIT, 3)
    outDF1$tau_CFLITA <- round(outDF1$CLLFALL/outDF1$CFLITA, 3)
    outDF1$tau_CFLITB <- round(outDF1$CFRLIN/outDF1$CFLITB, 3)
    
    outDF1$tau_SOIL <- round(outDF1$RHET/outDF1$CSOIL, 3)
    
    outDF2 <- summaryBy(.~CO2, keep.names=T, data=outDF1, FUN=mean)
    
    outDF2$YEAR <- NULL
    
    ### calculate the difference (eCO2 - aCO2) and percent difference
    l <- dim(outDF2)[2]
    test1 <- outDF2[outDF2$CO2=="eCO2",2:l] - outDF2[outDF2$CO2=="aCO2",2:l] 
    test2 <- outDF2[outDF2$CO2=="eCO2",2:l] / outDF2[outDF2$CO2=="aCO2",2:l] 
    
    diff <- rbind(test1, test2)
    diff$CO2 <- c("abs", "pct")
    
    outDF3 <- rbind(outDF2, diff)
    
    return(outDF3)
    
}