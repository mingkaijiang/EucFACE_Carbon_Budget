read_in_clmp <- function() {

    ##################################################
    ##Read in output data and prepare data format
    ##################################################
    ## read cn amb obs
    inDF1 <- read.csv("model_output/CLMP/D1CLMPEUCAMBAVG.csv")
    
    inDF2 <- read.csv("model_output/CLMP/D1CLMPEUCELEAVG.csv")
    
    
    inDF1[inDF1<=-999] <- 0
    inDF2[inDF2<=-999] <- 0
    
    ### prepare a output df
    yr <- unique(inDF1$YEAR)
    yr2 <- yr[1:11]
    
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
        annDF1$CFLIT[annDF1$YEAR == i] <- inDF1$CFLIT[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$CFLITA[annDF1$YEAR == i] <- inDF1$CFLITA[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$CFLITB[annDF1$YEAR == i] <- inDF1$CFLITB[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$CCLITB[annDF1$YEAR == i] <- inDF1$CCLITB[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$CSOIL[annDF1$YEAR == i] <- inDF1$CSOIL[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$TNC[annDF1$YEAR == i] <- inDF1$TNC[inDF1$YEAR==i&inDF1$DOY==1]
    }
    
    
    for (i in yr) {
        annDF2$CL[annDF2$YEAR == i] <- inDF2$CL[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CW[annDF2$YEAR == i] <- inDF2$CW[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CCR[annDF2$YEAR == i] <- inDF2$CCR[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CFR[annDF2$YEAR == i] <- inDF2$CFR[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CFLIT[annDF2$YEAR == i] <- inDF2$CFLIT[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CFLITA[annDF2$YEAR == i] <- inDF2$CFLITA[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CFLITB[annDF2$YEAR == i] <- inDF2$CFLITB[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CCLITB[annDF2$YEAR == i] <- inDF2$CCLITB[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$CSOIL[annDF2$YEAR == i] <- inDF2$CSOIL[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$TNC[annDF2$YEAR == i] <- inDF2$TNC[inDF2$YEAR==i&inDF2$DOY==1]
    }
    
    ### delta pools
    for (i in yr2) {
        annDF1$delta_CL[annDF1$YEAR == i] <- inDF1$CL[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CL[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CW[annDF1$YEAR == i] <- inDF1$CW[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CW[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CCR[annDF1$YEAR == i] <- inDF1$CCR[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CCR[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CFR[annDF1$YEAR == i] <- inDF1$CFR[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CFR[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CFLIT[annDF1$YEAR == i] <- inDF1$CFLIT[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CFLIT[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CFLITA[annDF1$YEAR == i] <- inDF1$CFLITA[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CFLITA[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CFLITB[annDF1$YEAR == i] <- inDF1$CFLITB[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CFLITB[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CCLITB[annDF1$YEAR == i] <- inDF1$CCLITB[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CCLITB[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_CSOIL[annDF1$YEAR == i] <- inDF1$CSOIL[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$CSOIL[inDF1$YEAR==i&inDF1$DOY==1]
        annDF1$delta_TNC[annDF1$YEAR == i] <- inDF1$TNC[inDF1$YEAR==(i+1)&inDF1$DOY==1]-inDF1$TNC[inDF1$YEAR==i&inDF1$DOY==1]
    }
    
    for (i in yr2) {
        annDF2$delta_CL[annDF2$YEAR == i] <- inDF2$CL[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CL[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CW[annDF2$YEAR == i] <- inDF2$CW[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CW[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CCR[annDF2$YEAR == i] <- inDF2$CCR[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CCR[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CFR[annDF2$YEAR == i] <- inDF2$CFR[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CFR[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CFLIT[annDF2$YEAR == i] <- inDF2$CFLIT[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CFLIT[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CFLITA[annDF2$YEAR == i] <- inDF2$CFLITA[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CFLITA[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CFLITB[annDF2$YEAR == i] <- inDF2$CFLITB[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CFLITB[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CCLITB[annDF2$YEAR == i] <- inDF2$CCLITB[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CCLITB[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_CSOIL[annDF2$YEAR == i] <- inDF2$CSOIL[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$CSOIL[inDF2$YEAR==i&inDF2$DOY==1]
        annDF2$delta_TNC[annDF2$YEAR == i] <- inDF2$TNC[inDF2$YEAR==(i+1)&inDF2$DOY==1]-inDF2$TNC[inDF2$YEAR==i&inDF2$DOY==1]
    }
    
    i <- "2023"
    
    annDF1$delta_CL[annDF1$YEAR == i] <- inDF1$CL[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CL[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CW[annDF1$YEAR == i] <- inDF1$CW[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CW[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CCR[annDF1$YEAR == i] <- inDF1$CCR[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CCR[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CFR[annDF1$YEAR == i] <- inDF1$CFR[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CFR[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CFLIT[annDF1$YEAR == i] <- inDF1$CFLIT[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CFLIT[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CFLITA[annDF1$YEAR == i] <- inDF1$CFLITA[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CFLITA[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CFLITB[annDF1$YEAR == i] <- inDF1$CFLITB[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CFLITB[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CCLITB[annDF1$YEAR == i] <- inDF1$CCLITB[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CCLITB[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_CSOIL[annDF1$YEAR == i] <- inDF1$CSOIL[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$CSOIL[inDF1$YEAR==i&inDF1$DOY==1]
    annDF1$delta_TNC[annDF1$YEAR == i] <- inDF1$TNC[inDF1$YEAR==i&inDF1$DOY==365]-inDF1$TNC[inDF1$YEAR==i&inDF1$DOY==1]
    
    
    annDF2$delta_CL[annDF2$YEAR == i] <- inDF2$CL[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CL[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CW[annDF2$YEAR == i] <- inDF2$CW[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CW[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CCR[annDF2$YEAR == i] <- inDF2$CCR[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CCR[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CFR[annDF2$YEAR == i] <- inDF2$CFR[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CFR[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CFLIT[annDF2$YEAR == i] <- inDF2$CFLIT[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CFLIT[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CFLITA[annDF2$YEAR == i] <- inDF2$CFLITA[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CFLITA[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CFLITB[annDF2$YEAR == i] <- inDF2$CFLITB[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CFLITB[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CCLITB[annDF2$YEAR == i] <- inDF2$CCLITB[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CCLITB[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_CSOIL[annDF2$YEAR == i] <- inDF2$CSOIL[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$CSOIL[inDF2$YEAR==i&inDF2$DOY==1]
    annDF2$delta_TNC[annDF2$YEAR == i] <- inDF2$TNC[inDF2$YEAR==i&inDF2$DOY==365]-inDF2$TNC[inDF2$YEAR==i&inDF2$DOY==1]
    
    ### assign CO2 treatment
    annDF1$CO2 <- "aCO2"
    annDF2$CO2 <- "eCO2"
    
    outDF1 <- rbind(annDF1, annDF2)
    
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