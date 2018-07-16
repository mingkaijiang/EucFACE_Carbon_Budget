water_logging_2015 <- function(fr.pool, rsoil) {
    #### Read in files
    metDF <- read.csv("R_other/VOC_met_data.csv")
    metDF$Date <- as.Date(metDF$DateHour)
    mDF <- summaryBy(.~Date, FUN=mean, data=metDF, na.rm=T, keep.names=T)
    rDF <- summaryBy(Rain~Date, FUN=sum, data=metDF, na.rm=T, keep.names=T)
    mDF$Rain <- rDF$Rain
    
    ### check SM with fineroot biomass
    pdf("R_other/SM_Fineroot_water_logging.pdf", width=6, height=8)
    par(mfrow=c(3,2), mar=c(5, 4, 4, 6) + 0.1)
    
    ## Ring 1
    fDF <- subset(fr.pool, Ring==1)
    
    # Set up tick labs
    ytick1 <-seq(0, 120, by=20)
    ytick1.lab <-seq(0, 30, by=5)
    ytick2 <- seq(60, 120, by=10)
    
    # Plot
    with(mDF, plot(SM_R1*300~Date, type="l", ylim=c(0,120), 
                   ylab="Soil Moisture at 30cm (%)", yaxt="n", main="R1"))
    axis(side=2, at=ytick1, labels = ytick1.lab)

    with(fDF, points(fineroot_pool~Date, pch=19, col="red"))
    axis(side=4, at=ytick2, labels = ytick2)
    mtext("Fineroot C Pool",side=4,line=2.5)
    
    ## Ring 2
    fDF <- subset(fr.pool, Ring==2)

    # Plot
    with(mDF, plot(SM_R2*300~Date, type="l", ylim=c(0,120), 
                   ylab="Soil Moisture at 30cm (%)", yaxt="n", main="R2"))
    axis(side=2, at=ytick1, labels = ytick1.lab)
    
    with(fDF, points(fineroot_pool~Date, pch=19, col="red"))
    axis(side=4, at=ytick2, labels = ytick2)
    mtext("Fineroot C Pool",side=4,line=2.5)
    
    
    ## Ring 3
    fDF <- subset(fr.pool, Ring==3)
    
    # Plot
    with(mDF, plot(SM_R3*300~Date, type="l", ylim=c(0,120), 
                   ylab="Soil Moisture at 30cm (%)", yaxt="n", main="R3"))
    axis(side=2, at=ytick1, labels = ytick1.lab)
    
    with(fDF, points(fineroot_pool~Date, pch=19, col="red"))
    axis(side=4, at=ytick2, labels = ytick2)
    mtext("Fineroot C Pool",side=4,line=2.5)
    
    
    ## Ring 4
    fDF <- subset(fr.pool, Ring==4)

    # Plot
    with(mDF, plot(SM_R4*300~Date, type="l", ylim=c(0,120), 
                   ylab="Soil Moisture at 30cm (%)", yaxt="n", main="R4"))
    axis(side=2, at=ytick1, labels = ytick1.lab)
    
    with(fDF, points(fineroot_pool~Date, pch=19, col="red"))
    axis(side=4, at=ytick2, labels = ytick2)
    mtext("Fineroot C Pool",side=4,line=2.5)
    
    
    ## Ring 5
    fDF <- subset(fr.pool, Ring==5)
    
    # Plot
    with(mDF, plot(SM_R5*300~Date, type="l", ylim=c(0,120), 
                   ylab="Soil Moisture at 30cm (%)", yaxt="n", main="R5"))
    axis(side=2, at=ytick1, labels = ytick1.lab)
    
    with(fDF, points(fineroot_pool~Date, pch=19, col="red"))
    axis(side=4, at=ytick2, labels = ytick2)
    mtext("Fineroot C Pool",side=4,line=2.5)
    
    
    ## Ring 6
    fDF <- subset(fr.pool, Ring==6)
    
    # Plot
    with(mDF, plot(SM_R6*300~Date, type="l", ylim=c(0,120), 
                   ylab="Soil Moisture at 30cm (%)", yaxt="n", main="R6"))
    axis(side=2, at=ytick1, labels = ytick1.lab)
    
    with(fDF, points(fineroot_pool~Date, pch=19, col="red"))
    axis(side=4, at=ytick2, labels = ytick2)
    mtext("Fineroot C Pool",side=4,line=2.5)
    
    dev.off()
    
    ### Check SM and Rsoil
    pdf("R_other/SM_Rsoil_water_logging.pdf", width=6, height=8)
    par(mfrow=c(3,2), mar=c(5, 4, 4, 6) + 0.1)
    
    ## Ring 1
    sDF <- subset(rsoil, Ring==1)
    
    # Set up tick labs
    ytick1 <-seq(0, 120, by=20)
    ytick1.lab <-seq(0, 30, by=5)
    ytick2 <- seq(0, 150, by=25)
    ytick2.lab <- seq(0, 24, by=4)
    
    # Plot
    with(mDF, plot(SM_R1*300~Date, type="l", ylim=c(0,150), 
                   ylab="Soil Moisture at 30cm (%)", yaxt="n", main="R1"))
    axis(side=2, at=ytick1, labels = ytick1.lab)
    
    with(sDF, points(soil_respiration_flux/100~Date, pch=19, col="red", cex=0.5))
    axis(side=4, at=ytick2, labels = ytick2.lab)
    mtext("Rsoil (mg m-2 d-1)",side=4,line=2.5)
    
    ## Ring 2
    sDF <- subset(rsoil, Ring==2)
    
    # Plot
    with(mDF, plot(SM_R2*300~Date, type="l", ylim=c(0,150), 
                   ylab="Soil Moisture at 30cm (%)", yaxt="n", main="R1"))
    axis(side=2, at=ytick1, labels = ytick1.lab)
    
    with(sDF, points(soil_respiration_flux/100~Date, pch=19, col="red", cex=0.5))
    axis(side=4, at=ytick2, labels = ytick2.lab)
    mtext("Rsoil (mg m-2 d-1)",side=4,line=2.5)
    
    ## Ring 3
    sDF <- subset(rsoil, Ring==3)
    
    # Plot
    with(mDF, plot(SM_R3*300~Date, type="l", ylim=c(0,150), 
                   ylab="Soil Moisture at 30cm (%)", yaxt="n", main="R1"))
    axis(side=2, at=ytick1, labels = ytick1.lab)
    
    with(sDF, points(soil_respiration_flux/100~Date, pch=19, col="red", cex=0.5))
    axis(side=4, at=ytick2, labels = ytick2.lab)
    mtext("Rsoil (mg m-2 d-1)",side=4,line=2.5)
    
    ## Ring 4
    sDF <- subset(rsoil, Ring==4)
    
    # Plot
    with(mDF, plot(SM_R4*300~Date, type="l", ylim=c(0,150), 
                   ylab="Soil Moisture at 30cm (%)", yaxt="n", main="R1"))
    axis(side=2, at=ytick1, labels = ytick1.lab)
    
    with(sDF, points(soil_respiration_flux/100~Date, pch=19, col="red", cex=0.5))
    axis(side=4, at=ytick2, labels = ytick2.lab)
    mtext("Rsoil (mg m-2 d-1)",side=4,line=2.5)
    
    ## Ring 5
    sDF <- subset(rsoil, Ring==5)
    
    # Plot
    with(mDF, plot(SM_R5*300~Date, type="l", ylim=c(0,150), 
                   ylab="Soil Moisture at 30cm (%)", yaxt="n", main="R1"))
    axis(side=2, at=ytick1, labels = ytick1.lab)
    
    with(sDF, points(soil_respiration_flux/100~Date, pch=19, col="red", cex=0.5))
    axis(side=4, at=ytick2, labels = ytick2.lab)
    mtext("Rsoil (mg m-2 d-1)",side=4,line=2.5)
    
    ## Ring 6
    sDF <- subset(rsoil, Ring==6)
    
    # Plot
    with(mDF, plot(SM_R6*300~Date, type="l", ylim=c(0,150), 
                   ylab="Soil Moisture at 30cm (%)", yaxt="n", main="R1"))
    axis(side=2, at=ytick1, labels = ytick1.lab)
    
    with(sDF, points(soil_respiration_flux/100~Date, pch=19, col="red", cex=0.5))
    axis(side=4, at=ytick2, labels = ytick2.lab)
    mtext("Rsoil (mg m-2 d-1)",side=4,line=2.5)
    
    dev.off()
    
    
    ### Check SM and Rsoil, 1:1 plot
    pdf("R_other/SM_Rsoil_1_1_plot.pdf", width=6, height=8)
    par(mfrow=c(3,2), mar=c(5, 4, 4, 6) + 0.1)
    
    ## Ring 1
    sDF <- subset(rsoil, Ring==1)
    oneDF <- merge(mDF,sDF, by=c("Date"), keep.names=T, all=T)
    
    with(oneDF, plot(soil_respiration_flux~SM_R1, ylab="Rsoil (mg m-2 d-1)", xlab="Soil Moisture",
                     xlim=c(0, 0.4), ylim=c(0, 10000)))
    mod <- lm(soil_respiration_flux~SM_R1, data=oneDF)
    abline(coefficients(mod)[[1]], coefficients(mod)[[2]], col="red", lty=2)
    
    ## Ring 2
    sDF <- subset(rsoil, Ring==2)
    oneDF <- merge(mDF,sDF, by=c("Date"), keep.names=T, all=T)
    
    with(oneDF, plot(soil_respiration_flux~SM_R2, ylab="Rsoil (mg m-2 d-1)", xlab="Soil Moisture",
                     xlim=c(0, 0.4), ylim=c(0, 10000)))
    mod <- lm(soil_respiration_flux~SM_R2, data=oneDF)
    abline(coefficients(mod)[[1]], coefficients(mod)[[2]], col="red", lty=2)
    
    ## Ring 3
    sDF <- subset(rsoil, Ring==3)
    oneDF <- merge(mDF,sDF, by=c("Date"), keep.names=T, all=T)
    
    with(oneDF, plot(soil_respiration_flux~SM_R3, ylab="Rsoil (mg m-2 d-1)", xlab="Soil Moisture",
                     xlim=c(0, 0.4), ylim=c(0, 10000)))
    mod <- lm(soil_respiration_flux~SM_R3, data=oneDF)
    abline(coefficients(mod)[[1]], coefficients(mod)[[2]], col="red", lty=2)
    
    ## Ring 4
    sDF <- subset(rsoil, Ring==4)
    oneDF <- merge(mDF,sDF, by=c("Date"), keep.names=T, all=T)
    
    with(oneDF, plot(soil_respiration_flux~SM_R4, ylab="Rsoil (mg m-2 d-1)", xlab="Soil Moisture",
                     xlim=c(0, 0.4), ylim=c(0, 10000)))
    mod <- lm(soil_respiration_flux~SM_R4, data=oneDF)
    abline(coefficients(mod)[[1]], coefficients(mod)[[2]], col="red", lty=2)
    
    ## Ring 5
    sDF <- subset(rsoil, Ring==5)
    oneDF <- merge(mDF,sDF, by=c("Date"), keep.names=T, all=T)
    
    with(oneDF, plot(soil_respiration_flux~SM_R5, ylab="Rsoil (mg m-2 d-1)", xlab="Soil Moisture",
                     xlim=c(0, 0.4), ylim=c(0, 10000)))
    mod <- lm(soil_respiration_flux~SM_R5, data=oneDF)
    abline(coefficients(mod)[[1]], coefficients(mod)[[2]], col="red", lty=2)
    
    ## Ring 6
    sDF <- subset(rsoil, Ring==6)
    oneDF <- merge(mDF,sDF, by=c("Date"), keep.names=T, all=T)
    
    with(oneDF, plot(soil_respiration_flux~SM_R6, ylab="Rsoil (mg m-2 d-1)", xlab="Soil Moisture",
                     xlim=c(0, 0.4), ylim=c(0, 10000)))
    mod <- lm(soil_respiration_flux~SM_R6, data=oneDF)
    abline(coefficients(mod)[[1]], coefficients(mod)[[2]], col="red", lty=2)
    
    dev.off()
    
}