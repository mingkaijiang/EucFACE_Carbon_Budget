combine_met_data <- function() {
    
    ### Read input
    DF1 <- read.csv("R_other/met_air_flux_data_daily.csv")
    DF2 <- read.csv("R_other/rainfall_data_daily.csv")
    
    ### Convert PAR from umol m-2 s-1 to W m-2 
    DF1$PAR_W <- DF1$LI190SB_PAR_Den_Avg * 4.57
    DF1$PAR_MJ_d <- DF1$PAR_W * 24 * 3600 / 1000000
    
    ### Update date column names to be consistent
    names(DF1)[1] <- names(DF2)[1] <- "Date"
    myDF <- merge(DF1, DF2, by.x="Date", all=T)
    colnames(myDF) <- c("Date", "Tair", "RH", "PAR", "Pressure", "Wind", "PAR_W", "PAR_MJ_d", "Rain")
    myDF$Date <- as.Date(myDF$Date)
    
    ### Add month information
    myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
    myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
    
    ### Convert PAR from umol m-2 s-1 to MJ m-2 d-1
    myDF$PAR_W <- myDF$PAR * 4.57
    myDF$PAR_MJ <- myDF$PAR_W * 3.6 / 1000
    
    ### compute monthly mean and sd
    plotDF1 <- summaryBy(Tair+RH+PAR_MJ+Pressure+Wind~Month, data=myDF, FUN=mean, keep.names=T, na.rm=T)
    plotDF2 <- summaryBy(Rain~Month, data=myDF, FUN=sum, keep.names=T, na.rm=T)
    plotDF3 <- summaryBy(Tair+RH+PAR_MJ+Pressure+Wind~Month, data=myDF, FUN=sd, keep.names=T, na.rm=T)
    
    plotDF <- cbind(plotDF1, plotDF2$Rain, plotDF3$Tair, plotDF3$RH, plotDF3$PAR_MJ, plotDF3$Pressure, plotDF3$Wind)
    colnames(plotDF) <- c("Date", "Tair", "RH", "PAR", "Pressure", "Wind", "Rain",
                          "Tair_sd", "RH_sd", "PAR_sd", "Pressure_sd", "Wind_sd")
    
    ### Prepare plots
    ## Tair
    p1 <- ggplot(plotDF, aes(Date, Tair))+
        geom_ribbon(aes(ymin=Tair-Tair_sd,ymax=Tair+Tair_sd), fill="pink", alpha=0.6)+
        geom_line(color="red")+
        labs(x="Date", y=expression("Air T ("* degree* "C)"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        scale_x_date(date_breaks = "1 year", 
                     date_labels="%b-%Y",
                     limits = as.Date(c('2012-09-01','2017-08-31')))
    
    ## Prec
    p2 <- ggplot(plotDF, aes(Date, Rain))+
        geom_line(color="cyan")+
        geom_area(aes(y = Rain), fill="cyan")+
        labs(x="Date", y="Rainfall (mm)")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        scale_x_date(date_breaks = "1 year", 
                     date_labels="%b-%Y",
                     limits = as.Date(c('2012-09-01','2017-08-31')))

    ## Wind
#    p3 <- ggplot(plotDF, aes(Date, Wind))+
#        geom_ribbon(aes(ymin=Wind-Wind_sd,ymax=Wind+Wind_sd), fill="violet",alpha=0.4)+
#        geom_line(color="purple")+
#        labs(x="Date", y=expression(paste("WS (m ", s^-1, ")")))+
#        theme_linedraw() +
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_blank(), 
#              axis.text.x = element_blank(),
#              axis.text.y=element_text(size=10),
#              axis.title.y=element_text(size=10),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_line(color="grey"),
#              legend.position="none")+
#        scale_x_date(date_breaks = "1 year", 
#                     date_labels="%b-%Y",
#                     limits = as.Date(c('2012-09-01','2017-08-31')))
    
    ## PAR
    p4 <- ggplot(plotDF, aes(Date, PAR))+
        geom_ribbon(aes(ymin=PAR-PAR_sd,ymax=PAR+PAR_sd), fill="yellow", alpha=0.3)+
        geom_line(color="orange")+
        labs(x="Date", y=expression(paste("PAR (MJ ", m^-2, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        scale_x_date(date_breaks = "1 year", 
                     date_labels="%b-%Y",
                     limits = as.Date(c('2012-09-01','2017-08-31')))
    
    ## RH
#    p5 <- ggplot(plotDF, aes(Date, RH))+
#        geom_ribbon(aes(ymin=RH-RH_sd,ymax=RH+RH_sd), fill="grey", alpha=0.4)+
#        geom_line(color="darkblue")+
#        labs(x="Date", y="RH (%)")+
#        theme_linedraw() +
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_blank(), 
#              axis.text.x = element_blank(),
#              axis.text.y=element_text(size=10),
#              axis.title.y=element_text(size=10),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_line(color="grey"),
#              legend.position="none")+
#        scale_x_date(date_breaks = "1 year", 
#                     date_labels="%b-%Y",
#                     limits = as.Date(c('2012-09-01','2017-08-31')))
    
    ## Pressure
#    p6 <- ggplot(plotDF, aes(Date, Pressure))+
#        geom_ribbon(aes(ymin=Pressure-Pressure_sd,ymax=Pressure+Pressure_sd), fill="green", alpha=0.4)+
#        geom_line(color="darkgreen")+
#        labs(x="Date", y="Pressure (HPA)")+
#        theme_linedraw() +
#        theme(panel.grid.minor=element_blank(),
#              axis.title.x = element_text(size=12), 
#              axis.text.x = element_text(size=10),
#              axis.text.y=element_text(size=10),
#              axis.title.y=element_text(size=10),
#              legend.text=element_text(size=12),
#              legend.title=element_text(size=14),
#              panel.grid.major=element_line(color="grey"),
#              legend.position="none")+
#        scale_x_date(date_breaks = "1 year", 
#                     date_labels="%b-%Y",
#                     limits = as.Date(c('2012-09-01','2017-08-31')))
    
    grid.labs <- c("(a)", "(b)", "(c)")
    
    require(grid)
    
    ## plot 
    pdf("output/Figure_S1.pdf", width=10,height=8)
 
    
    grid.newpage()
    grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), 
                    ggplotGrob(p4), size="last"))
    
    grid.text(grid.labs,x = 0.09, y = c(0.96, 0.64, 0.32),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    ### Return the combined file as well
    return(myDF)
}