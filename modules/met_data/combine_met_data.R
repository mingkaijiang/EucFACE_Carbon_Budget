combine_met_data <- function(timestep) {
    
    ### Read input
    if (timestep == "Monthly") {
        DF1 <- read.csv("R_other/met_air_flux_data_monthly.csv")
        DF2 <- read.csv("R_other/rainfall_data_monthly.csv")

    } else if (timestep == "Daily") {
        DF1 <- read.csv("R_other/met_air_flux_data_daily.csv")
        DF2 <- read.csv("R_other/rainfall_data_daily.csv")
        
    } else if (timestep == "Hourly") {
        DF1 <- read.csv("R_other/met_air_flux_data_hourly.csv")
        DF2 <- read.csv("R_other/rainfall_data_hourly.csv")
        
    }
    
    ### Update date column names to be consistent
    names(DF1)[1] <- names(DF2)[1] <- "Date"
    
    myDF <- merge(DF1, DF2, by.x="Date", all=T)
    
    colnames(myDF) <- c("Date", "Tair", "RH", "PAR", "Pressure", "Wind", "Rain")
    
    myDF$Date <- as.Date(myDF$Date)
    
    ### Prepare plots
    ## Tair
    p1 <- ggplot(myDF, aes(Date))+
        geom_line(data=myDF, aes(y=Tair), color="pink")+
        labs(x="Date", y="Air T (C)")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        scale_x_date(date_breaks = "1 year", 
                     date_labels="%b-%Y",
                     limits = as.Date(c('2012-08-01','2018-06-01')))
    
    ## Prec
    p2 <- ggplot(myDF, aes(Date))+
        geom_line(data=myDF, aes(y=Rain), color="cyan")+
        labs(x="Date", y="Rainfall (mm)")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        scale_x_date(date_breaks = "1 year", 
                     date_labels="%b-%Y",
                     limits = as.Date(c('2012-08-01','2018-06-01')))

    ## Wind
    p3 <- ggplot(myDF, aes(Date))+
        geom_line(data=myDF, aes(y=Wind), color="grey")+
        labs(x="Date", y="Wind speed (m/s)")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        scale_x_date(date_breaks = "1 year", 
                     date_labels="%b-%Y",
                     limits = as.Date(c('2012-08-01','2018-06-01')))
    
    ## PAR
    p4 <- ggplot(myDF, aes(Date))+
        geom_line(data=myDF, aes(y=PAR), color="orange")+
        labs(x="Date", y=paste(expression("PAR (umol ", m^-2, " ", s^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        scale_x_date(date_breaks = "1 year", 
                     date_labels="%b-%Y",
                     limits = as.Date(c('2012-08-01','2018-06-01')))
    
    ## RH
    p5 <- ggplot(myDF, aes(Date))+
        geom_line(data=myDF, aes(y=RH), color="blue")+
        labs(x="Date", y="RH (%)")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        scale_x_date(date_breaks = "1 year", 
                     date_labels="%b-%Y",
                     limits = as.Date(c('2012-08-01','2018-06-01')))
    
    ## Pressure
    p6 <- ggplot(myDF, aes(Date))+
        geom_line(data=myDF, aes(y=Pressure), color="green")+
        labs(x="Date", y="Pressure (hPA)")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        scale_x_date(date_breaks = "1 year", 
                     date_labels="%b-%Y",
                     limits = as.Date(c('2012-08-01','2018-06-01')))
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)")
    
    require(grid)
    
    ## plot 
    if (timestep == "Monthly") {
        pdf("output/Monthly_met_time_series.pdf", width=10,height=8)
    } else if (timestep == "Daily") {
        pdf("output/Daily_met_time_series.pdf", width=10,height=8)
    } else if (timestep == "Hourly") {
        pdf("output/Hourly_met_time_series.pdf", width=10,height=8)
    }
    
    grid.newpage()
    grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), 
                    ggplotGrob(p3), ggplotGrob(p4),
                    ggplotGrob(p5), ggplotGrob(p6),size="last"))
    
    grid.text(grid.labs,x = 0.09, y = c(0.96, 0.8, 0.64, 0.48, 0.32, 0.16),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    ### Return the combined file as well
    return(myDF)
}