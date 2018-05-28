leaf_npp_and_lerp_production_plot <- function(leaf_npp, lerp_production,
                                              frass_production, lai,
                                              insect_consumption) {
    
    ### Look at all results
    with(insect_consumption, plot(Start_date, herbivory_leaf_consumption_flux, ylim=c(0, 600),
                                  ylab = "flux (mg C m-2 d-1)"))
    with(leaf_npp, points(leaf_flux/6~Start_date, col=adjustcolor("green", alpha.f=0.9)))
    with(lerp_production, points(lerp_production_flux~Start_date, col="red", pch=19))
    with(lai, points(lai_variable*240~Date, col=adjustcolor("blue", alpha.f=0.1)))
    with(frass_production, points(frass_production_flux*2~Start_date, 
                                  col=adjustcolor("orange", alpha.f=0.5)))
    legend("topleft", c("insect", "npp/6", "lerp", "lai*240", "frass*2"),
           col=c("black", "green", "red", "blue", "orange"), pch = c(21, 21, 19, 21, 21))
    
    
    ### look at insect dynamics
    with(lerp_production, plot(lerp_production_flux~Start_date, col="red", pch=19,
                               ylab = "C flux (mg C m-2 d-1)"))
    with(frass_production, points(frass_production_flux~Start_date, 
                                  col=adjustcolor("orange", alpha.f=0.5)))
    with(insect_consumption, points(herbivory_leaf_consumption_flux/5~Start_date,
                                    col="green"))
    legend("topleft", c("lerp production", "frass production", "insect consumption/5"),
           col=c("red", "orange", "green"), pch = c(19, 21, 21))
    
    
    ### Add doy and year information
    lerp_production$DOY <- yday(lerp_production$Start_date)
    lerp_production$year <- year(lerp_production$Start_date)
    
    leaf_npp$DOY <- yday(leaf_npp$Start_date)
    leaf_npp$year <- year(leaf_npp$Start_date)
    
    
    ### plot leaf NPP and lerp production for each year
    for (i in 1:6) {
        test <- subset(leaf_npp, Ring == i)
        with(test[test$year == 2012, ], plot(leaf_flux~DOY, 
                                             col=adjustcolor("orange", alpha.f=0.8), type="b", 
                                             xlim=c(0, 366), ylim=c(0, 1500), xlab = "DOY",
                                             ylab = "leaf NPP (mg C m-2 d-1)"))
        with(test[test$year == 2013, ], points(leaf_flux~DOY, col=adjustcolor("lightgreen", alpha.f=0.8), type="b"))
        with(test[test$year == 2015, ], points(leaf_flux~DOY, col=adjustcolor("pink", alpha.f=0.8), type="b"))
        with(test[test$year == 2014, ], points(leaf_flux~DOY, col="red", type="b"))
        with(test[test$year == 2016, ], points(leaf_flux~DOY, col=adjustcolor("lightblue", alpha.f=0.8), type="b"))
        
        # psyllid attack year
        abline(v=61)
        with(lerp_production[lerp_production$Ring == i & lerp_production$year == 2014,], 
             points(lerp_production_flux~DOY,col="black", pch=19))
        
        # normal year lerp production
        with(lerp_production[lerp_production$Ring == i & lerp_production$year != 2014,], 
             points(lerp_production_flux~DOY,col="grey", pch=19))
        
        legend("topleft", legend=c("Y2012", "Y2013", "Y2014", "Y2015", "Y2016", "Psyllid attack",
                                    "lerp 2014", "lerp normal"), col = c("orange", "lightgreen", "pink",
                                                                "red", "lightblue", "black", "black"),
               lwd = c(1,1,1,1,1,1,NA,NA), pch = c(NA, NA, NA, NA, NA, NA, 19, 19), ncol=2, cex = 0.7)
    }

}
