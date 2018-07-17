make_leaflitter_pool <- function(c_frac){
    
    ### download data
    litter_raw <- download_leaflitter()  
    
    ### glitch fix
    litter_raw$Ring <- as.character(litter_raw$Ring)
    litter_raw$Trap <- as.character(litter_raw$Trap)
    litter_raw$Ring[is.na(litter_raw$Ring)] <- litter_raw$RING[is.na(litter_raw$Ring)]
    litter_raw$TRAP[is.na(litter_raw$Ring)] <- litter_raw$RING[is.na(litter_raw$Ring)]
    
    ### remove two data points where big branches fall into litter bascket
    line.num <- which.max(litter_raw$Twig)
    litter_raw <- litter_raw[-line.num,]
    
    line.num <- which.max(litter_raw$Twig)
    litter_raw <- litter_raw[-line.num,]
    
    ### Conversion factor from g basket-1 to mg m-2
    conv <- c_frac * 1000 / frass_basket_area
    
    litter <- dplyr::mutate(litter_raw, 
                            Date = as.Date(litter_raw$Date, format = "%d/%m/%Y"),
                            Start_date = Date - days.past,
                            End_date = Date,
                            ndays = days.past,
                            Twig = as.numeric(Twig) * conv,
                            Bark = as.numeric(Bark) * conv,
                            Seed = as.numeric(Seed) * conv,
                            Leaf = as.numeric(Leaf) * conv)
    
    ### Averages by Ring
    litter_a <- summaryBy(Twig + Bark + Seed + Leaf ~ Date + Ring, FUN=mean, na.rm=TRUE,
                          data=litter, id = ~Start_date + End_date, keep.names=TRUE)
    
    litter_a <- as.data.frame(dplyr::rename(litter_a, 
                                            twig_flux = Twig,
                                            bark_flux = Bark,
                                            seed_flux = Seed,
                                            leaf_flux = Leaf))
    
    litter_a$ndays <- as.numeric(litter_a$End_date - litter_a$Start_date) + 1
    
    ### Only use data period 2012-2016
    litter_a <- litter_a[litter_a$Date<="2016-12-31",]
    
    ### Litter decomposition rate
    decomp <- make_leaflitter_decomposition_rate()
    
    ### get date time series
    d.list <- unique(litter_a$Date)
    
    ### Calculate mass remained, non-cumulative
    for (i in 1:length(d.list)-1) {
        for (j in 1:6) {
            ndays <- as.numeric(d.list[i+1] - d.list[i])
            p.r <- exp(decomp$coef[decomp$Ring==j] * ndays + decomp$int[decomp$Ring==j])/100

            litter_a$mass_remain[litter_a$Date==d.list[i] & litter_a$Ring==j] <- litter_a$leaf_flux[litter_a$Date==d.list[i] & litter_a$Ring==j] * p.r
        }
    }
    
    
    
    ### Cumulate mass remained
    for (j in 1:6) {
        litter_a$cum_mass_remain[litter_a$Date==d.list[1] & litter_a$Ring==j] <- litter_a$leaf_flux[litter_a$Date==d.list[1] & litter_a$Ring==j]
    }
    
    for (i in 2:length(d.list)) {
        for (j in 1:6) {
            ndays <- as.numeric(d.list[i] - d.list[i-1])
            p.r <- exp(decomp$coef[decomp$Ring==j] * ndays + decomp$int[decomp$Ring==j])/100
            add.mass <- litter_a$cum_mass_remain[litter_a$Date==d.list[i-1] & litter_a$Ring==j] * p.r
            
            litter_a$cum_mass_remain[litter_a$Date==d.list[i] & litter_a$Ring==j] <- add.mass + litter_a$leaf_flux[litter_a$Date==d.list[i] & litter_a$Ring==j]
        }
    }
    
    ### prepare outDF
    out <- litter_a[,c("Date", "Ring", "Start_date", "End_date", "ndays", "cum_mass_remain")]
    colnames(out) <- c("Date", "Ring", "Start_date", "End_date", "ndays", "leaflitter_pool")
    out$leaflitter_pool <- out$leaflitter_pool/1000
    out <- out[out$Date>="2013-01-01",]

    return(out)
}

