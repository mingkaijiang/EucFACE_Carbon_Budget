make_leaflitter_flux <- function(c_frac,
                                 return.decision="data",
                                 trt.effect="abs",
                                 stat.model="interaction",
                                 var.col=7){
    
    litter_raw <- download_leaflitter()  
    
    # glitch fix
    litter_raw$Ring <- as.character(litter_raw$Ring)
    litter_raw$Trap <- as.character(litter_raw$Trap)
    litter_raw$Ring[is.na(litter_raw$Ring)] <- litter_raw$RING[is.na(litter_raw$Ring)]
    litter_raw$TRAP[is.na(litter_raw$Ring)] <- litter_raw$RING[is.na(litter_raw$Ring)]
    
    # remove two data points where big branches fall into litter bascket
    line.num <- which.max(litter_raw$Twig)
    litter_raw <- litter_raw[-line.num,]
    
    line.num <- which.max(litter_raw$Twig)
    litter_raw <- litter_raw[-line.num,]
    
    # Conversion factor from g basket-1 to mg m-2
    conv <- c_frac * 1000 / frass_basket_area
    
    litter <- dplyr::mutate(litter_raw, 
                            Date = as.Date(litter_raw$Date, format = "%d/%m/%Y"),
                            Start_date = Date - days.past,
                            End_date = Date,
                            ndays = days.past,
                            Twig = as.numeric(Twig) * conv / days.past,
                            Bark = as.numeric(Bark) * conv / days.past,
                            Seed = as.numeric(Seed) * conv / days.past,
                            Leaf = as.numeric(Leaf) * conv / days.past)
    
    # Averages by Ring
    litter_a <- summaryBy(Twig + Bark + Seed + Leaf ~ Date + Ring, FUN=mean, na.rm=TRUE,
                          data=litter, id = ~Start_date + End_date, keep.names=TRUE)
    
    litter_a <- as.data.frame(dplyr::rename(litter_a, 
                                            twig_flux = Twig,
                                            bark_flux = Bark,
                                            seed_flux = Seed,
                                            leaf_flux = Leaf))
    
    litter_a$ndays <- as.numeric(litter_a$End_date - litter_a$Start_date) + 1
    
    # Only use data period 2012-2016
    litter_a <- litter_a[litter_a$Date<="2016-12-31",]
    
    ### Compute statistical analyses
    if (return.decision=="stats") {
        if (trt.effect == "abs") {
            if (stat.model == "dynamic") {
                source("R/stats/treatment_effect_abs_statistics_dynamic.R")
            } else if (stat.model == "no_interaction") {
                source("R/stats/treatment_effect_abs_statistics_no_interaction.R")
            } else if (stat.model == "interaction") {
                source("R/stats/treatment_effect_abs_statistics_interaction.R")
            } else {
                source("R/stats/treatment_effect_abs_statistics_no_random_effect.R")
            }
            
            s.stats <- treatment_effect_abs_statistics(inDF=litter, 
                                                       var.cond="flux", var.col=var.col,
                                                       date.as.factor=T)
        } else if (trt.effect == "ratio") {
            if (stat.model == "dynamic") {
                source("R/stats/treatment_effect_ratio_statistics_dynamic.R")
            } else if (stat.model == "no_interaction") {
                source("R/stats/treatment_effect_ratio_statistics_no_interaction.R")
            } else if (stat.model == "interaction") {
                source("R/stats/treatment_effect_ratio_statistics_interaction.R")
            } else {
                source("R/stats/treatment_effect_ratio_statistics_no_random_effect.R")
            }
            
            s.stats <- treatment_effect_ratio_statistics(inDF=litter, 
                                                         var.cond="flux", var.col=var.col,
                                                         date.as.factor=T)
        }
        
    }

    ### Decision on what to return
    if (return.decision == "data") {
        return(litter_a)
    } else if (return.decision == "stats") {
        return(s.stats)
    }
        
    return(litter_a)
}

