make_leaflitter_flux <- function(c_fraction){
  
  litter_raw <- download_leaflitter()  
  
  # glitch fix
  litter_raw$Ring <- as.character(litter_raw$Ring)
  litter_raw$Trap <- as.character(litter_raw$Trap)
  litter_raw$Ring[is.na(litter_raw$Ring)] <- litter_raw$RING[is.na(litter_raw$Ring)]
  litter_raw$TRAP[is.na(litter_raw$Ring)] <- litter_raw$RING[is.na(litter_raw$Ring)]
  
  # Conversion factor from g basket-1 to mg m-2
  conv <- c_fraction * 1000 / frass_basket_area
  
  litter <- mutate(litter_raw, 
                   Date = as.Date(litter_raw$Date, format = "%d/%m/%Y"),
                   Start_date = Date - days.past,
                   End_date = Date,
                   Twig = Twig * conv / days.past,
                   Bark = Bark * conv / days.past,
                   Seed = Seed * conv / days.past,
                   Leaf = Leaf * conv / days.past
                   )
  
  # Averages by Ring
  litter_a <- summaryBy(Twig + Bark + Seed + Leaf ~ Date + Ring, FUN=mean, na.rm=TRUE,
                        data=litter, id = ~Start_date + End_date, keep.names=TRUE)
  
  litter_a <- as.data.frame(rename(litter_a, 
                                   twig_flux = Twig,
                                   bark_flux = Bark,
                                   seed_flux = Seed,
                                   leaf_flux = Leaf))
  
return(litter_a)
}

