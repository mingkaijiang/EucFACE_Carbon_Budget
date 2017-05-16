
# Make the table of results 
# Very much first try, May 2017

source("run.R")

conv <- 365 / 1000  # convert production flux from mg m-2 d-1 to g m-2 yr-1

# In / out fluxes
term <- c("GPP overstorey", "GPP understorey", "CH4 uptake",
          "Ra leaf", "Ra stem", "Ra understorey", "VOC",
          "Rherbivore", "DOC loss", "Rsoil", "Rgrowth")
inout <- data.frame(term)
inout$value <- rep(NA, length(inout$term))

maespa <- read.csv("data/2013_maespa_gpp_respiration.csv")
inout$value[inout$term == "GPP overstorey"] <- mean(maespa$GPP.mg.m2.d) * conv
inout$value[inout$term == "Ra leaf"] <- mean(maespa$Respiration.mg.m2.d) * conv
inout$value[inout$term == "Rsoil"] <- mean(soil_respiration_flux$soil_respiration_flux) * conv

# NPP fluxes
term <- c("Leaf NPP", "Stem NPP", "Understorey NPP",
          "Fine Root NPP", "Frass production", "R hetero", "Mycorrhizal production",
          "Flower production")

npp <- data.frame(term)
npp$value <- rep(NA, length(npp$term))

leaflitter_flux$twig_flux[leaflitter_flux$twig_flux > 5000] <- 0   # surely these values are in error, correct
leaflitter_flux$total <- with(leaflitter_flux, twig_flux + bark_flux + seed_flux + leaf_flux)
leaflitter_flux$days <- as.numeric(with(leaflitter_flux,End_date - Start_date))
litter_prod <- with(leaflitter_flux,sum(total*days)/sum(days)) * conv 
npp$value[npp$term == "Leaf NPP"] <- litter_prod
stem_prod <- mean(wood_production_flux$wood_production_flux) * conv
npp$value[npp$term == "Stem NPP"] <- stem_prod
froot_prod <- mean(fineroot_production_flux$fineroot_production_flux) * conv
npp$value[npp$term == "Fine Root NPP"] <- froot_prod
npp$value[npp$term == "Frass production"] <- mean(frass_production_flux$frass_production_flux) * conv

ccost <- 0.3 # growth respiration cost - no idea of this value really
inout$value[inout$term == "Rgrowth"] <- ccost * (litter_prod + stem_prod + froot_prod)

# Standing C pools
term <- c("Overstorey leaf", "Overstorey wood", "Understorey above-ground",
          "Fine Root", "Coarse Root", "Litter", "Coarse woody debris", 
          "Microbial biomass", "Soil C", "Mycorrhizae", "Insects")

pool <- data.frame(term)
pool$value <- rep(NA, length(pool$term))

pool$value[pool$term == "Overstorey leaf"] <- mean(leaf_pool$leaf_pool)
pool$value[pool$term == "Overstorey wood"] <- mean(wood_pool$wood_pool)
pool$value[pool$term == "Fine Root"] <- mean(fineroot_pool$fineroot_pool)
pool$value[pool$term == "Soil C"] <- mean(soil_carbon_pool$soil_carbon_pool)

# output tables
inout
npp
pool
