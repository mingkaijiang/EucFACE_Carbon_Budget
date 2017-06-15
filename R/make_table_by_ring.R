
# Make the table of results 
# Very much first try, May 2017

source("run.R")

conv <- 365 / 1000  # convert production flux from mg m-2 d-1 to g m-2 yr-1

# In / out fluxes (Method 1 of getting NEP)
term <- c("GPP overstorey", "GPP understorey", "CH4 uptake",
          "Ra leaf", "Ra stem", "Ra understorey", "VOC",
          "Rherbivore", "DOC loss", "Rsoil", "Rgrowth")
inout <- data.frame(term)
inout$R1 <- rep(NA, length(inout$term))
inout$R2 <- rep(NA, length(inout$term))
inout$R3 <- rep(NA, length(inout$term))
inout$R4 <- rep(NA, length(inout$term))
inout$R5 <- rep(NA, length(inout$term))
inout$R6 <- rep(NA, length(inout$term))
inout$aCO2 <- rep(NA, length(inout$term))
inout$eCO2 <- rep(NA, length(inout$term))
inout$notes <- rep("", length(inout$term))

maespa <- read.csv("data/2013_maespa_gpp_respiration.csv")
inout$R1[inout$term == "GPP overstorey"] <- maespa[maespa$Ring==1, "GPP.mg.m2.d"] * conv
inout$R2[inout$term == "GPP overstorey"] <- maespa[maespa$Ring==2, "GPP.mg.m2.d"] * conv
inout$R3[inout$term == "GPP overstorey"] <- maespa[maespa$Ring==3, "GPP.mg.m2.d"] * conv
inout$R4[inout$term == "GPP overstorey"] <- maespa[maespa$Ring==4, "GPP.mg.m2.d"] * conv
inout$R5[inout$term == "GPP overstorey"] <- maespa[maespa$Ring==5, "GPP.mg.m2.d"] * conv
inout$R6[inout$term == "GPP overstorey"] <- maespa[maespa$Ring==6, "GPP.mg.m2.d"] * conv
inout$notes[inout$term == "GPP overstorey"] <- "MAESPA output - still working on parameterisation"

inout$R1[inout$term == "Ra leaf"] <- maespa[maespa$Ring==1, "Respiration.mg.m2.d"] * conv
inout$R2[inout$term == "Ra leaf"] <- maespa[maespa$Ring==2, "Respiration.mg.m2.d"] * conv
inout$R3[inout$term == "Ra leaf"] <- maespa[maespa$Ring==3, "Respiration.mg.m2.d"] * conv
inout$R4[inout$term == "Ra leaf"] <- maespa[maespa$Ring==4, "Respiration.mg.m2.d"] * conv
inout$R5[inout$term == "Ra leaf"] <- maespa[maespa$Ring==5, "Respiration.mg.m2.d"] * conv
inout$R6[inout$term == "Ra leaf"] <- maespa[maespa$Ring==6, "Respiration.mg.m2.d"] * conv
inout$notes[inout$term == "Ra leaf"] <- "MAESPA output - still working on parameterisation"

inout$R1[inout$term == "Rsoil"] <- mean(soil_respiration_flux[soil_respiration_flux$Ring == 1, "soil_respiration_flux"]) * conv
inout$R2[inout$term == "Rsoil"] <- mean(soil_respiration_flux[soil_respiration_flux$Ring == 2, "soil_respiration_flux"]) * conv
inout$R3[inout$term == "Rsoil"] <- mean(soil_respiration_flux[soil_respiration_flux$Ring == 3, "soil_respiration_flux"]) * conv
inout$R4[inout$term == "Rsoil"] <- mean(soil_respiration_flux[soil_respiration_flux$Ring == 4, "soil_respiration_flux"]) * conv
inout$R5[inout$term == "Rsoil"] <- mean(soil_respiration_flux[soil_respiration_flux$Ring == 5, "soil_respiration_flux"]) * conv
inout$R6[inout$term == "Rsoil"] <- mean(soil_respiration_flux[soil_respiration_flux$Ring == 6, "soil_respiration_flux"]) * conv
inout$notes[inout$term == "Rsoil"] <- "Three years soil respiration data"

# NPP fluxes (Method 3 of getting NEP)
term <- c("Leaf NPP", "Stem NPP", "Understorey NPP",
          "Fine Root NPP", "Frass production", "R hetero", "Mycorrhizal production",
          "Flower production")

npp <- data.frame(term)
npp$R1 <- rep(NA, length(npp$term))
npp$R2 <- rep(NA, length(npp$term))
npp$R3 <- rep(NA, length(npp$term))
npp$R4 <- rep(NA, length(npp$term))
npp$R5 <- rep(NA, length(npp$term))
npp$R6 <- rep(NA, length(npp$term))
npp$aCO2 <- rep(NA, length(npp$term))
npp$eCO2 <- rep(NA, length(npp$term))

npp$notes <- rep("", length(npp$term))

# leaflitter_flux$twig_flux[leaflitter_flux$twig_flux > 5000] <- 0   # v high twig fluxes - maybe leave in? 
leaflitter_flux$total <- with(leaflitter_flux, twig_flux + bark_flux + seed_flux + leaf_flux)
leaflitter_flux$days <- as.numeric(with(leaflitter_flux,End_date - Start_date))

Ring <- c(1:6)
litter_prod <- data.frame(Ring)
litter_prod$value <- rep(NA)
litter_prod[litter_prod$Ring == 1, "value"] <- with(leaflitter_flux[leaflitter_flux$Ring ==1,],sum(total*days)/sum(days)) * conv 
litter_prod[litter_prod$Ring == 2, "value"] <- with(leaflitter_flux[leaflitter_flux$Ring ==2,],sum(total*days)/sum(days)) * conv 
litter_prod[litter_prod$Ring == 3, "value"] <- with(leaflitter_flux[leaflitter_flux$Ring ==3,],sum(total*days)/sum(days)) * conv 
litter_prod[litter_prod$Ring == 4, "value"] <- with(leaflitter_flux[leaflitter_flux$Ring ==4,],sum(total*days)/sum(days)) * conv 
litter_prod[litter_prod$Ring == 5, "value"] <- with(leaflitter_flux[leaflitter_flux$Ring ==5,],sum(total*days)/sum(days)) * conv 
litter_prod[litter_prod$Ring == 6, "value"] <- with(leaflitter_flux[leaflitter_flux$Ring ==6,],sum(total*days)/sum(days)) * conv 

npp$R1[npp$term == "Leaf NPP"] <- litter_prod[litter_prod$Ring == 1, "value"]
npp$R2[npp$term == "Leaf NPP"] <- litter_prod[litter_prod$Ring == 2, "value"] 
npp$R3[npp$term == "Leaf NPP"] <- litter_prod[litter_prod$Ring == 3, "value"]
npp$R4[npp$term == "Leaf NPP"] <- litter_prod[litter_prod$Ring == 4, "value"]
npp$R5[npp$term == "Leaf NPP"] <- litter_prod[litter_prod$Ring == 5, "value"]
npp$R6[npp$term == "Leaf NPP"] <- litter_prod[litter_prod$Ring == 6, "value"] 
npp$notes[npp$term == "Leaf NPP"] <- "Calculated from leaf, twig, bark and seed litterfall"

stem_prod <- litter_prod
stem_prod[stem_prod$Ring == 1, "value"] <- mean(wood_production_flux[wood_production_flux$Ring == 1, "wood_production_flux"]) * conv 
stem_prod[stem_prod$Ring == 2, "value"] <- mean(wood_production_flux[wood_production_flux$Ring == 2, "wood_production_flux"]) * conv 
stem_prod[stem_prod$Ring == 3, "value"] <- mean(wood_production_flux[wood_production_flux$Ring == 3, "wood_production_flux"]) * conv 
stem_prod[stem_prod$Ring == 4, "value"] <- mean(wood_production_flux[wood_production_flux$Ring == 4, "wood_production_flux"]) * conv 
stem_prod[stem_prod$Ring == 5, "value"] <- mean(wood_production_flux[wood_production_flux$Ring == 5, "wood_production_flux"]) * conv 
stem_prod[stem_prod$Ring == 6, "value"] <- mean(wood_production_flux[wood_production_flux$Ring == 6, "wood_production_flux"]) * conv 

npp$R1[npp$term == "Stem NPP"] <- stem_prod[stem_prod$Ring == 1, "value"]
npp$R2[npp$term == "Stem NPP"] <- stem_prod[stem_prod$Ring == 2, "value"]
npp$R3[npp$term == "Stem NPP"] <- stem_prod[stem_prod$Ring == 3, "value"]
npp$R4[npp$term == "Stem NPP"] <- stem_prod[stem_prod$Ring == 4, "value"]
npp$R5[npp$term == "Stem NPP"] <- stem_prod[stem_prod$Ring == 5, "value"]
npp$R6[npp$term == "Stem NPP"] <- stem_prod[stem_prod$Ring == 6, "value"]
npp$notes[npp$term == "Stem NPP"] <- "Calculated from stem diameter + allometry. Includes all trees"

froot_prod <- litter_prod
froot_prod[froot_prod$Ring == 1, "value"] <- mean(fineroot_production_flux[fineroot_production_flux$Ring == 1, 
                                                                           "fineroot_production_flux"]) * conv 
froot_prod[froot_prod$Ring == 2, "value"] <- mean(fineroot_production_flux[fineroot_production_flux$Ring == 2, 
                                                                           "fineroot_production_flux"]) * conv
froot_prod[froot_prod$Ring == 3, "value"] <- mean(fineroot_production_flux[fineroot_production_flux$Ring == 3, 
                                                                           "fineroot_production_flux"]) * conv
froot_prod[froot_prod$Ring == 4, "value"] <- mean(fineroot_production_flux[fineroot_production_flux$Ring == 4, 
                                                                           "fineroot_production_flux"]) * conv
froot_prod[froot_prod$Ring == 5, "value"] <- mean(fineroot_production_flux[fineroot_production_flux$Ring == 5, 
                                                                           "fineroot_production_flux"]) * conv
froot_prod[froot_prod$Ring == 6, "value"] <- mean(fineroot_production_flux[fineroot_production_flux$Ring == 6, 
                                                                           "fineroot_production_flux"]) * conv

npp$R1[npp$term == "Fine Root NPP"] <- froot_prod[froot_prod$Ring == 1, "value"]
npp$R2[npp$term == "Fine Root NPP"] <- froot_prod[froot_prod$Ring == 2, "value"]
npp$R3[npp$term == "Fine Root NPP"] <- froot_prod[froot_prod$Ring == 3, "value"]
npp$R4[npp$term == "Fine Root NPP"] <- froot_prod[froot_prod$Ring == 4, "value"]
npp$R5[npp$term == "Fine Root NPP"] <- froot_prod[froot_prod$Ring == 5, "value"]
npp$R6[npp$term == "Fine Root NPP"] <- froot_prod[froot_prod$Ring == 6, "value"]
npp$notes[npp$term == "Fine Root NPP"] <- "One year's data only"

frass_production_flux$days <- as.numeric(with(frass_production_flux,End_date - Start_date))
npp$R1[npp$term == "Frass production"] <- with(frass_production_flux[frass_production_flux$Ring ==1,],
                                               sum(frass_production_flux*days)/sum(days)) * conv 
npp$R2[npp$term == "Frass production"] <- with(frass_production_flux[frass_production_flux$Ring ==2,],
                                               sum(frass_production_flux*days)/sum(days)) * conv 
npp$R3[npp$term == "Frass production"] <- with(frass_production_flux[frass_production_flux$Ring ==3,],
                                               sum(frass_production_flux*days)/sum(days)) * conv 
npp$R4[npp$term == "Frass production"] <- with(frass_production_flux[frass_production_flux$Ring ==4,],
                                               sum(frass_production_flux*days)/sum(days)) * conv 
npp$R5[npp$term == "Frass production"] <- with(frass_production_flux[frass_production_flux$Ring ==5,],
                                               sum(frass_production_flux*days)/sum(days)) * conv 
npp$R6[npp$term == "Frass production"] <- with(frass_production_flux[frass_production_flux$Ring ==6,],
                                               sum(frass_production_flux*days)/sum(days)) * conv 

ccost <- 0.3 # growth respiration cost - no idea of this value really
inout$R1[inout$term == "Rgrowth"] <- ccost * (litter_prod[1,"value"] + stem_prod[1,"value"] + froot_prod[1,"value"])
inout$R2[inout$term == "Rgrowth"] <- ccost * (litter_prod[2,"value"] + stem_prod[2,"value"] + froot_prod[2,"value"])
inout$R3[inout$term == "Rgrowth"] <- ccost * (litter_prod[3,"value"] + stem_prod[3,"value"] + froot_prod[3,"value"])
inout$R4[inout$term == "Rgrowth"] <- ccost * (litter_prod[4,"value"] + stem_prod[4,"value"] + froot_prod[4,"value"])
inout$R5[inout$term == "Rgrowth"] <- ccost * (litter_prod[5,"value"] + stem_prod[5,"value"] + froot_prod[5,"value"])
inout$R6[inout$term == "Rgrowth"] <- ccost * (litter_prod[6,"value"] + stem_prod[6,"value"] + froot_prod[6,"value"])
inout$notes[inout$term == "Rgrowth"] <- "Calculated by multiplying NPP by 0.3"
  
# Standing C pools
term <- c("Overstorey leaf", "Overstorey wood", "Understorey above-ground",
          "Fine Root", "Coarse Root", "Litter", "Coarse woody debris", 
          "Microbial biomass", "Soil C", "Mycorrhizae", "Insects")

pool <- data.frame(term)
pool$R1 <- rep(NA, length(pool$term))
pool$R2 <- rep(NA, length(pool$term))
pool$R3 <- rep(NA, length(pool$term))
pool$R4 <- rep(NA, length(pool$term))
pool$R5 <- rep(NA, length(pool$term))
pool$R6 <- rep(NA, length(pool$term))
pool$aCO2 <- rep(NA, length(pool$term))
pool$eCO2 <- rep(NA, length(pool$term))
pool$notes <- rep("", length(pool$term))

pool$R1[pool$term == "Overstorey leaf"] <- mean(leaf_pool[leaf_pool$Ring == 1, "leaf_pool"])
pool$R2[pool$term == "Overstorey leaf"] <- mean(leaf_pool[leaf_pool$Ring == 2, "leaf_pool"])
pool$R3[pool$term == "Overstorey leaf"] <- mean(leaf_pool[leaf_pool$Ring == 3, "leaf_pool"])
pool$R4[pool$term == "Overstorey leaf"] <- mean(leaf_pool[leaf_pool$Ring == 4, "leaf_pool"])
pool$R5[pool$term == "Overstorey leaf"] <- mean(leaf_pool[leaf_pool$Ring == 5, "leaf_pool"])
pool$R6[pool$term == "Overstorey leaf"] <- mean(leaf_pool[leaf_pool$Ring == 6, "leaf_pool"])
pool$notes[pool$term == "Overstorey leaf"] <- "Calculated from plant area index using constant SLA"

pool$R1[pool$term == "Overstorey wood"] <- mean(wood_pool[wood_pool$Ring == 1, "wood_pool"])
pool$R2[pool$term == "Overstorey wood"] <- mean(wood_pool[wood_pool$Ring == 2, "wood_pool"])
pool$R3[pool$term == "Overstorey wood"] <- mean(wood_pool[wood_pool$Ring == 3, "wood_pool"])
pool$R4[pool$term == "Overstorey wood"] <- mean(wood_pool[wood_pool$Ring == 4, "wood_pool"])
pool$R5[pool$term == "Overstorey wood"] <- mean(wood_pool[wood_pool$Ring == 5, "wood_pool"])
pool$R6[pool$term == "Overstorey wood"] <- mean(wood_pool[wood_pool$Ring == 6, "wood_pool"])

pool$R1[pool$term == "Fine Root"] <- mean(fineroot_pool[fineroot_pool$Ring == 1, "fineroot_pool"])
pool$R2[pool$term == "Fine Root"] <- mean(fineroot_pool[fineroot_pool$Ring == 2, "fineroot_pool"])
pool$R3[pool$term == "Fine Root"] <- mean(fineroot_pool[fineroot_pool$Ring == 3, "fineroot_pool"])
pool$R4[pool$term == "Fine Root"] <- mean(fineroot_pool[fineroot_pool$Ring == 4, "fineroot_pool"])
pool$R5[pool$term == "Fine Root"] <- mean(fineroot_pool[fineroot_pool$Ring == 5, "fineroot_pool"])
pool$R6[pool$term == "Fine Root"] <- mean(fineroot_pool[fineroot_pool$Ring == 6, "fineroot_pool"])

pool$R1[pool$term == "Soil C"] <- mean(soil_carbon_pool[soil_carbon_pool$Ring == 1, "soil_carbon_pool"])
pool$R2[pool$term == "Soil C"] <- mean(soil_carbon_pool[soil_carbon_pool$Ring == 2, "soil_carbon_pool"])
pool$R3[pool$term == "Soil C"] <- mean(soil_carbon_pool[soil_carbon_pool$Ring == 3, "soil_carbon_pool"])
pool$R4[pool$term == "Soil C"] <- mean(soil_carbon_pool[soil_carbon_pool$Ring == 4, "soil_carbon_pool"])
pool$R5[pool$term == "Soil C"] <- mean(soil_carbon_pool[soil_carbon_pool$Ring == 5, "soil_carbon_pool"])
pool$R6[pool$term == "Soil C"] <- mean(soil_carbon_pool[soil_carbon_pool$Ring == 6, "soil_carbon_pool"])

# calculate aCO2 and eCO2 results
inout$aCO2 <- rowMeans(subset(inout, select=c(R2, R3, R6)), na.rm=T)
inout$eCO2 <- rowMeans(subset(inout, select=c(R1, R4, R5)), na.rm=T)

npp$aCO2 <- rowMeans(subset(npp, select=c(R2, R3, R6)), na.rm=T)
npp$eCO2 <- rowMeans(subset(npp, select=c(R1, R4, R5)), na.rm=T)

pool$aCO2 <- rowMeans(subset(pool, select=c(R2, R3, R6)), na.rm=T)
pool$eCO2 <- rowMeans(subset(pool, select=c(R1, R4, R5)), na.rm=T)

# output tables
inout
npp
pool
