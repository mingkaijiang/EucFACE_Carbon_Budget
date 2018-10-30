#### Definitions of global constants


# Biomass is 50% carbon
c_fraction <- 0.5

# need to add fine root specific c fraction (from Juan)
# for now, use c_fraction value!!!
c_fraction_fr <- 0.5

# lerp C content (from Andrew)
c_fraction_lp <- 0.78

# water fraction in biomass
water_frac <- 0.5

# g to mg
g_to_mg <- 1000.0

# g to kg
g_to_kg <- 0.001

# cm2 to m2
cm2_to_m2 <- 0.0001

# cm3 to m3
cm3_to_m3 <- 1e-6

# litter basket area m-2
frass_basket_area <- 0.1979

# ring diameter m
ring_diameter <- 25

# ring ground area m2
ring_area <- pi * (ring_diameter/2)^2

# Wood density (g cm-3), average of 10 entries in global wood density 
# database of Zanne et al. (SD = 0.1)
wood_density <- 0.827

# number of days in a month
ndays_in_month <- 30.0

# understorey biomass harvest strip area
strip_area <- 0.1

# convert production flux from mg m-2 d-1 to g m-2 yr-1
conv <- 365 / 1000  

# ccost growth respiration based on Drake et al. 2018 NP.
ccost <- 0.3

# root respiration coefficients
# derived from Whole Tree Chamber experiment 3
# based on 1-year seedlings of Eucalyptus Tereticornis
# this basal respiration rate is for fineroot only!
Rcoef_fr <- 4.425 # nmol CO2 m-2 s-1

# This is the base rate
Rbase <- 2.26

## fraction of coarse root at top 30 cm of soil
cr_at_top_soil <- 0.5

# respiration rate is for coarseroot only!
Rcoef_cr <- 1.33 # nmol CO2 m-2 s-1

# understorey cue
under_cue <- 0.5

# insect cue
insect_cue <- 0.5
