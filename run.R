#### Main script to compute EucFACE C balance fluxes and variables
#### Return units: Pools - g C m-2, fluxes - mg C m-2 d-1
#### Simplified assumptions include:
###                                  1. C fraction of fine root, leaf and lerp
###                                  2. leaching rate

###### ---------------- setting up runs -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("R/prepare.R")

###### ----------Compute c fluxes, variables, and pools-------------- ######
### LAI
lai_variable <- make_lai_variable()

### SLA
sla_variable <- make_sla_variable()

### Soil bulk density at 3 depths
### unit is kg m-3
soil_bulk_density_variable <- make_soil_bulk_density()

### Soil C pool, using soil bulk density data
### output options are shallow and all_depths data
soil_c_pool <- make_soil_carbon_pool(bk_density=soil_bulk_density_variable,
                                     return="all_depths")

### soil respiration flux
soil_respiration_flux <- make_soil_respiration_flux()


### leaf C pool
### read in c_fraction defined in constant
leaf_c_pool <- make_leaf_pool(lai_variable, sla_variable, c_fraction)


### Fine root pool
### reads in c_fraction_fr defined in constant
fineroot_c_pool <- make_fineroot_pool(c_fraction_fr)

### Canopy c production flux


### fineroot c production flux
### reads in c_fraction_fr defined in constant
fineroot_production_flux <- make_fineroot_production_flux(c_fraction_fr)

### frass c production flux
frass_production_flux <- make_frass_production_flux()

### herbivore leaf c consumption flux
### extrapolated based on frass weight, leaf area consumed and sla data
### This consumed leaf mass need to be added on top of the canopy c pool. 
herbivory_leaf_consumption_flux <- make_herbivory_leaf_consumption_flux(sla=sla_variable, 
                                                                        frass_flux=frass_production_flux)

### lerp production flux
### reads in c_fraction_lp from constant
lerp_production_flux <- make_lerp_production_flux(c_fraction_lp)

### DOC leaching flux
### Can return shallow, deep and all_depths result
### This data also contains total dissolved carbon, and dissolved inorganic carbon
### For now, return only deep option
### Also assumes leaching = 20 ml m-2 d-1
doc_leaching_flux <- make_doc_leaching_flux(return="deep")

### Litter fluxes
### This dataframe includes all of twig, bark, seed, leaf.
### reads in c_fraction coefficient from constant
leaflitter_flux <- make_leaflitter_flux(c_fraction)

### wood C pool
wood_c_pool <- make_wood_pool(ring_area,c_fraction)

### Wood C production
wood_production_flux <- make_wood_production_flux(wood_c_pool)

### Second method for the wood pool.
### See R_other/compare_wood_pool_methods.R !
wood_pool_2 <- make_wood_pool_2(ring_area,c_fraction,wood_density)

### understorey SLA
understorey_sla_variable <- make_understorey_sla_variable()


### Understorey aboveground C pool
### reads in c_fraction from constant
understorey_aboveground_biomass_pool <- make_understorey_aboveground_biomass_pool(c_fraction)

### Understorey LAI
#understorey_lai_variable <- make_understorey_lai_variable(understorey_aboveground_biomass_pool, 
#                                                          understorey_sla_variable)

### Soil microbial C pool
microbial_pool <- make_microbial_pool(soil_bulk_density_variable)

### Soil methane C flux
### still need to correct for unit (currently in ng of CH4-C per cm3)
methane_flux <- make_methane_flux()

### Root respiration flux
root_respiration_flux <- make_root_respiration_flux(fineroot_pool)

### Rh C flux
heterotrophic_respiration_flux <- make_heterotrophic_respiration_flux(soil_respiration_flux, 
                                                                      root_respiration_flux)

###### ----------Make summary tables-------------- ######
### Generate overall summary table (ignoring rings and time)
source("R/make_table.R")
overall_tables <- make_EucFACE_table()

### Generate ring-specific table (ignoring time variable)
source("R/make_table_by_ring.R")
tables_by_ring <- make_table_by_ring()

### Generate per year table (ignore ring variability)
source("R/make_table_by_year.R")
tables_by_year <- make_EucFACE_table_by_year()




###### ----------Make comparison plots, etc. -------------- ######
source("R/change_in_wood_soil_pools.R")
plot_change_in_wood_soil_microbe_pools(soil_carbon_pool,
                                       wood_pool,
                                       microbial_pool,
                                       destDir = "R_other")


###### ---------------- End -------------------- ######
