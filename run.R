#### Main script to compute EucFACE C balance fluxes and variables


###### ---------------- setting up runs -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("R/prepare.R")


###### ----------Compute fluxes, variables, and pools-------------- ######
lai_variable <- make_lai_variable()

sla_variable <- make_sla_variable()

leaf_pool <- make_leaf_pool(lai_variable, sla_variable)

soil_respiration_flux <- make_soil_respiration_flux()

soil_carbon_pool <- make_soil_carbon_pool()

fineroot_pool <- make_fineroot_pool()

fineroot_production_flux <- make_fineroot_production_flux()

frass_production_flux <- make_frass_production_flux()

herbivory_leaf_consumption_flux <- make_herbivory_leaf_consumption_flux(sla_variable, frass_production_flux)

lerp_production_flux <- make_lerp_production_flux()

doc_leaching_flux <- make_doc_leaching_flux()

# Litter fluxes. This dataframe includes all of twig, bark, seed, leaf. 
leaflitter_flux <- make_leaflitter_flux(c_fraction)

wood_pool <- make_wood_pool(ring_area,c_fraction)

wood_production_flux <- make_wood_production_flux(wood_pool)

# Second method for the wood pool.
# See R_other/compare_wood_pool_methods.R !
wood_pool_2 <- make_wood_pool_2(ring_area,c_fraction,wood_density)

## understorey stuffs
understorey_sla_variable <- make_understorey_sla_variable()
understorey_aboveground_biomass_pool <- make_understorey_aboveground_biomass_pool()
#understorey_lai_variable <- make_understorey_lai_variable(understorey_aboveground_biomass_pool, 
#                                                          understorey_sla_variable)

soil_bulk_density_variable <- make_soil_bulk_density()

microbial_pool <- make_microbial_pool(soil_bulk_density_variable)

# still need to correct for unit (currently in ng of CH4-C per cm3)
methane_flux <- make_methane_flux()

root_respiration_flux <- make_root_respiration_flux(fineroot_pool)

heterotrophic_respiration_flux <- make_heterotrophic_respiration_flux(soil_respiration_flux, 
                                                                      root_respiration_flux)

###### ----------Make summary tables-------------- ######
### Generate overall summary table (ignoring rings and time)
source("R/make_table.R")
overall_tables <- make_EucFACE_table()

### Generate ring-specific table (ignoring time variable)
source("R/make_table_by_ring.R")
tables_by_ring <- make_table_by_ring()

###### ----------Make comparison plots, etc. -------------- ######
source("R/change_in_wood_soil_pools.R")
plot_change_in_wood_soil_microbe_pools(soil_carbon_pool,
                                       wood_pool,
                                       microbial_pool,
                                       destDir = "R_other")


###### ---------------- End -------------------- ######
