
source("R/prepare.R")

lai_variable <- make_lai_variable()

sla_variable <- make_sla_variable()

leaf_pool <- make_leaf_pool(lai_variable, sla_variable)

soil_respiration_flux <- make_soil_respiration_flux()

soil_carbon_pool <- make_soil_carbon_pool()

fineroot_pool <- make_fineroot_pool(c_fraction)

fineroot_production_flux <- make_fineroot_production_flux(c_fraction)

frass_production_flux <- make_frass_production_flux()

# incomplete   - frass production record does not match lai record
herbivory_leaf_consumption_flux <- make_herbivory_leaf_consumption_flux(sla_variable, frass_production_flux)

lerp_production_flux <- make_lerp_production_flux()

# incomplete  - needs a water flux, possibly simulated
# doc_leaching_flux <- make_doc_leaching_flux()


# Litter fluxes. This dataframe includes all of twig, bark, seed, leaf. 

leaflitter_flux <- make_leaflitter_flux(c_fraction)

wood_pool <- make_wood_pool(ring_area,c_fraction)
wood_production_flux <- make_wood_production_flux(wood_pool)

