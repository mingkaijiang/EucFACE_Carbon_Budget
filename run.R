#### Main script to compute EucFACE C balance fluxes and variables
#### Return units: Pools - g C m-2, fluxes - mg C m-2 d-1


###### ---------------- setting up runs -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("R/prepare.R")

#### Suppress warning messages
options(warn=-1)

###### ----------Add met data -------------- ######
## Soil moisture data
#pdf("output/soil_moisture_plots.pdf", width=10, height=4)
#prepare_soil_moisture_data(plot.image = T, monthly=T)
#dev.off()

### Top canopy Pressure (hPa)
#pairDF <- prepare_surface_pressure_data(plot.image=F, timestep="Monthly")

### Top canopy Rainfall (mm)
#rainDF <- prepare_rainfall_data(plot.image=F, timestep="Monthly")

### Tair (K), RH and PAR (umol m-2 s-1) top of canopy
#tair_rh_parDF <- prepare_tair_rh_par_data(timestep="Monthly")

### Wind speed top of canopy (m/s)
#windDF <- prepare_wind_data(plot.image=F,timestep="Monthly")

### tair, RH, PAR and pressure 
#tair_rh_par_presDF <- prepare_met_air_data(timestep="Monthly")

### Combine met data together
#metDF <- combine_met_data(timestep="Monthly")

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
### We can either use mean sla value or variable SLA value to calculate leaf C
### sla_option: "mean", or "variable"
leaf_c_pool <- make_leaf_pool(lai_variable, sla_variable, c_fraction,
                              sla_option = "variable")


### Fine root pool
### reads in c_fraction_fr defined in constant
fineroot_c_pool <- make_fineroot_pool(c_fraction_fr)


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
doc_leaching_flux <- make_doc_leaching_flux(depth="deep")

dic_leaching_flux <- make_dic_leaching_flux(depth="deep")

### Litter fluxes
### This dataframe includes all of twig, bark, seed, leaf.
### reads in c_fraction coefficient from constant
leaflitter_flux <- make_leaflitter_flux(c_fraction)

### sapwood C and N fraction
sapwood_cn_fraction <- make_sapwood_c_n_fraction()

### wood C pool
wood_c_pool <- make_wood_pool(ring_area,c_fraction)

### Wood C production
wood_production_flux <- make_wood_production_flux(wood_c_pool)

### Second method for the wood pool.
### See R_other/compare_wood_pool_methods.R !
wood_pool_2 <- make_wood_pool_2(ring_area,c_fraction,wood_density)

### Standing dead C pool
standing_dead_c_pool <- make_standing_dead_c_pool(ring_area=ring_area,
                                                  c_frac=c_fraction)

### understorey SLA
understorey_sla_variable <- make_understorey_sla_variable()


### Understorey aboveground C pool
### reads in c_fraction from constant
understorey_aboveground_c_pool <- make_understorey_aboveground_c_pool(c_fraction)
understorey_aboveground_c_pool_2 <- make_understorey_aboveground_c_pool_2(c_fraction)

### Understorey production flux
understorey_aboveground_production_flux <- make_understorey_aboveground_production_flux(c_fraction)

### Understorey LAI
understorey_lai_variable <- make_understorey_lai_variable(understorey_aboveground_c_pool, 
                                                          understorey_sla_variable)


### Soil microbial C pool
### top 10 cm only - Cat's data
microbial_c_pool <- make_microbial_pool(soil_bulk_density_variable)

### Yolima's data
microbial_c_pool2 <- make_microbial_pool2(soil_bulk_density_variable)

### Soil mycorrhizal production
mycorrhizal_c_pool <- make_mycorrhizal_c_pool(soil_bulk_density_variable)

### Soil methane C flux
## This is a simplified version because we didn't fill the gaps
methane_c_flux <- make_methane_flux()
## This is the version with gap-filled data, using DAMM
methane_c_flux2 <- make_methane_flux2()

### Herbivory respiration flux
herbivory_respiration_flux <- make_herbivory_respiration_flux(leaf_consumed=herbivory_leaf_consumption_flux,
                                                              frass_prod=frass_production_flux,
                                                              method="diff")


### Coarse root C pool 
coarse_root_c_pool_1 <- make_coarse_root_pool_1(c_fraction, fr_pool=fineroot_c_pool) 
coarse_root_c_pool_2 <- make_coarse_root_pool_2(c_fraction) 


#### Coarse root C production
coarse_root_production_flux_1 <- make_coarse_root_production_flux(coarse_root_c_pool_1) 
coarse_root_production_flux_2 <- make_coarse_root_production_flux(coarse_root_c_pool_2) 

### Root respiration flux
root_respiration_flux <- make_root_respiration_flux(fineroot_c_pool, coarse_root_c_pool_1)

### Rh C flux
heterotrophic_respiration_flux <- make_heterotrophic_respiration_flux(soil_respiration_flux, 
                                                                      root_respiration_flux)

#### Overstorey GPP 
overstorey_gpp_flux <- make_overstorey_gpp_flux()

### Overstorey foliage respiration
overstorey_leaf_respiration_flux <- make_overstorey_leaf_respiration_flux()

### Understorey GPP
understorey_gpp_flux <- make_understorey_GPP_flux()

### Understorey respiration
### assumes either a fixed or a function of temperature
understorey_respiration_flux <- make_understorey_respiration_flux(c_pool=understorey_aboveground_c_pool,
                                                                  c_frac=c_fraction,
                                                                  gpp=understorey_gpp_flux,
                                                                  assumption="cue")

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

###### ----------Check for C gaps-------------- ######

### GPP gaps
source("R/gpp_gap_plot.R")
gpp_gap_plot(inDF=tables_by_ring)

### Rsoil gaps
source("R/rsoil_gap_plot.R")
pdf("R_other/rsoil_gap.pdf")
rsoil_gap_plot(inDF=tables_by_ring)
dev.off()


### NEP gaps   - Note the different input file!
source("R/nep_gap_plot.R")
pdf("R_other/nep_gap.pdf")
nep_gap_plot(inDF=overall_tables)
dev.off()

### Biomass increment
source("R/biomass_increment_plot.R")
biomass_increment_plot()

### NPP allocation
source("R/make_npp_allocation_ratio.R")
make_npp_allocation_ratio()

#### TBCA
source("R/make_belowground_c_flux_allocation.R")
make_belowground_c_flux_allocation()

###### ----------Make comparison plots, etc. -------------- ######
source("R/change_in_wood_soil_pools.R")
plot_change_in_wood_soil_microbe_pools(soil_c_pool,
                                       wood_c_pool,
                                       microbial_c_pool,
                                       destDir = "R_other")


source("R/understorey_production_check.R")
understorey_production_check()


source("R/leaf_npp_and_lerp_production_plot.R")
pdf("R_other/leaf_npp_lerp_production.pdf")
leaf_npp_and_lerp_production_plot(leaf_npp=leaflitter_flux,
                                  lerp_production=lerp_production_flux,
                                  frass_production = frass_production_flux, 
                                  lai = lai_variable,
                                  insect_consumption = herbivory_leaf_consumption_flux)
dev.off()

###### ----------Make stats -------------- ######=
### Generate ratio , considering no interaction
#generate_stats_ratio(stat.model="no_interaction")
    
### Generate abs, considering no interaction
#generate_stats_abs(stat.model="no_interaction")

### Make some plots
#make_statistical_comparison_plots()

###### ---------------- End -------------------- ######
options(warn=0)
