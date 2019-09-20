###########################################################################
###########################################################################
###                                                                     ###
###                EucFACE carbon budget code repository                ###
###                                                                     ###
###########################################################################
###########################################################################
###########################################################################
#### Main script to compute EucFACE C budget fluxes and variables.
#### Return units: Pools - g C m-2, fluxes - mg C m-2 d-1
###########################################################################
####
###########################################################################
#### Core coding team: 
####                 Mingkai Jiang (m.jiang@westernsydney.edu.au) 
####                 Remko Duursma
####                 John Drake
####                 Belinda Medlyn
###########################################################################


###### -------------------- setting up runs ------------------------ ######
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("R/prepare.R")

#### Suppress warning messages
options(warn=-1)

###### ---------------------Add met data ------------------------- ######
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

### Combine met data together, also make combined met data plot
#metDF <- combine_met_data()

### Calculate mean annual temperature and precipitation
#met_ann <- calculate_annual_mean_met_data(timestep="Daily")

###### ----------Compute c fluxes, variables, and pools-------------- ######
### LAI
lai_variable <- make_lai_variable()

#### Overstorey GPP 
overstorey_gpp_flux <- make_overstorey_gpp_flux()

## without pre-treatment
#overstorey_gpp_flux2 <- make_overstorey_gpp_flux_2()

### SLA
sla_variable <- make_sla_variable()

### Soil bulk density at 3 depths
### unit is kg m-3
soil_bulk_density_variable <- make_soil_bulk_density()

### Soil C pool, using soil bulk density data
### output options are shallow and all_depths data
soil_c_pool <- make_soil_carbon_pool(bk_density=soil_bulk_density_variable,
                                     return="shallow")

## return all depth and date
soil_c_pool_all_depth <- make_soil_carbon_pool_all_depths(bk_density=soil_bulk_density_variable,
                                                          return="all_depths")

### soil respiration flux
### first method is John's
### second is Alexis's
soil_respiration_flux <- make_soil_respiration_flux()
#soil_respiration_flux2 <- make_soil_respiration_flux_2()

#compare_Rsoil(aDF=soil_respiration_flux2, jDF=soil_respiration_flux)

### VOC flux - process hourly met data within the function
### Requires: PAR (umol m-2 s-1), Tair (K), Prec (mm), Pressure (Pa), wind speed (m/s), RH
### LAI, and soil moisture (m3/m3)
#prepare_VOC_met_data(laiDF=lai_variable)

### first result based on model with soil moisture
### second set of result based on model without soil moisture
#voc_emission_flux <- make_voc_emission_flux()
voc_emission_flux <- make_voc_emission_flux2()

### leaf C pool
### read in c_fraction defined in constant
### We can either use mean sla value or variable SLA value to calculate leaf C
### sla_option: "mean", or "variable"
leaf_c_pool <- make_leaf_pool(lai_variable, sla_variable, c_fraction,
                              sla_option = "variable")


### Fine root pool
### reads in ring-specific c fraction information
fineroot_c_pool <- make_fineroot_pool()


### fineroot c production flux
fineroot_production_flux <- make_fineroot_production_flux()

### frass c production flux
frass_production_flux <- make_frass_production_flux()

### herbivore leaf c consumption flux
### extrapolated based on frass weight, leaf area consumed and sla data
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

### Leaf litter pool
leaflitter_pool <- make_leaflitter_pool(c_fraction)

### Insect pool
### method 1 is based on litter basket data
### understorey insect is based on suction data
### ground dwelling is based on pitfall data
insect_pool <- make_insect_pool(c_fraction_ins)
understorey_insect_pool <- make_understorey_insect_pool(c_frac=c_fraction_ins)
ground_dwelling_insect_pool <- make_ground_dwelling_insect_pool(c_frac=c_fraction_ins)

### sapwood C and N fraction
sapwood_cn_fraction <- make_sapwood_c_n_fraction()

### wood C pool
wood_c_pool <- make_wood_pool(ring_area,c_fraction)


### Wood C production
wood_production_flux <- make_wood_production_flux(wood_c_pool)


### Wood respiration flux
### Method 1 is Nam Jin's method
### Method 2 is a sapwood mass based method
### Method 3 is based on Roberto's three month data, temperature function fitted to Jan and Feb only
### Method 4 is based on Roberto's three month data, but with temperature function fitted to each campaign
### method 5 is based on 9 month of data, with temperature function fitted to each treatment only
#wood_respiration_flux <- make_wood_respiration_flux()
##wood_respiration_flux <- make_wood_respiration_flux_3()
##wood_respiration_flux2 <- make_wood_respiration_flux_2(wood.pool=wood_c_pool)
wood_respiration_flux <- make_wood_respiration_flux_5()
# compare_Rwood(nDF=wood_respiration_flux5, oDF=wood_respiration_flux)

### understorey SLA
understorey_sla_variable <- make_understorey_sla_variable()


### Understorey aboveground C pool
### reads in c_fraction from constant
### method 1 is based on harvest
### method 2 is based on stereo camera
understorey_aboveground_c_pool <- make_understorey_aboveground_c_pool(c_fraction_ua)
understorey_aboveground_c_pool_2 <- make_understorey_aboveground_c_pool_2(c_fraction_ua)


### Understorey production flux
### Method 2 is not used because understorey vegetation fluctuates a lot
understorey_aboveground_production_flux <- make_understorey_aboveground_production_flux(c_fraction_ua)
#understorey_aboveground_production_flux <- make_understorey_aboveground_production_flux_2(understorey_aboveground_c_pool)

### Understorey LAI
### method 1 based on harvesting data, live biomass only (hence the lowest LAI possible)
understorey_lai_variable <- make_understorey_lai_variable(understorey_aboveground_c_pool, 
                                                          understorey_sla_variable)

### Soil microbial C pool
### top 10 cm only - Cat's data
microbial_c_pool <- make_microbial_pool(soil_bulk_density_variable)

### Yolima's data
#microbial_c_pool2 <- make_microbial_pool2(soil_bulk_density_variable)

### Soil mycorrhizae pool
mycorrhizal_c_pool <- make_mycorrhizal_c_pool(micDF=microbial_c_pool)

### Soil mycorrhizae production
#mycorrhizal_c_production_flux <- make_mycorrhizal_production_flux(soil_bulk_density_variable)

### Soil methane C flux
## This is a simplified version because we didn't fill the gaps
methane_c_flux <- make_methane_flux()
## This is the version with gap-filled data, using DAMM
#methane_c_flux2 <- make_methane_flux2()

### Herbivory respiration flux
herbivory_respiration_flux <- make_herbivory_respiration_flux(leaf_consumed=herbivory_leaf_consumption_flux,
                                                              frass_prod=frass_production_flux,
                                                              method="diff")


### Coarse root C pool 
coarse_root_c_pool_1 <- make_coarse_root_pool_1(c_fraction_croot, fr_pool=fineroot_c_pool) 
#coarse_root_c_pool_2 <- make_coarse_root_pool_2(c_fraction_croot) 


#### Coarse root C production
coarse_root_production_flux_1 <- make_coarse_root_production_flux(coarse_root_c_pool_1) 
#coarse_root_production_flux_2 <- make_coarse_root_production_flux(coarse_root_c_pool_2) 

### Root respiration flux
#root_respiration_flux_old <- make_root_respiration_flux(fineroot_c_pool, 
#                                                    coarse_root_c_pool_1)

root_respiration_flux <- make_root_respiration_flux_2(fr_pool=fineroot_c_pool, 
                                                      cr_pool=coarse_root_c_pool_1)

#compare_Rroot(nDF=root_respiration_flux,
#              oDF=root_respiration_flux_old)


### Rh C flux
heterotrophic_respiration_flux <- make_heterotrophic_respiration_flux(soil_respiration_flux, 
                                                                      root_respiration_flux)

### Overstorey foliage respiration
overstorey_leaf_respiration_flux <- make_overstorey_leaf_respiration_flux()

### Understorey GPP
understorey_gpp_flux <- make_understorey_GPP_flux3()

### Understorey respiration
### assumes either a fixed or a function of temperature
understorey_respiration_flux <- make_understorey_respiration_flux(c_pool=understorey_aboveground_c_pool,
                                                                  c_frac=c_fraction_ua,
                                                                  gpp=understorey_gpp_flux,
                                                                  assumption="maespa_all")



### Delta pools
delta_soil_c_pool <- make_delta_soil_pool_function(inDF=soil_c_pool, var.col=3)

delta_leaf_c_pool <- make_delta_leaf_pool_function(inDF=leaf_c_pool, var.col=3)

delta_leaf_c_pool <- make_delta_pool_function(inDF=leaf_c_pool, var.col=3)

delta_wood_c_pool <- make_delta_wood_pool_function(inDF=wood_c_pool, var.col=3)

#delta_fineroot_c_pool <- make_delta_fineroot_pool_function(inDF=fineroot_c_pool, var.col=3)
delta_fineroot_c_pool <- make_delta_fineroot_pool_function_2(inDF=fineroot_c_pool, var.col=3)

delta_coarse_root_c_pool <- make_delta_coarseroot_pool_function(inDF=coarse_root_c_pool_1, var.col=3)

delta_understorey_aboveground_c_pool <- make_delta_ua_pool_function(inDF=understorey_aboveground_c_pool, var.col=5)

delta_understorey_aboveground_c_pool_2 <- make_delta_ua_pool_function_4(inDF=understorey_aboveground_c_pool_2, var.col=3)

delta_microbial_c_pool <- make_delta_microbial_pool_function(inDF=microbial_c_pool, var.col=3)

delta_mycorrhizal_c_pool <- make_delta_mycorrhizal_pool_function(inDF=mycorrhizal_c_pool, var.col=3)

delta_leaflitter_pool <- make_delta_leaflitter_pool_function(inDF=leaflitter_pool, var.col=6)

delta_insect_pool <- make_delta_insect_pool_function(inDF=insect_pool, var.col=3)

delta_ground_dwelling_insect_pool <- make_delta_ground_dwelling_insect_pool_function(inDF=ground_dwelling_insect_pool, var.col=3)


###### ----------Make summary tables-------------- ######
###### This is a summary table of all raw data, without LAI as a covariate

### Generate overall summary table (ignoring rings and time)
#source("R/make_table.R")
#overall_tables <- make_EucFACE_table()

### Generate per year table (ignore ring variability)
#source("R/make_table_by_year.R")
#tables_by_year <- make_EucFACE_table_by_year()

### Generate ring-specific table (ignoring time variable)
#source("R/make_table_by_ring.R")
#tables_by_ring <- make_table_by_ring()
#inDF <- tables_by_ring

###### ----------Check for C gaps-------------- ######
### GPP gaps
#source("R/gpp_gap_plot.R")
#gpp_gap_plot(inDF=tables_by_ring)

### Rsoil gaps
#source("R/rsoil_gap_plot.R")
#rsoil_gap_plot(inDF=tables_by_ring)

### Plot a combined gpp and rsoil gap plot
### To plot, you need to go into the function
#source("R/gpp_and_rsoil_gap_plot.R")
#gpp_and_rsoil_gap_plot(inDF=tables_by_ring)

#source("R/nep_gap_plot.R")
#nep_gap_plot(inDF=tables_by_ring)

#source("R/make_eCO2_effect_on_GPP_plot.R")
#make_eCO2_effect_on_GPP_plot(inDF=tables_by_ring)


###### ----------normalization-------------- ######
### Generate abs on all variables, 
### considering no interaction but with pre-treatment LAI as covariate
### also output predicted values based on the stat model for all variables
### All stats for fluxes are based on annual rate
#source("R/stats/generate_stats_abs_covariate.R")
#generate_stats_abs_covariate(stat.model="no_interaction_with_covariate")

### Make some plots
#source("R/make_statistical_comparison_plots.R")
#make_statistical_comparison_plots()


###### ----------Add predicted values back onto all fluxes and pools-------------- ######
###### Predict CO2 effect based on LAI, for all variables again
###### Note that all fluxes are now annualized

### overstorey gpp flux
overstorey_gpp_flux_ann <- make_overstorey_gpp_treatment_abs_effect_statistics(inDF=overstorey_gpp_flux, 
                                                                               var.cond="ann.flux", var.col=3,
                                                                               date.as.factor=T,
                                                                               stat.model="no_interaction_with_covariate",
                                                                               return.outcome="predicted")

### Understorey GPP
understorey_gpp_flux_ann <- make_understorey_gpp_treatment_abs_effect_statistics(inDF=understorey_gpp_flux, 
                                                                                 var.cond="ann.flux", var.col=3,
                                                                                 date.as.factor=T,
                                                                                 stat.model="no_interaction_with_covariate",
                                                                                 return.outcome="predicted")

### Overstorey Leaf respiration
overstorey_leaf_respiration_flux_ann <- make_overstorey_ra_leaf_treatment_abs_effect_statistics(inDF=overstorey_leaf_respiration_flux, 
                                                                                                var.cond="ann.flux", var.col=3,
                                                                                                date.as.factor=T,
                                                                                                stat.model="no_interaction_with_covariate",
                                                                                                return.outcome="predicted")

### Wood respiration
wood_respiration_flux_ann <- make_ra_wood_treatment_abs_effect_statistics(inDF=wood_respiration_flux, 
                                                                          var.cond="flux", var.col=5,
                                                                          date.as.factor=T,
                                                                          stat.model="no_interaction_with_covariate",
                                                                          return.outcome="predicted")

### Root respiration
root_respiration_flux_ann <- make_ra_root_treatment_abs_effect_statistics(inDF=root_respiration_flux, 
                                                                          var.cond="flux", var.col=5,
                                                                          date.as.factor=T,
                                                                          stat.model="no_interaction_with_covariate",
                                                                          return.outcome="predicted")
### Understorey respiration
understorey_respiration_flux_ann <- make_ra_und_treatment_abs_effect_statistics(inDF=understorey_respiration_flux, 
                                                                                var.cond="flux", var.col=5,
                                                                                date.as.factor=T,
                                                                                stat.model="no_interaction_with_covariate",
                                                                                return.outcome="predicted")

### Frass production
frass_production_flux_ann <- make_frass_treatment_abs_effect_statistics(inDF=frass_production_flux, 
                                                                        var.cond="flux", var.col=5,
                                                                        date.as.factor=T,
                                                                        stat.model="no_interaction_with_covariate",
                                                                        return.outcome="predicted")

### herbivory leaf consumption flux
herbivory_leaf_consumption_flux_ann <- make_hb_cons_treatment_abs_effect_statistics(inDF=herbivory_leaf_consumption_flux, 
                                                                                    var.cond="flux", var.col=5,
                                                                                    date.as.factor=T,
                                                                                    stat.model="no_interaction_with_covariate",
                                                                                    return.outcome="predicted")

### Herbivory respiration
herbivory_respiration_flux_ann <- make_r_hb_treatment_abs_effect_statistics(inDF=herbivory_respiration_flux, 
                                                                            var.cond="flux", var.col=5,
                                                                            date.as.factor=T,
                                                                            stat.model="no_interaction_with_covariate",
                                                                            return.outcome="predicted")

### Lerp production
lerp_production_flux_ann <- make_lp_treatment_abs_effect_statistics(inDF=lerp_production_flux, 
                                                                    var.cond="flux", var.col=5,
                                                                    date.as.factor=T,
                                                                    stat.model="no_interaction_with_covariate",
                                                                    return.outcome="predicted")

### soil respiration
soil_respiration_flux_ann <- make_rsoil_treatment_abs_effect_statistics(inDF=soil_respiration_flux, 
                                                                        var.cond="flux", var.col=5,
                                                                        date.as.factor=T,
                                                                        stat.model="no_interaction_with_covariate",
                                                                        return.outcome="predicted")

### DOC leaching
doc_leaching_flux_ann <- make_doc_treatment_abs_effect_statistics(inDF=doc_leaching_flux, 
                                                                  var.cond="flux", var.col=5,
                                                                  date.as.factor=T,
                                                                  stat.model="no_interaction_with_covariate",
                                                                  return.outcome="predicted")

### CH4 uptake - un-gap filled data
methane_c_flux_ann <- make_ch4_treatment_abs_effect_statistics(inDF=methane_c_flux, 
                                                               var.cond="flux", var.col=3,
                                                               date.as.factor=T,
                                                               stat.model="no_interaction_with_covariate",
                                                               return.outcome="predicted")

### VOC
voc_c_flux_ann <- make_voc_treatment_abs_effect_statistics(inDF=voc_emission_flux, 
                                                           var.cond="flux", var.col=3,
                                                           date.as.factor=T,
                                                           stat.model="no_interaction_with_covariate",
                                                           return.outcome="predicted")

### Leaflitter flux
leaflitter_flux_ann <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                        var.cond="flux", var.col=6,
                                                                        date.as.factor=T,
                                                                        stat.model="no_interaction_with_covariate",
                                                                        return.outcome="predicted")  

### twig litter flux
twiglitter_flux_ann <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                        var.cond="flux", var.col=3,
                                                                        date.as.factor=T,
                                                                        stat.model="no_interaction_with_covariate",
                                                                        return.outcome="predicted")

### bark litter flux
barklitter_flux_ann <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                        var.cond="flux", var.col=4,
                                                                        date.as.factor=T,
                                                                        stat.model="no_interaction_with_covariate",
                                                                        return.outcome="predicted") 

### Seed litter flux
seedlitter_flux_ann <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                        var.cond="flux", var.col=5,
                                                                        date.as.factor=T,
                                                                        stat.model="no_interaction_with_covariate",
                                                                        return.outcome="predicted")

### Wood production flux
wood_production_flux_ann <- make_wood_prod_treatment_abs_effect_statistics(inDF=wood_production_flux, 
                                                                           var.cond="flux", var.col=5,
                                                                           date.as.factor=T,
                                                                           stat.model="no_interaction_with_covariate",
                                                                           return.outcome="predicted") 

### Fineroot production flux
fineroot_production_flux_ann <- make_froot_prod_treatment_abs_effect_statistics(inDF=fineroot_production_flux, 
                                                                                var.cond="flux", var.col=5,
                                                                                date.as.factor=T,
                                                                                stat.model="no_interaction_with_covariate",
                                                                                return.outcome="predicted") 

### Coarseroot production
coarse_root_production_flux_ann <- make_croot_prod_treatment_abs_effect_statistics(inDF=coarse_root_production_flux_1, 
                                                                                   var.cond="flux", var.col=5,
                                                                                   date.as.factor=T,
                                                                                   stat.model="no_interaction_with_covariate",
                                                                                   return.outcome="predicted")

### Understorey aboveground production
understorey_aboveground_production_flux_ann <- make_und_prod_treatment_abs_effect_statistics(inDF=understorey_aboveground_production_flux, 
                                                                                             var.cond="flux", var.col=5,
                                                                                             date.as.factor=T,
                                                                                             stat.model="no_interaction_with_covariate",
                                                                                             return.outcome="predicted")

### Understorey aboveground litter 
understorey_litter_production_flux_ann <- make_und_prod_treatment_abs_effect_statistics(inDF=understorey_aboveground_production_flux, 
                                                                                        var.cond="flux", var.col=6,
                                                                                        date.as.factor=T,
                                                                                        stat.model="no_interaction_with_covariate",
                                                                                        return.outcome="predicted")
### Rh respiration
heterotrophic_respiration_flux_ann <- make_rh_treatment_abs_effect_statistics(inDF=heterotrophic_respiration_flux, 
                                                                              var.cond="flux", var.col=5,
                                                                              date.as.factor=T,
                                                                              stat.model="no_interaction_with_covariate",
                                                                              return.outcome="predicted")

### Mycorrhizal production
#mycorrhizal_c_production_flux_ann <- make_myc_production_treatment_abs_effect_statistics(inDF=mycorrhizal_c_production_flux, 
#                                                                              var.cond="flux", var.col=5,
#                                                                              date.as.factor=T,
#                                                                              stat.model="no_interaction_with_covariate",
#                                                                              return.outcome="predicted")

### Soil C
soil_c_pool_ann <- make_soilc_treatment_abs_effect_statistics(inDF=soil_c_pool, 
                                                              var.cond="pool", var.col=3,
                                                              date.as.factor=T,
                                                              stat.model="no_interaction_with_covariate",
                                                              return.outcome="predicted")

### Leaf C
leaf_c_pool_ann <- make_leafc_treatment_abs_effect_statistics(inDF=leaf_c_pool, 
                                                              var.cond="pool", var.col=3,
                                                              date.as.factor=T,
                                                              stat.model="no_interaction_with_covariate",
                                                              return.outcome="predicted")

### Wood C pool
wood_c_pool_ann <- make_woodc_treatment_abs_effect_statistics(inDF=wood_c_pool, 
                                                              var.cond="pool", var.col=3,
                                                              date.as.factor=T,
                                                              stat.model="no_interaction_with_covariate",
                                                              return.outcome="predicted") 

### Fineroot C pool
fineroot_c_pool_ann <- make_frootc_treatment_abs_effect_statistics(inDF=fineroot_c_pool, 
                                                                   var.cond="pool", var.col=3,
                                                                   date.as.factor=T,
                                                                   stat.model="no_interaction_with_covariate",
                                                                   return.outcome="predicted")

### Coarseroot C pool
coarse_root_c_pool_ann <- make_crootc_treatment_abs_effect_statistics(inDF=coarse_root_c_pool_1, 
                                                                      var.cond="pool", var.col=3,
                                                                      date.as.factor=T,
                                                                      stat.model="no_interaction_with_covariate",
                                                                      return.outcome="predicted")

### Understorey aboveground C pool
understorey_aboveground_c_pool_ann <- make_uac_treatment_abs_effect_statistics(inDF=understorey_aboveground_c_pool, 
                                                                               var.cond="pool", var.col=5,
                                                                               date.as.factor=T,
                                                                               stat.model="no_interaction_with_covariate",
                                                                               return.outcome="predicted")

understorey_aboveground_c_pool_2_ann <- make_uac_treatment_abs_effect_statistics(inDF=understorey_aboveground_c_pool_2, 
                                                                                 var.cond="pool", var.col=3,
                                                                                 date.as.factor=T,
                                                                                 stat.model="no_interaction_with_covariate",
                                                                                 return.outcome="predicted")

### Microbial C pool
microbial_c_pool_ann <- make_micc_treatment_abs_effect_statistics(inDF=microbial_c_pool, 
                                                                  var.cond="pool", var.col=3,
                                                                  date.as.factor=T,
                                                                  stat.model="no_interaction_with_covariate",
                                                                  return.outcome="predicted")

### Mycorrhizal C pool
mycorrhizal_c_pool_ann <- make_mycc_treatment_abs_effect_statistics(inDF=mycorrhizal_c_pool, 
                                                                    var.cond="pool", var.col=3,
                                                                    date.as.factor=T,
                                                                    stat.model="no_interaction_with_covariate",
                                                                    return.outcome="predicted")

### Leaf litter C pool
leaflitter_pool_ann <- make_litc_treatment_abs_effect_statistics(inDF=leaflitter_pool, 
                                                                 var.cond="pool", var.col=6,
                                                                 date.as.factor=T,
                                                                 stat.model="no_interaction_with_covariate",
                                                                 return.outcome="predicted")

### Insect pool
insect_pool_ann <- make_insc_treatment_abs_effect_statistics(inDF=insect_pool, 
                                                             var.cond="pool", var.col=3,
                                                             date.as.factor=T,
                                                             stat.model="no_interaction_with_covariate",
                                                             return.outcome="predicted")

understorey_insect_pool_ann <- make_insc_treatment_abs_effect_statistics(inDF=understorey_insect_pool, 
                                                                         var.cond="pool", var.col=3,
                                                                         date.as.factor=T,
                                                                         stat.model="no_interaction_with_covariate",
                                                                         return.outcome="predicted")

ground_dwelling_insect_pool_ann <- make_insc_treatment_abs_effect_statistics(inDF=ground_dwelling_insect_pool, 
                                                                             var.cond="pool", var.col=3,
                                                                             date.as.factor=T,
                                                                             stat.model="no_interaction_with_covariate",
                                                                             return.outcome="predicted")

### Delta pools
delta_soil_c_pool_ann <- make_delta_soil_pool_treatment_abs_effect(inDF=soil_c_pool_ann, var.col=10)

delta_leaf_c_pool_ann <- make_delta_leaf_pool_treatment_abs_effect(inDF=leaf_c_pool_ann, var.col=12)

delta_wood_c_pool_ann <- make_delta_wood_pool_treatment_abs_effect(inDF=wood_c_pool_ann, var.col=14)

delta_fineroot_c_pool_ann <- make_delta_fineroot_pool_treatment_abs_effect_2(inDF=fineroot_c_pool_ann, var.col=14)

delta_coarse_root_c_pool_ann <- make_delta_coarseroot_pool_treatment_abs_effect(inDF=coarse_root_c_pool_ann, var.col=13)

delta_understorey_aboveground_c_pool_ann <- make_delta_ua_pool_treatment_abs_effect(inDF=understorey_aboveground_c_pool_ann, var.col=14)

delta_understorey_aboveground_c_pool_2_ann <- make_delta_ua_pool_treatment_abs_effect_4(inDF=understorey_aboveground_c_pool_2_ann, var.col=12)

delta_microbial_c_pool_ann <- make_delta_microbial_pool_treatment_abs_effect(inDF=microbial_c_pool_ann, var.col=11)

delta_mycorrhizal_c_pool_ann <- make_delta_mycorrhizal_pool_treatment_abs_effect(inDF=mycorrhizal_c_pool_ann, var.col=11)

delta_leaflitter_pool_ann <- make_delta_leaflitter_pool_treatment_abs_effect(inDF=leaflitter_pool_ann, var.col=15)

delta_insect_pool_ann <- make_delta_insect_pool_treatment_abs_effect(inDF=insect_pool_ann, var.col=12)

delta_ground_dwelling_insect_pool_ann <- make_delta_ground_dwelling_insect_pool_treatment_abs_effect(inDF=ground_dwelling_insect_pool_ann, var.col=12)


###### ----------Make summary tables-------------- ######
###### This is a summary table of all predicted data

### Generate ring-specific table (ignoring time variable)
### This table is predicted based on average LAI
source("R/make_table_by_ring_predicted.R")
tables_by_ring_predicted <- make_table_by_ring_predicted()

inDF <- tables_by_ring_predicted

write.csv(inDF$inout, "output/inout.csv", row.names=F)
write.csv(inDF$npp, "output/npp.csv", row.names=F)
write.csv(inDF$delta_pool, "output/delta_pool.csv", row.names=F)
write.csv(inDF$pool, "output/pool.csv", row.names=F)


###### ----------Check for C gaps-------------- ######
### GPP and Rsoil
source("R/gpp_and_rsoil_gap_unbootstrap_plot_2.R")
gpp_and_rsoil_gap_unbootstrap_plot_2(inDF=tables_by_ring_predicted)

### NEP gaps   - Note the different input file!
source("R/nep_gap_unbootstrap_plot_3.R")
nep_gap_unbootstrap_plot_3(inDF=tables_by_ring_predicted)

### CUE
source("R/cue_unbootstrapped_calculation.R")
cueDF <- cue_unbootstrapped_calculation(inDF=tables_by_ring_predicted)


### Make some plots
#source("R/make_statistical_comparison_plots.R")
#make_statistical_comparison_plots()

source("R/make_statistical_comparison_plots_based_on_ring_level_data.R")
make_statistical_comparison_plots_based_on_ring_level_data(inDF=tables_by_ring_predicted)

#source("R/make_eCO2_effect_on_GPP_plot_with_covariate.R")
#make_eCO2_effect_on_GPP_plot_with_covariate_predicted()

source("R/make_eCO2_effect_on_GPP_plot_with_covariate_predicted_alternative_plot_color_based_on_ring_level_data_2.R")
make_eCO2_effect_on_GPP_plot_with_covariate_predicted_alternative_plot_color_based_on_ring_level_data_2(inDF=tables_by_ring_predicted)

### all supplementary figures
source("R/plot_combined_figures.R")

### make a summary table for all key values used in the manuscript
#report_key_values_for_manuscript()


###### ---------------- End -------------------- ######
options(warn=0)