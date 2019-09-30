###########################################################################
###########################################################################
###                                                                     ###
###                EucFACE carbon budget code repository                ###
###                                                                     ###
###########################################################################
###########################################################################
#### Core coding team: 
####                 Mingkai Jiang (m.jiang@westernsydney.edu.au) 
####                 Remko Duursma
####                 John Drake
####                 Belinda Medlyn
###########################################################################
###########################################################################
#### Short description: 
####       Main script to compute EucFACE C budget fluxes and variables.
####       Return units: Pools - g C m-2, fluxes - mg C m-2 d-1
###########################################################################
###########################################################################
#### Code structure:
#### There are in total 8 chunk steps:
#### Step 1: Prepare the basics, including R package, constants, sourcing code scripts;
#### Step 2: (Optional) Prepare met data for simulations;
#### Step 3: Calculate C budget fluxes, pools, and variables;
#### Step 4: Make summary tables and figures,
####         based on un-normalized results;
#### Step 5: Normalize all responses to pretreatment LAI;
#### Step 6: Make summary table, based on normalized results;
#### Step 7: Perform data assimilation to estimate uncertainties,
####         and NPPmyco;
#### Step 8: Return to C budget and generate figures.
####
###########################################################################
###########################################################################
#### Notes:
#### 1. functions that take a long time to run:
####    make_leafc_treatment_abs_effect_statistics()
####    DA fitting functions (if long chain is used)
#### 2. to plot figures, we need to go into functions itself for many plotting scripts
####
###########################################################################
###########################################################################
####                                   |
####                                   |
####                                  Ready
####                                   |
####                                 Steady
####                                   |
####                                   Go!
####                                   |
###########################################################################
###                Step 1: Set up the basics                            ###
###                                                                     ###
###########################################################################
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("R/prepare.R")

#### Suppress warning messages
options(warn=-1)

###########################################################################
###                Step 2: Prepare met data                             ###
###                          OPTIONAL                                   ###
###########################################################################
### Soil moisture data
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

### Make a Whitaker diagram to show EucFACE in the context of global temperature and precipitation
make_whitaker_diagram()


###########################################################################
###          Step 3: Compute C fluxes, pools & variables                ###
###                                                                     ###
###########################################################################
### LAI
lai_variable <- make_lai_variable()

#### Overstorey GPP 
overstorey_gpp_flux <- make_overstorey_gpp_flux()

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
soil_respiration_flux <- make_soil_respiration_flux()

### VOC flux - process hourly met data within the function
### Requires: PAR (umol m-2 s-1), Tair (K), Prec (mm), Pressure (Pa), wind speed (m/s), RH
### LAI, and soil moisture (m3/m3)
#prepare_VOC_met_data(laiDF=lai_variable)


### compare isoprene contribution against monoterpene in total VOC
compare_voc_fluxes()

### result based on model without soil moisture
voc_emission_flux <- make_voc_emission_flux2()

### leaf C pool
### read in c_fraction defined in constant
### We can either use mean sla value or variable SLA value to calculate leaf C
### sla_option: "mean", or "variable"
leaf_c_pool <- make_leaf_pool(lai_variable, sla_variable, c_fraction,
                              sla_option = "variable")


### Fine root pool (< 2 mm)
fineroot_c_pool <- make_fineroot_pool_2()

### fineroot c production flux
fineroot_production_flux <- make_fineroot_production_flux_2()

### Coarseroot pool (2 - 3 mm)
### Note:
### both Juan and Johanna's data gives similar result,
### in terms of the Froot/Croot fraction, 
### despite they have slightly different definitions of croot.
### This in turn means we have higher Rroot (~ 600 g C m-2 yr-1), 
### about more than half in aCO2 rings, and about half in eCO2 rings. 
### We have a pretty good estimate of the Froot/Croot fraction in top soil, 
### in that the variability is relatively low. 
### The variability of coarseroot biomass in 10 - 30 cm soil is large. 
#coarseroot_c_pool_old <- make_coarse_root_pool(froot=fineroot_c_pool)

### estimate coarseroot pool based on a relationship 
### between f/c ~ fineroot biomass, at two depths
coarseroot_c_pool <- make_coarse_root_pool_4(bkDF=soil_bulk_density_variable)

### coarseroot c production
#coarseroot_production_flux <- make_coarse_root_production_flux(cr_pool=coarseroot_c_pool)
coarseroot_production_flux <- make_coarse_root_production_flux_2(inDF=coarseroot_c_pool)


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
### based on 9 month of data, with temperature function fitted to each treatment only
wood_respiration_flux <- make_wood_respiration_flux_5()

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


### Bole root C pool 
bole_root_c_pool <- make_bole_root_pool(c_fraction_croot, fr_pool=fineroot_c_pool,
                                        cr_pool=coarseroot_c_pool) 

#### Bole root C production
bole_root_production_flux <- make_bole_root_production_flux(bole_root_c_pool) 

### Root respiration flux
## based on treatment-specific respiration rates
#root_respiration_flux_old <- make_root_respiration_flux_1(froot=fineroot_c_pool, 
#                                                          croot=coarseroot_c_pool)
## based on average respiration rates
#root_respiration_flux_old <- make_root_respiration_flux_2(froot=fineroot_c_pool, 
#                                                      croot=coarseroot_c_pool_old)

root_respiration_flux <- make_root_respiration_flux_2(froot=fineroot_c_pool, 
                                                      croot=coarseroot_c_pool)

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

#delta_leaf_c_pool_old <- make_delta_leaf_pool_function(inDF=leaf_c_pool, var.col=3)
#delta_leaf_c_pool <- make_delta_pool_function(inDF=leaf_c_pool, var.col=3)
delta_leaf_c_pool <- make_delta_leaf_pool_function_2(inDF=leaf_c_pool, var.col=3)

delta_wood_c_pool <- make_delta_wood_pool_function(inDF=wood_c_pool, var.col=3)

#delta_fineroot_c_pool_old <- make_delta_fineroot_pool_function_2(inDF=fineroot_c_pool, var.col=3)
delta_fineroot_c_pool <- make_delta_fineroot_pool_function_3(inDF=fineroot_c_pool, var.col=3)

delta_coarse_root_c_pool <- make_delta_coarseroot_pool_function(inDF=coarseroot_c_pool, var.col=3)

delta_bole_root_c_pool <- make_delta_boleroot_pool_function(inDF=bole_root_c_pool, var.col=3)

delta_understorey_aboveground_c_pool <- make_delta_ua_pool_function(inDF=understorey_aboveground_c_pool, var.col=5)

delta_understorey_aboveground_c_pool_2 <- make_delta_ua_pool_function_4(inDF=understorey_aboveground_c_pool_2, var.col=3)

delta_microbial_c_pool <- make_delta_microbial_pool_function(inDF=microbial_c_pool, var.col=3)

delta_mycorrhizal_c_pool <- make_delta_mycorrhizal_pool_function(inDF=mycorrhizal_c_pool, var.col=3)

delta_leaflitter_pool <- make_delta_leaflitter_pool_function(inDF=leaflitter_pool, var.col=6)

delta_insect_pool <- make_delta_insect_pool_function(inDF=insect_pool, var.col=3)

delta_ground_dwelling_insect_pool <- make_delta_ground_dwelling_insect_pool_function(inDF=ground_dwelling_insect_pool, var.col=3)



###########################################################################
###          Step 4: Make summary tables and figures                    ###
###                  based on data without LAI as a covariate           ###
###                  commented out at the moment                        ###
###########################################################################

### Generate overall summary table (ignoring rings and time)
source("R/un_normalized/make_table.R")
overall_tables <- make_EucFACE_table()

### Generate per year table (ignore ring variability)
source("R/un_normalized/make_table_by_year.R")
tables_by_year <- make_EucFACE_table_by_year()

### Generate ring-specific table (ignoring time variable)
source("R/un_normalized/make_table_by_ring.R")
tables_by_ring <- make_table_by_ring()

###### ----------Check for C gaps-------------- ######
### Plot a combined gpp and rsoil gap plot
### To plot, you need to go into the function
#source("R/un_normalized/gpp_and_rsoil_gap_plot.R")
#gpp_and_rsoil_gap_plot(inDF=tables_by_ring)
#
#source("R/un_normalized/nep_gap_plot.R")
#nep_gap_plot(inDF=tables_by_ring)
#
#source("R/un_normalized/make_eCO2_effect_on_GPP_plot.R")
#make_eCO2_effect_on_GPP_plot(inDF=tables_by_ring)

###########################################################################
###    Step 5: Normalize response with LAI as a covariate               ###
###              Note that all fluxes are now annualized                ###
###########################################################################
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


### Coarseroot production flux
coarseroot_production_flux_ann <- make_croot_prod_treatment_abs_effect_statistics(inDF=coarseroot_production_flux, 
                                                                                var.cond="flux", var.col=5,
                                                                                date.as.factor=T,
                                                                                stat.model="no_interaction_with_covariate",
                                                                                return.outcome="predicted") 

### Boleroot production
bole_root_production_flux_ann <- make_broot_prod_treatment_abs_effect_statistics(inDF=bole_root_production_flux, 
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
coarseroot_c_pool_ann <- make_crootc_treatment_abs_effect_statistics(inDF=coarseroot_c_pool, 
                                                                   var.cond="pool", var.col=3,
                                                                   date.as.factor=T,
                                                                   stat.model="no_interaction_with_covariate",
                                                                   return.outcome="predicted")

### Boleroot C pool
bole_root_c_pool_ann <- make_brootc_treatment_abs_effect_statistics(inDF=bole_root_c_pool, 
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
delta_soil_c_pool_ann <- make_delta_soil_pool_treatment_abs_effect(inDF=soil_c_pool_ann, var.col=7)

delta_leaf_c_pool_ann <- make_delta_leaf_pool_treatment_abs_effect(inDF=leaf_c_pool_ann, var.col=7)

delta_wood_c_pool_ann <- make_delta_wood_pool_treatment_abs_effect(inDF=wood_c_pool_ann, var.col=10)

delta_fineroot_c_pool_ann <- make_delta_fineroot_pool_treatment_abs_effect_2(inDF=fineroot_c_pool_ann, var.col=10)

delta_coarseroot_c_pool_ann <- make_delta_coarseroot_pool_treatment_abs_effect(inDF=coarseroot_c_pool_ann, var.col=10)

delta_bole_root_c_pool_ann <- make_delta_boleroot_pool_treatment_abs_effect(inDF=bole_root_c_pool_ann, var.col=8)

delta_understorey_aboveground_c_pool_ann <- make_delta_ua_pool_treatment_abs_effect(inDF=understorey_aboveground_c_pool_ann, var.col=10)

delta_understorey_aboveground_c_pool_2_ann <- make_delta_ua_pool_treatment_abs_effect_4(inDF=understorey_aboveground_c_pool_2_ann, var.col=8)

delta_microbial_c_pool_ann <- make_delta_microbial_pool_treatment_abs_effect(inDF=microbial_c_pool_ann, var.col=7)

delta_mycorrhizal_c_pool_ann <- make_delta_mycorrhizal_pool_treatment_abs_effect(inDF=mycorrhizal_c_pool_ann, var.col=7)

delta_leaflitter_pool_ann <- make_delta_leaflitter_pool_treatment_abs_effect(inDF=leaflitter_pool_ann, var.col=11)

delta_insect_pool_ann <- make_delta_insect_pool_treatment_abs_effect(inDF=insect_pool_ann, var.col=8)

delta_ground_dwelling_insect_pool_ann <- make_delta_ground_dwelling_insect_pool_treatment_abs_effect(inDF=ground_dwelling_insect_pool_ann, var.col=8)


###########################################################################
###                 Step 6: Make summary tables,                        ###
###                         based on normalized results                 ###
###########################################################################
### Generate ring-specific table (ignoring time variable)
### This table is predicted based on average LAI
source("R/normalized/make_table_by_ring_predicted.R")
tables_by_ring_predicted <- make_table_by_ring_predicted()



###########################################################################
###        Step 7: Perform data assimilation,                           ###
###                to estimate parameter/variable uncertainties         ###
###                and NPPmyco                                          ###
###########################################################################
#### A. prepare DA stuffs
### prepare
source("R/prepare_DA.R")

### reproducibility
set.seed(15)

### set up distribution type for parameter space
dist.type <- "uniform"

### prepare the input dataframe for aCO2 and eCO2 treatment
obsDF <- initialize_obs_amb_dataframe()
eco2DF <- initialize_obs_ele_dataframe()

#### B. Estimate prefit allocation parameter uncertainties for ambient CO2 treatment
### step B1:
## this prefitting function explores allocation and turnover rates for several parameters
## to better constrain their initial values and distributions
init.parameters <- run_prefit_program_MCMC(dist.type=dist.type, 
                                           obsDF=obsDF,
                                           eco2DF=eco2DF,
                                           range.option="sd")

########################################################################################
#### C. Estimate remaining parameter uncertainties for ambient CO2 treatment
### step C1: set up 
## initialize parameters 
## based on prefit parameters
source("definitions/initialize_aCO2_parameters.R")
source("definitions/initialize_eCO2_parameters.R")

### Assign chain length for MCMC parameter fitting
chainLength <- 100000

### step C2: fitting
## Ring 2
step.size.aCO2 <- 0.0035 

pChain_aCO2_1 <- MCMC_model_fitting(params = params.aCO2.R2, 
                                    params.lower = params.aCO2.lower.R2,
                                    params.upper = params.aCO2.upper.R2,
                                    obs=obsDF[1,],
                                    chainLength=chainLength,
                                    dist.type=dist.type,
                                    step.size=step.size.aCO2)


generate_most_likely_outcome(inDF=pChain_aCO2_1,
                             obs=obsDF[1,])


plot_parameter_trace_within_parameter_space(params= params.aCO2.R2, 
                                            params.lower = params.aCO2.lower.R2,
                                            params.upper = params.aCO2.upper.R2,
                                            inDF = pChain_aCO2_1,
                                            dist.type=dist.type,
                                            step.size=step.size.aCO2,
                                            chainLength=chainLength,
                                            Trt = "aCO2_1")


# Ring 3
step.size.aCO2 <- 0.0035 

pChain_aCO2_2 <- MCMC_model_fitting(params = params.aCO2.R3, 
                                    params.lower = params.aCO2.lower.R3,
                                    params.upper = params.aCO2.upper.R3,
                                    obs=obsDF[2,],
                                    chainLength=chainLength,
                                    dist.type=dist.type,
                                    step.size=step.size.aCO2)

generate_most_likely_outcome(inDF=pChain_aCO2_2,
                             obs=obsDF[2,])


plot_parameter_trace_within_parameter_space(params= params.aCO2.R3, 
                                            params.lower = params.aCO2.lower.R3,
                                            params.upper = params.aCO2.upper.R3,
                                            inDF = pChain_aCO2_2,
                                            dist.type=dist.type,
                                            step.size=step.size.aCO2,
                                            chainLength=chainLength,
                                            Trt = "aCO2_2")

# Ring 6
step.size.aCO2 <- 0.003 

pChain_aCO2_3 <- MCMC_model_fitting(params = params.aCO2.R6, 
                                    params.lower = params.aCO2.lower.R6,
                                    params.upper = params.aCO2.upper.R6,
                                    obs=obsDF[3,],
                                    chainLength=chainLength,
                                    dist.type=dist.type,
                                    step.size=step.size.aCO2)


generate_most_likely_outcome(inDF=pChain_aCO2_3,
                             obs=obsDF[3,])


plot_parameter_trace_within_parameter_space(params= params.aCO2.R6, 
                                            params.lower = params.aCO2.lower.R6,
                                            params.upper = params.aCO2.upper.R6,
                                            inDF = pChain_aCO2_3,
                                            dist.type=dist.type,
                                            step.size=step.size.aCO2,
                                            chainLength=chainLength,
                                            Trt = "aCO2_3")


### step C3: 
### combine the results, and make some plots
pChain.aCO2 <- rbind(pChain_aCO2_1, pChain_aCO2_2, pChain_aCO2_3)

plot_posterior(inDF = pChain.aCO2, Trt = "aCO2", dist.type = dist.type,
               chainLength = chainLength)

### step C4: 
### predict final output, at mean aCO2
### print out the final predicted results 
### check if the Rhet is OKish?
predict_final_output(pChain = pChain.aCO2, 
                     obs = obsDF[4,],
                     return.option = "Check result")


########################################################################################
#### D: Check what parameters are needed for the eCO2 response
### step D1: 
predict_final_output(pChain = pChain.aCO2, 
                     obs = eco2DF[4,],
                     return.option = "Check result")

### step D2: 
### set up step size for aCO2 and eCO2 
chainLength <- 200000


### step D3:
### fit the model with eCO2 parameter space to get parameter uncertainties
# Ring 1
step.size.eCO2 <- 0.0002 

pChain_eCO2_1 <- MCMC_model_fitting(params = params.eCO2.R1, 
                                    params.lower = params.eCO2.lower.R1,
                                    params.upper = params.eCO2.upper.R1,
                                    obs=eco2DF[1,],
                                    chainLength=chainLength,
                                    dist.type=dist.type,
                                    step.size=step.size.eCO2)

generate_most_likely_outcome(inDF=pChain_eCO2_1,
                             obs=eco2DF[1,])

plot_parameter_trace_within_parameter_space(params= params.eCO2.R1, 
                                            params.lower = params.eCO2.lower.R1,
                                            params.upper = params.eCO2.upper.R1,
                                            inDF = pChain_eCO2_1,
                                            dist.type=dist.type,
                                            step.size=step.size.eCO2,
                                            chainLength=chainLength,
                                            Trt = "eCO2_1")


# ring 4
step.size.eCO2 <- 0.0003 

pChain_eCO2_2 <- MCMC_model_fitting(params = params.eCO2.R4, 
                                    params.lower = params.eCO2.lower.R4,
                                    params.upper = params.eCO2.upper.R4,
                                    obs=eco2DF[2,],
                                    chainLength=chainLength,
                                    dist.type=dist.type,
                                    step.size=step.size.eCO2)

generate_most_likely_outcome(inDF=pChain_eCO2_2,
                             obs=eco2DF[2,])


plot_parameter_trace_within_parameter_space(params= params.eCO2.R4, 
                                            params.lower = params.eCO2.lower.R4,
                                            params.upper = params.eCO2.upper.R4,
                                            inDF = pChain_eCO2_1,
                                            dist.type=dist.type,
                                            step.size=step.size.eCO2,
                                            chainLength=chainLength,
                                            Trt = "eCO2_2")


# ring 5
step.size.eCO2 <- 0.0003 

pChain_eCO2_3 <- MCMC_model_fitting(params = params.eCO2.R5, 
                                    params.lower = params.eCO2.lower.R5,
                                    params.upper = params.eCO2.upper.R5,
                                    obs=eco2DF[3,],
                                    chainLength=chainLength,
                                    dist.type=dist.type,
                                    step.size=step.size.eCO2)

generate_most_likely_outcome(inDF=pChain_eCO2_3,
                             obs=eco2DF[3,])

plot_parameter_trace_within_parameter_space(params= params.eCO2.R5, 
                                            params.lower = params.eCO2.lower.R5,
                                            params.upper = params.eCO2.upper.R5,
                                            inDF = pChain_eCO2_1,
                                            dist.type=dist.type,
                                            step.size=step.size.eCO2,
                                            chainLength=chainLength,
                                            Trt = "eCO2_3")


### step D4: 
### combine the results, and make some plots
pChain.eCO2 <- rbind(pChain_eCO2_1, pChain_eCO2_2, pChain_eCO2_3)

plot_posterior(inDF = pChain.eCO2, Trt = "eCO2", dist.type = dist.type,
               chainLength = chainLength)

### step D5: 
### predict final output, at mean eCO2 values
### print out the final predicted results 
### check if the Rhet is OK?
predict_final_output(pChain = pChain.eCO2, 
                     obs = eco2DF[4,],
                     return.option = "Check result")


#### E: Make aCO2 and eCO2 comparison summaries
### compute a output table to summarize parameters and their uncertainties
make_parameter_summary_table()


########################################################################################
#### F: generate model-data comparison on allocation and turnover coefficients
####    Need to go into function to plot
combine_all_model_output()

#### G: model-data comparison based on traceability framework
####    Need to go into function to plot
#compare_data_model_traceability()


###########################################################################
###                 Step 8: Return to C budget                          ###
###                         prepare summary tables and figures          ###
###########################################################################
### add NPP myco flux to the dataframe
source("R/add_NPPmyco_to_summary_table.R")
tables_by_ring_predicted <- add_NPPmyco_to_summary_table(inDF = tables_by_ring_predicted,
                                                         pChain.aCO2 = pChain.aCO2,
                                                         pChain.eCO2 = pChain.eCO2)

inDF <- tables_by_ring_predicted

### GPP and Rsoil
###    Need to go into function to plot
source("R/normalized/gpp_and_rsoil_normalized_plot.R")
gpp_and_rsoil_normalized_plot(inDF=tables_by_ring_predicted)


source("R/normalized/gpp_and_rsoil_normalized_plot_with_NPPmyco.R")
gpp_and_rsoil_normalized_plot_with_NPPmyco(inDF=tables_by_ring_predicted)


### NEP gaps   
###    Need to go into function to plot
source("R/normalized/nep_normalized_plot.R")
nep_normalized_plot(inDF=tables_by_ring_predicted)


source("R/normalized/nep_normalized_plot_with_NPPmyco.R")
nep_normalized_plot_with_NPPmyco(inDF=tables_by_ring_predicted)

### CUE
source("R/normalized/cue_calculation.R")
cueDF <- cue_calculation(inDF=tables_by_ring_predicted)

### make a summary table for all key values used in the manuscript
#source("R/report_key_values_for_manuscript.R")
#report_key_values_for_manuscript()


### eCO2 effect on fate of carbon
###    Need to go into function to plot
source("R/normalized/make_eCO2_effect_on_GPP_normalized_plot.R")
make_eCO2_effect_on_GPP_normalized_plot(inDF=tables_by_ring_predicted)

source("R/normalized/make_eCO2_effect_on_GPP_normalized_plot_with_NPPmyco.R")
make_eCO2_effect_on_GPP_normalized_plot_with_NPPmyco(inDF=tables_by_ring_predicted)


### all eCO2 effect on a single vertical plot
###    Need to go into function to plot
source("R/normalized/make_statistical_comparison_normalized_plots.R")
make_statistical_comparison_normalized_plots(inDF=tables_by_ring_predicted)


### all supplementary figures
source("R/plot_supplementary_figures.R")






###########################################################################
###                                 The End                             ###
###########################################################################
options(warn=0)