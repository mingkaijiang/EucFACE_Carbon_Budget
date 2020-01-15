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
#### 1. to plot figures, we need to go into functions itself for many plotting scripts
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


### soil respiration flux
soil_respiration_flux <- make_soil_respiration_flux()


### isoprene and monoterpenes 
voc_emission_flux <- make_voc_emission_flux()



### leaf C pool
### read in c_fraction defined in constant
### We can either use mean sla value or variable SLA value to calculate leaf C
### sla_option: "mean", or "variable"
leaf_c_pool <- make_leaf_pool(lai_variable, sla_variable, c_fraction,
                              sla_option = "variable")


### Fine root pool (< 2 mm)
fineroot_c_pool <- make_fineroot_pool()

### fineroot c production flux
fineroot_production_flux <- make_fineroot_production_flux()

### Intermediate pool (2 - 3 mm)
### estimate intermediate root pool based on a relationship 
### between f/c ~ fineroot biomass, at two depths
intermediate_root_c_pool <- make_intermediate_root_pool(bkDF=soil_bulk_density_variable)

### intermediate root c production
intermediate_root_production_flux <- make_intermediate_root_production_flux(inDF=intermediate_root_c_pool)


### frass c production flux
frass_production_flux <- make_frass_production_flux()

### herbivore leaf c consumption flux
### extrapolated based on frass weight, leaf area consumed and sla data
herbivory_leaf_consumption_flux <- make_herbivory_leaf_consumption_flux(sla=sla_variable, 
                                                                        frass_flux=frass_production_flux)


### DOC leaching flux
### Can return shallow, deep and all_depths result
### Also assumes leaching = 20 ml m-2 d-1
doc_leaching_flux <- make_doc_leaching_flux(depth="deep")


### Litter fluxes
### This dataframe includes all of twig, bark, seed, leaf.
### reads in c_fraction coefficient from constant
leaflitter_flux <- make_leaflitter_flux(c_fraction)

### Leaf litter pool
leaflitter_pool <- make_leaflitter_pool(c_fraction)

### Insect pool
### based on litter basket data
insect_pool <- make_insect_pool(c_fraction_ins)

### understorey insect is based on suction data
understorey_insect_pool <- make_understorey_insect_pool(c_frac=c_fraction_ins)

### sapwood C and N fraction
sapwood_cn_fraction <- make_sapwood_c_n_fraction()

### wood C pool
wood_c_pool <- make_wood_pool(ring_area,c_fraction)


### Wood C production
wood_production_flux <- make_wood_production_flux(wood_c_pool)


### Wood respiration flux
wood_respiration_flux <- make_wood_respiration_flux()

### understorey SLA
understorey_sla_variable <- make_understorey_sla_variable()


### Understorey aboveground C pool
### reads in c_fraction from constant
### method 1 based on harvesting
### method 2 based on stereo camera
understorey_aboveground_c_pool <- make_understorey_aboveground_c_pool_1(c_fraction_ua)
understorey_aboveground_c_pool_2 <- make_understorey_aboveground_c_pool_2(c_fraction_ua)


### Understorey production flux
understorey_aboveground_production_flux <- make_understorey_aboveground_production_flux(c_fraction_ua)


### Soil microbial C pool
### top 10 cm only - Cat's data
microbial_c_pool <- make_microbial_pool(soil_bulk_density_variable)

### Soil mycorrhizae pool
mycorrhizal_c_pool <- make_mycorrhizal_c_pool(micDF=microbial_c_pool)

### Soil methane C flux
methane_c_flux <- make_methane_flux()


### Herbivory respiration flux
herbivory_respiration_flux <- make_herbivory_respiration_flux(leaf_consumed=herbivory_leaf_consumption_flux,
                                                              frass_prod=frass_production_flux,
                                                              method="diff")


### Coarse root C pool 
coarse_root_c_pool <- make_coarse_root_pool(c_fraction_croot, fr_pool=fineroot_c_pool,
                                        ir_pool=intermediate_root_c_pool) 

#### coarse root C production
coarse_root_production_flux <- make_coarse_root_production_flux(coarse_root_c_pool) 

### Root respiration flux
root_respiration_flux <- make_root_respiration_flux(froot=fineroot_c_pool, 
                                                    iroot=intermediate_root_c_pool,
                                                    croot=coarse_root_c_pool,
                                                    rstem=wood_respiration_flux,
                                                    stem=wood_c_pool)


### Rh C flux
heterotrophic_respiration_flux <- make_heterotrophic_respiration_flux(soil_respiration_flux, 
                                                                      root_respiration_flux)

### Overstorey foliage respiration
overstorey_leaf_respiration_flux <- make_overstorey_leaf_respiration_flux()

### Understorey GPP
understorey_gpp_flux <- make_understorey_GPP_flux()

### Understorey respiration
### assumes either a fixed or a function of temperature
understorey_respiration_flux <- make_understorey_respiration_flux(c_pool=understorey_aboveground_c_pool,
                                                                  c_frac=c_fraction_ua,
                                                                  gpp=understorey_gpp_flux,
                                                                  assumption="maespa_all")



### Delta pools
delta_soil_c_pool <- make_delta_soil_pool_function(inDF=soil_c_pool, var.col=3)

delta_leaf_c_pool <- make_delta_leaf_pool_function(inDF=leaf_c_pool, var.col=3)

delta_wood_c_pool <- make_delta_wood_pool_function(inDF=wood_c_pool, var.col=3)

delta_fineroot_c_pool <- make_delta_fineroot_pool_function(inDF=fineroot_c_pool, var.col=3)

delta_intermediate_root_c_pool <- make_delta_intermediate_root_pool_function(inDF=intermediate_root_c_pool, var.col=3)

delta_coarse_root_c_pool <- make_delta_coarseroot_pool_function(inDF=coarse_root_c_pool, var.col=3)

delta_understorey_aboveground_c_pool_2 <- make_delta_ua_pool_function(inDF=understorey_aboveground_c_pool_2, var.col=3)

delta_microbial_c_pool <- make_delta_microbial_pool_function(inDF=microbial_c_pool, var.col=3)

delta_mycorrhizal_c_pool <- make_delta_mycorrhizal_pool_function(inDF=mycorrhizal_c_pool, var.col=3)

delta_leaflitter_pool <- make_delta_leaflitter_pool_function(inDF=leaflitter_pool, var.col=6)

delta_insect_pool <- make_delta_insect_pool_function(inDF=insect_pool, var.col=3)

delta_understorey_insect_pool <- make_delta_insect_pool_function(inDF=understorey_insect_pool, var.col=3)


###########################################################################
###          Step 4: Make summary tables and figures                    ###
###                  based on data without LAI as a covariate           ###
###                  commented out at the moment                        ###
###########################################################################

### Generate ring-specific table (ignoring time variable)
source("R/un_normalized/make_table_by_ring.R")
tables_by_ring <- make_table_by_ring()


###########################################################################
###    Step 5: Normalize response with LAI as a covariate               ###
###              Note that all fluxes are now annualized                ###
###########################################################################
### overstorey gpp flux
overstorey_gpp_flux_ann <- make_overstorey_gpp_treatment_abs_effect_statistics(inDF=overstorey_gpp_flux, 
                                                                               var.col=3,
                                                                               return.outcome="predicted")

### Understorey GPP
understorey_gpp_flux_ann <- make_understorey_gpp_treatment_abs_effect_statistics(inDF=understorey_gpp_flux, 
                                                                                 var.col=3,
                                                                                 return.outcome="predicted")

### Overstorey Leaf respiration
overstorey_leaf_respiration_flux_ann <- make_overstorey_ra_leaf_treatment_abs_effect_statistics(inDF=overstorey_leaf_respiration_flux, 
                                                                                                var.col=3,
                                                                                                return.outcome="predicted")

### Wood respiration
wood_respiration_flux_ann <- make_ra_wood_treatment_abs_effect_statistics(inDF=wood_respiration_flux, 
                                                                          var.col=5,
                                                                          return.outcome="predicted")


### Root respiration
root_respiration_flux_ann <- make_ra_root_treatment_abs_effect_statistics(inDF=root_respiration_flux, 
                                                                          var.col=5,
                                                                          return.outcome="predicted")
### Understorey respiration
understorey_respiration_flux_ann <- make_ra_und_treatment_abs_effect_statistics(inDF=understorey_respiration_flux, 
                                                                                var.col=5,
                                                                                return.outcome="predicted")

### Frass production
frass_production_flux_ann <- make_frass_treatment_abs_effect_statistics(inDF=frass_production_flux, 
                                                                        var.col=5,
                                                                        return.outcome="predicted")

### herbivory leaf consumption flux
herbivory_leaf_consumption_flux_ann <- make_hb_cons_treatment_abs_effect_statistics(inDF=herbivory_leaf_consumption_flux, 
                                                                                    var.col=5,
                                                                                    return.outcome="predicted")

### Herbivory respiration
herbivory_respiration_flux_ann <- make_r_hb_treatment_abs_effect_statistics(inDF=herbivory_respiration_flux, 
                                                                            var.col=5,
                                                                            return.outcome="predicted")

### soil respiration
soil_respiration_flux_ann <- make_rsoil_treatment_abs_effect_statistics(inDF=soil_respiration_flux, 
                                                                        var.col=5,
                                                                        return.outcome="predicted")

### DOC leaching
doc_leaching_flux_ann <- make_doc_treatment_abs_effect_statistics(inDF=doc_leaching_flux, 
                                                                  var.col=5,
                                                                  return.outcome="predicted")

### CH4 uptake - un-gap filled data
methane_c_flux_ann <- make_ch4_treatment_abs_effect_statistics(inDF=methane_c_flux, 
                                                               var.col=3,
                                                               return.outcome="predicted")

### VOC
voc_c_flux_ann <- make_voc_treatment_abs_effect_statistics(inDF=voc_emission_flux, 
                                                           var.col=3,
                                                           return.outcome="predicted")

### Leaflitter flux
leaflitter_flux_ann <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                        var.col=6,
                                                                        return.outcome="predicted")  

### twig litter flux
twiglitter_flux_ann <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                        var.col=3,
                                                                        return.outcome="predicted")

### bark litter flux
barklitter_flux_ann <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                        var.col=4,
                                                                        return.outcome="predicted") 

### Seed litter flux
seedlitter_flux_ann <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_flux, 
                                                                        var.col=5,
                                                                        return.outcome="predicted")

### Wood production flux
wood_production_flux_ann <- make_wood_prod_treatment_abs_effect_statistics(inDF=wood_production_flux, 
                                                                           var.col=5,
                                                                           return.outcome="predicted") 

### Fineroot production flux
fineroot_production_flux_ann <- make_froot_prod_treatment_abs_effect_statistics(inDF=fineroot_production_flux, 
                                                                                var.col=5,
                                                                                return.outcome="predicted") 


### Coarseroot production flux
coarseroot_production_flux_ann <- make_croot_prod_treatment_abs_effect_statistics(inDF=coarse_root_production_flux, 
                                                                                var.col=5,
                                                                                return.outcome="predicted") 

### Intermediate root production
intermediate_root_production_flux_ann <- make_iroot_prod_treatment_abs_effect_statistics(inDF=intermediate_root_production_flux, 
                                                                                   var.col=5,
                                                                                   return.outcome="predicted")

### Understorey aboveground production
understorey_aboveground_production_flux_ann <- make_und_prod_treatment_abs_effect_statistics(inDF=understorey_aboveground_production_flux, 
                                                                                             var.col=5,
                                                                                             return.outcome="predicted")

### Understorey aboveground litter 
understorey_litter_production_flux_ann <- make_und_prod_treatment_abs_effect_statistics(inDF=understorey_aboveground_production_flux, 
                                                                                        var.col=6,
                                                                                        return.outcome="predicted")

### Rh respiration
heterotrophic_respiration_flux_ann <- make_rh_treatment_abs_effect_statistics(inDF=heterotrophic_respiration_flux, 
                                                                              var.col=5,
                                                                              return.outcome="predicted")

### Soil C
soil_c_pool_ann <- make_soilc_treatment_abs_effect_statistics(inDF=soil_c_pool, 
                                                              var.col=3,
                                                              return.outcome="predicted")

### Leaf C
leaf_c_pool_ann <- make_leafc_treatment_abs_effect_statistics(inDF=leaf_c_pool, 
                                                              var.col=3,
                                                              return.outcome="predicted")

### Wood C pool
wood_c_pool_ann <- make_woodc_treatment_abs_effect_statistics(inDF=wood_c_pool, 
                                                              var.col=3,
                                                              return.outcome="predicted") 

### Fineroot C pool
fineroot_c_pool_ann <- make_frootc_treatment_abs_effect_statistics(inDF=fineroot_c_pool, 
                                                                   var.col=3,
                                                                   return.outcome="predicted")


### Coarseroot C pool
coarseroot_c_pool_ann <- make_crootc_treatment_abs_effect_statistics(inDF=coarse_root_c_pool, 
                                                                   var.col=3,
                                                                   return.outcome="predicted")

### Intermediate root C pool
intermediate_root_c_pool_ann <- make_irootc_treatment_abs_effect_statistics(inDF=intermediate_root_c_pool, 
                                                                      var.col=3,
                                                                      return.outcome="predicted")

### Understorey aboveground C pool
understorey_aboveground_c_pool_2_ann <- make_uac_treatment_abs_effect_statistics(inDF=understorey_aboveground_c_pool_2, 
                                                                                 var.col=3,
                                                                                 return.outcome="predicted")

### Microbial C pool
microbial_c_pool_ann <- make_micc_treatment_abs_effect_statistics(inDF=microbial_c_pool, 
                                                                  var.col=3,
                                                                  return.outcome="predicted")

### Mycorrhizal C pool
mycorrhizal_c_pool_ann <- make_mycc_treatment_abs_effect_statistics(inDF=mycorrhizal_c_pool, 
                                                                    var.col=3,
                                                                    return.outcome="predicted")

### Leaf litter C pool
leaflitter_pool_ann <- make_litc_treatment_abs_effect_statistics(inDF=leaflitter_pool, 
                                                                 var.col=6,
                                                                 return.outcome="predicted")

### Insect pool
insect_pool_ann <- make_insc_treatment_abs_effect_statistics(inDF=insect_pool, 
                                                             var.col=3,
                                                             return.outcome="predicted")

understorey_insect_pool_ann <- make_insc_treatment_abs_effect_statistics(inDF=understorey_insect_pool, 
                                                                         var.col=3,
                                                                         return.outcome="predicted")


### Delta pools
delta_soil_c_pool_ann <- make_delta_soil_pool_treatment_abs_effect(inDF=soil_c_pool_ann, var.col=7)

delta_leaf_c_pool_ann <- make_delta_leaf_pool_treatment_abs_effect(inDF=leaf_c_pool_ann, var.col=8)

delta_wood_c_pool_ann <- make_delta_wood_pool_treatment_abs_effect(inDF=wood_c_pool_ann, var.col=10)

delta_fineroot_c_pool_ann <- make_delta_fineroot_pool_treatment_abs_effect_2(inDF=fineroot_c_pool_ann, var.col=10)

delta_coarseroot_c_pool_ann <- make_delta_coarseroot_pool_treatment_abs_effect(inDF=coarseroot_c_pool_ann, var.col=8)

delta_intermediate_root_c_pool_ann <- make_delta_intermediate_root_pool_treatment_abs_effect(inDF=intermediate_root_c_pool_ann, var.col=10)

delta_understorey_aboveground_c_pool_2_ann <- make_delta_ua_pool_treatment_abs_effect(inDF=understorey_aboveground_c_pool_2_ann, var.col=8)

delta_microbial_c_pool_ann <- make_delta_microbial_pool_treatment_abs_effect(inDF=microbial_c_pool_ann, var.col=7)

delta_mycorrhizal_c_pool_ann <- make_delta_mycorrhizal_pool_treatment_abs_effect(inDF=mycorrhizal_c_pool_ann, var.col=7)

delta_leaflitter_pool_ann <- make_delta_leaflitter_pool_treatment_abs_effect(inDF=leaflitter_pool_ann, var.col=11)

delta_insect_pool_ann <- make_delta_insect_pool_treatment_abs_effect(inDF=insect_pool_ann, var.col=8)

delta_understorey_insect_pool_ann <- make_delta_insect_pool_treatment_abs_effect(inDF=understorey_insect_pool_ann, var.col=8)


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
chainLength <- 200000

### step C2: fitting
## Ring 2
step.size.aCO2 <- 0.0035 
set.seed(15)

pChain_aCO2_1 <- MCMC_model_fitting(params = params.aCO2.R2, 
                                    params.lower = params.aCO2.lower.R2,
                                    params.upper = params.aCO2.upper.R2,
                                    obs=obsDF[1,],
                                    chainLength=chainLength,
                                    dist.type=dist.type,
                                    step.size=step.size.aCO2)


generate_most_likely_outcome(inDF=pChain_aCO2_1,
                             obs=obsDF[1,])


# Ring 3
step.size.aCO2 <- 0.0035 
set.seed(15)

pChain_aCO2_2 <- MCMC_model_fitting(params = params.aCO2.R3, 
                                    params.lower = params.aCO2.lower.R3,
                                    params.upper = params.aCO2.upper.R3,
                                    obs=obsDF[2,],
                                    chainLength=chainLength,
                                    dist.type=dist.type,
                                    step.size=step.size.aCO2)

generate_most_likely_outcome(inDF=pChain_aCO2_2,
                             obs=obsDF[2,])


# Ring 6
step.size.aCO2 <- 0.003 
set.seed(15)

pChain_aCO2_3 <- MCMC_model_fitting(params = params.aCO2.R6, 
                                    params.lower = params.aCO2.lower.R6,
                                    params.upper = params.aCO2.upper.R6,
                                    obs=obsDF[3,],
                                    chainLength=chainLength,
                                    dist.type=dist.type,
                                    step.size=step.size.aCO2)


generate_most_likely_outcome(inDF=pChain_aCO2_3,
                             obs=obsDF[3,])



### step C3: 
### combine the results, and make some plots
pChain.aCO2 <- rbind(pChain_aCO2_1, pChain_aCO2_2, pChain_aCO2_3)

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
chainLength <- 500000


### step D3:
### fit the model with eCO2 parameter space to get parameter uncertainties
# Ring 1
step.size.eCO2 <- 0.0002 
set.seed(15)

pChain_eCO2_1 <- MCMC_model_fitting(params = params.eCO2.R1, 
                                    params.lower = params.eCO2.lower.R1,
                                    params.upper = params.eCO2.upper.R1,
                                    obs=eco2DF[1,],
                                    chainLength=chainLength,
                                    dist.type=dist.type,
                                    step.size=step.size.eCO2)

generate_most_likely_outcome(inDF=pChain_eCO2_1,
                             obs=eco2DF[1,])

# ring 4
step.size.eCO2 <- 0.00028 
set.seed(15)

pChain_eCO2_2 <- MCMC_model_fitting(params = params.eCO2.R4, 
                                    params.lower = params.eCO2.lower.R4,
                                    params.upper = params.eCO2.upper.R4,
                                    obs=eco2DF[2,],
                                    chainLength=chainLength,
                                    dist.type=dist.type,
                                    step.size=step.size.eCO2)

generate_most_likely_outcome(inDF=pChain_eCO2_2,
                             obs=eco2DF[2,])



# ring 5
step.size.eCO2 <- 0.0003 
set.seed(15)

pChain_eCO2_3 <- MCMC_model_fitting(params = params.eCO2.R5, 
                                    params.lower = params.eCO2.lower.R5,
                                    params.upper = params.eCO2.upper.R5,
                                    obs=eco2DF[3,],
                                    chainLength=chainLength,
                                    dist.type=dist.type,
                                    step.size=step.size.eCO2)

generate_most_likely_outcome(inDF=pChain_eCO2_3,
                             obs=eco2DF[3,])


### step D4: 
### combine the results, and make some plots
pChain.eCO2 <- rbind(pChain_eCO2_1, pChain_eCO2_2, pChain_eCO2_3)


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

pChain.aCO2 <- read.csv("DA_output/posterior_parameters_aCO2_uniform_2e+05.csv")
pChain.eCO2 <- read.csv("DA_output/posterior_parameters_eCO2_uniform_5e+05.csv")


########################################################################################
#### F: generate model-data comparison on allocation and turnover coefficients
####    Need to go into function to plot
combine_all_model_output()


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


### NEP gaps   
source("R/normalized/nep_normalized_plot.R")
nep_normalized_plot(inDF=tables_by_ring_predicted)


### CUE - this is data based CUE estimate - slightly different to data assimilation based CUE, because DA assumed simplified structure
source("R/normalized/cue_calculation.R")
cueDF <- cue_calculation(inDF=tables_by_ring_predicted)


### eCO2 effect on fate of carbon
###    Need to go into function to plot
source("R/normalized/make_eCO2_effect_on_GPP_normalized_plot.R")
make_eCO2_effect_on_GPP_normalized_plot(inDF=tables_by_ring_predicted)


### all eCO2 effect on a single vertical plot
###    Need to go into function to plot
source("R/normalized/make_statistical_comparison_normalized_plots.R")
make_statistical_comparison_normalized_plots(inDF=tables_by_ring_predicted)


### all supplementary figures
source("R/plot_supplementary_figures.R")


### Make a Whitaker diagram to show EucFACE in the context of global temperature and precipitation
source("R/global_comparison/make_whitaker_diagram.R")
make_whitaker_diagram()




###########################################################################
###                                 The End                             ###
###########################################################################
options(warn=0)