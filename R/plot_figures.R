####### This script calls run.R and plot figures based on that

#### Call run.R program
source("run.R")

#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

#### Source the function that makes treatment effect df
source("R/make_treatment_effect_df.R")
source("R/plot_treatment_effect.R")

#### Plot each variable
### LAI
lai.tr <- make_treatment_effect_df(inDF=lai_variable, v=3, cond=1)
p.lai <- plot_treatment_effect(inDF=lai.tr, y.lab="LAI")

### SLA
sla.tr <- make_treatment_effect_df(inDF=sla_variable, v=3, cond=2)
p.sla <- plot_treatment_effect(inDF=sla.tr, 
                            y.lab=expression(paste("SLA (cm ", g^-1, ")")))

### Understorey SLA - constant over time

### Soil bulk density - constant over time

### Soil C pool
soilc.tr <- make_treatment_effect_df(inDF=soil_c_pool, v=3, cond=1)
p.soilc <- plot_treatment_effect(inDF=soilc.tr, 
                            y.lab=expression(paste("Soil Carbon (g ", m^-2, ")")))

### Leaf C pool
leafc.tr <- make_treatment_effect_df(inDF=leaf_c_pool, v=3, cond=1)
p.leafc <- plot_treatment_effect(inDF=leafc.tr, 
                            y.lab=expression(paste("Leaf Carbon (g ", m^-2, ")")))

### Wood C pool
woodc.tr <- make_treatment_effect_df(inDF=wood_c_pool, v=3, cond=1)
p.woodc <- plot_treatment_effect(inDF=woodc.tr, 
                                 y.lab=expression(paste("Wood Carbon (g ", m^-2, ")")))

### Fineroot C pool
frc.tr <- make_treatment_effect_df(inDF=fineroot_c_pool, v=3, cond=1)
p.frc <- plot_treatment_effect(inDF=frc.tr, 
                                 y.lab=expression(paste("Fineroot Carbon (g ", m^-2, ")")))

### Coarseroot C pool
crc.tr <- make_treatment_effect_df(inDF=coarse_root_c_pool_1, v=3, cond=1)
p.crc <- plot_treatment_effect(inDF=crc.tr, 
                                 y.lab=expression(paste("Coarseroot Carbon (g ", m^-2, ")")))

### Understorey aboveground C pool
uac.tr <- make_treatment_effect_df(inDF=understorey_aboveground_c_pool, v=3, cond=1)
p.uac <- plot_treatment_effect(inDF=uac.tr, 
                                 y.lab=expression(paste("Understorey aboveground Carbon (g ", m^-2, ")")))

### Microbial C pool
micc.tr <- make_treatment_effect_df(inDF=microbial_c_pool, v=3, cond=1)
p.micc <- plot_treatment_effect(inDF=micc.tr, 
                                 y.lab=expression(paste("Microbial Carbon (g ", m^-2, ")")))

### Mycorrhizal C pool
mycc.tr <- make_treatment_effect_df(inDF=mycorrhizal_c_pool, v=3, cond=1)
p.mycc <- plot_treatment_effect(inDF=mycc.tr, 
                                 y.lab=expression(paste("Mycorrhizal Carbon (g ", m^-2, ")")))

### Standing dead C pool

### Overstorey GPP
#ogpp.tr <- make_treatment_effect_df(inDF=overstorey_gpp_flux, v=3, cond=1)
#p.ogpp <- plot_treatment_effect(inDF=ogpp.tr, 
#                                 y.lab=expression(paste("Gross Primary Production (mg ", m^-2, " ", d^-1, ")")))

### Understorey GPP

### Overstorey Leaf respiration

### Root respiration
rroot.tr <- make_treatment_effect_df(inDF=root_respiration_flux, v=5, cond=1)
p.rroot <- plot_treatment_effect(inDF=rroot.tr, 
                                 y.lab=expression(paste("Root respiration (mg ", m^-2, " ", d^-1, ")")))

### Understorey respiration
rund.tr <- make_treatment_effect_df(inDF=understorey_respiration_flux, v=5, cond=1)
p.rund <- plot_treatment_effect(inDF=rund.tr, 
                                y.lab=expression(paste("Understorey respiration (mg ", m^-2, " ", d^-1, ")")))


### Frass production
fras.tr <- make_treatment_effect_df(inDF=frass_production_flux, v=5, cond=2)
p.fras <- plot_treatment_effect(inDF=fras.tr, 
                              y.lab=expression(paste("Frass production (mg ", m^-2, " ", d^-1, ")")))


### herbivory leaf consumption flux
hb.cons.tr <- make_treatment_effect_df(inDF=herbivory_leaf_consumption_flux, v=5, cond=2)
p.hb.cons <- plot_treatment_effect(inDF=hb.cons.tr, 
                              y.lab=expression(paste("Herbivory leaf consumption (mg ", m^-2, " ", d^-1, ")")))


### Herbivory respiration
rhb.tr <- make_treatment_effect_df(inDF=herbivory_respiration_flux, v=5, cond=2)
p.rhb <- plot_treatment_effect(inDF=rhb.tr, 
                              y.lab=expression(paste("Herbivory respiration (mg ", m^-2, " ", d^-1, ")")))

### Lerp production
lerp.prod.tr <- make_treatment_effect_df(inDF=lerp_production_flux, v=5, cond=2)
p.lerp.prod <- plot_treatment_effect(inDF=lerp.prod.tr, 
                               y.lab=expression(paste("Lerp production (mg ", m^-2, " ", d^-1, ")")))

### soil respiration
rsoil.tr <- make_treatment_effect_df(inDF=soil_respiration_flux, v=5, cond=1)
p.rsoil <- plot_treatment_effect(inDF=rsoil.tr, 
                              y.lab=expression(paste("Soil respiration (mg ", m^-2, " ", d^-1, ")")))


### DOC leaching
doc.tr <- make_treatment_effect_df(inDF=doc_leaching_flux, v=5, cond=2)
p.doc <- plot_treatment_effect(inDF=doc.tr, 
                              y.lab=expression(paste("Dissolved organic carbon leaching (mg ", m^-2, " ", d^-1, ")")))


### CH4 uptake
ch4.tr <- make_treatment_effect_df(inDF=methane_c_flux, v=3, cond=1)
p.ch4 <- plot_treatment_effect(inDF=ch4.tr, 
                              y.lab=expression(paste("Methane uptake (mg ", m^-2, " ", d^-1, ")")))


### Leaflitter flux
lit.leaf.tr <- make_treatment_effect_df(inDF=leaflitter_flux, v=6, cond=1)
p.lit.leaf <- plot_treatment_effect(inDF=lit.leaf.tr, 
                              y.lab=expression(paste("Leaf litter (mg ", m^-2, " ", d^-1, ")")))

### twig litter flux
lit.twig.tr <- make_treatment_effect_df(inDF=leaflitter_flux, v=3, cond=1)
p.lit.twig <- plot_treatment_effect(inDF=lit.twig.tr, 
                                    y.lab=expression(paste("Twig litter (mg ", m^-2, " ", d^-1, ")")))

### bark litter flux
lit.bark.tr <- make_treatment_effect_df(inDF=leaflitter_flux, v=4, cond=1)
p.lit.bark <- plot_treatment_effect(inDF=lit.bark.tr, 
                                    y.lab=expression(paste("Bark litter (mg ", m^-2, " ", d^-1, ")")))


### Seed litter flux
lit.seed.tr <- make_treatment_effect_df(inDF=leaflitter_flux, v=3, cond=1)
p.lit.seed <- plot_treatment_effect(inDF=lit.seed.tr, 
                                    y.lab=expression(paste("Seed litter (mg ", m^-2, " ", d^-1, ")")))

### Wood production flux
wood.prod.tr <- make_treatment_effect_df(inDF=wood_production_flux, v=5, cond=1)
p.wood.prod <- plot_treatment_effect(inDF=wood.prod.tr, 
                              y.lab=expression(paste("Wood production (mg ", m^-2, " ", d^-1, ")")))


### Fineroot production flux
froot.prod.tr <- make_treatment_effect_df(inDF=fineroot_production_flux, v=5, cond=1)
p.froot.prod <- plot_treatment_effect(inDF=froot.prod.tr, 
                              y.lab=expression(paste("Fineroot production (mg ", m^-2, " ", d^-1, ")")))


### Coarseroot production
croot.prod.tr <- make_treatment_effect_df(inDF=coarse_root_production_flux_1, v=5, cond=1)
p.croot.prod <- plot_treatment_effect(inDF=croot.prod.tr, 
                              y.lab=expression(paste("Coarseroot production (mg ", m^-2, " ", d^-1, ")")))

### Understorey aboveground production
und.prod.tr <- make_treatment_effect_df(inDF=understorey_aboveground_production_flux, v=5, cond=1)
p.und.prod <- plot_treatment_effect(inDF=und.prod.tr, 
                                      y.lab=expression(paste("Understorey aboveground production (mg ", m^-2, " ", d^-1, ")")))


### Coarseroot production
rh.tr <- make_treatment_effect_df(inDF=heterotrophic_respiration_flux, v=5, cond=1)
p.rh <- plot_treatment_effect(inDF=rh.tr, 
                                      y.lab=expression(paste("Coarseroot production (mg ", m^-2, " ", d^-1, ")")))


### Coarseroot production
croot.prod.tr <- make_treatment_effect_df(inDF=coarse_root_production_flux_1, v=5, cond=1)
p.croot.prod <- plot_treatment_effect(inDF=croot.prod.tr, 
                                      y.lab=expression(paste("Coarseroot production (mg ", m^-2, " ", d^-1, ")")))


### Coarseroot production
croot.prod.tr <- make_treatment_effect_df(inDF=coarse_root_production_flux_1, v=5, cond=1)
p.croot.prod <- plot_treatment_effect(inDF=croot.prod.tr, 
                                      y.lab=expression(paste("Coarseroot production (mg ", m^-2, " ", d^-1, ")")))


#### save into pdf
pdf("output/time_series_plots.pdf", width=10, height=6)
### LAI
plot(p.lai)

### SLA 
plot(p.sla)

### Soil C
plot(p.soilc)

### Leaf C
plot(p.leafc)

### Wood C pool
plot(p.woodc) 

### Fineroot C pool
plot(p.frc) 

### Coarseroot C pool
plot(p.crc) 

### Understorey aboveground C pool
plot(p.uac) 

### Microbial C pool
plot(p.micc) 

### Mycorrhizal C pool
plot(p.mycc) 

### Standing dead C pool

### Overstorey GPP
#plot(p.ogpp) 

### Understorey GPP

### Overstorey Leaf respiration

### Root respiration
plot(p.rroot)

### Understorey respiration
plot(p.rund) 

### Frass production
plot(p.fras) 

### herbivory leaf consumption flux
plot(p.hb.cons) 

### Herbivory respiration
plot(p.rhb) 

### Lerp production
plot(p.lerp.prod) 

### soil respiration
plot(p.rsoil) 

### DOC leaching
plot(p.doc) 

### CH4 uptake
plot(p.ch4) 

### Leaflitter flux
plot(p.lit.leaf) 

### twig litter flux
plot(p.lit.twig) 

### bark litter flux
plot(p.lit.bark) 

### Seed litter flux
plot(p.lit.seed) 

### Wood production flux
plot(p.wood.prod) 

### Fineroot production flux
plot(p.froot.prod) 

### Coarseroot production
plot(p.croot.prod) 

### Understorey aboveground production
plot(p.und.prod)

### Coarseroot production
plot(p.rh) 

### Coarseroot production
plot(p.croot.prod)

### Coarseroot production
plot(p.croot.prod) 

dev.off()