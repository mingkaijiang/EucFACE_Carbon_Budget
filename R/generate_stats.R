generate_stats <- function() {
    #### Call run.R program
    source("run.R")
    
    #### Call the stats function
    source("R/treatment_effect_statistics.R")
    
    #### Work on each variable per time
    ### LAI
    s.lai <- treatment_effect_statistics(inDF=lai_variable, 
                                         var.cond="flux", var.col=3)
    
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
    s.leaflitter_flux <- treatment_effect_statistics(inDF=leaflitter_flux, 
                                                     var.cond="flux", var.col=3)    
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
    
    
}