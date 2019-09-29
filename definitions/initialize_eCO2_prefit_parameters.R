####################################### Define pools, parameters and fluxes

### parameter space

### R1
prefit.params.eCO2.R1 <- c(0.45,          # alloc leaf
                           0.2,           # alloc wood 
                           0.12,
                           1.25,          # tau leaf
                           1.3,           # tau root
                           38.0)

prefit.params.eCO2.lower.R1 <- c(0.3,          # alloc leaf
                                 0.1,          # alloc wood
                                 0.05,         # alloc root
                                 0.95,         # tau leaf
                                 0.8,          # tau root
                                 2.0)          # tau myco


prefit.params.eCO2.upper.R1 <- c(0.6,          # alloc leaf
                                 0.3,          # alloc wood
                                 0.2,          # alloc root
                                 1.5,          # tau leaf
                                 2.0,          # tau root
                                 80.0)         # tau myco


### R4
prefit.params.eCO2.R4 <- c(0.5,          # alloc leaf
                           0.2,          # alloc wood 
                           0.1,
                           1.1,          # tau leaf
                           1.2,          # tau root
                           30.0)

prefit.params.eCO2.lower.R4 <- c(0.3,          # alloc leaf
                                 0.1,          # alloc wood
                                 0.05,         # alloc root
                                 0.95,         # tau leaf
                                 0.8,          # tau root
                                 2.0)


prefit.params.eCO2.upper.R4 <- c(0.6,          # alloc leaf
                                 0.3,          # alloc wood
                                 0.2,          # alloc root
                                 1.5,          # tau leaf
                                 2.0,          # tau root
                                 80.0)        


### R5
prefit.params.eCO2.R5 <- c(0.35,          # alloc leaf
                           0.15,          # alloc wood 
                           0.09,
                           1.1,          # tau leaf
                           1.2,          # tau root
                           80.0)

prefit.params.eCO2.lower.R5 <- c(0.3,          # alloc leaf
                                 0.1,          # alloc wood
                                 0.05,         # alloc root
                                 0.95,         # tau leaf
                                 0.8,          # tau root
                                 2.0)


prefit.params.eCO2.upper.R5 <- c(0.6,          # alloc leaf
                                 0.3,          # alloc wood
                                 0.2,          # alloc root
                                 1.5,          # tau leaf
                                 2.0,          # tau root
                                 80.0)        
