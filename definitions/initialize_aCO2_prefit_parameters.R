####################################### Define pools, parameters and fluxes

### prefit.parameter space

### R2
prefit.params.aCO2.R2 <- c(0.55,          # alloc leaf
                           0.15,          # alloc wood 
                           0.15,          # alloc root
                           1.1,           # tau leaf
                           1.4,           # tau root
                           10.0)          # tau myco          


prefit.params.aCO2.lower.R2 <- c(0.3,          # alloc leaf
                                 0.1,          # alloc wood
                                 0.05,         # alloc root
                                 0.95,         # tau leaf
                                 0.6,          # tau root
                                 2.0)          # tau myco


prefit.params.aCO2.upper.R2 <- c(0.6,          # alloc leaf
                                 0.3,          # alloc wood
                                 0.3,          # alloc root
                                 1.5,          # tau leaf
                                 2.0,          # tau root
                                 80.0)         # tau myco


### set number of parameter variables
no.var <- length(prefit.params.aCO2.R2)


### R3
prefit.params.aCO2.R3 <- c(0.55,          # alloc leaf
                           0.3,           # alloc wood 
                           0.15,          # alloc root
                           0.97,          # tau leaf
                           1.4,           # tau root
                           10.0)          # tau myco       


prefit.params.aCO2.lower.R3 <- c(0.3,          # alloc leaf
                                 0.1,          # alloc wood
                                 0.05,         # alloc root
                                 0.95,         # tau leaf
                                 0.6,          # tau root
                                 2.0)          # tau myco


prefit.params.aCO2.upper.R3 <- c(0.6,          # alloc leaf
                                 0.3,          # alloc wood
                                 0.3,          # alloc root
                                 1.5,          # tau leaf
                                 2.0,          # tau root
                                 80.0)         # tau myco

### R6
prefit.params.aCO2.R6 <- c(0.55,          # alloc leaf
                           0.2,           # alloc wood 
                           0.15,          # alloc root
                           1.1,           # tau leaf
                           1.4,           # tau root
                           10.0)


prefit.params.aCO2.lower.R6 <- c(0.3,          # alloc leaf
                                 0.1,          # alloc wood
                                 0.05,         # alloc root
                                 0.95,         # tau leaf
                                 0.6,          # tau root
                                 2.0)          # tau myco


prefit.params.aCO2.upper.R6 <- c(0.6,          # alloc leaf
                                 0.3,          # alloc wood
                                 0.3,          # alloc root
                                 1.5,          # tau leaf
                                 2.0,          # tau root
                                 80.0)         # tau myco

