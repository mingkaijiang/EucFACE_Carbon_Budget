####################################### Define pools, parameters and fluxes

### prefit.parameter space

### R2
prefit.params.aCO2.R2 <- c(0.5,          # alloc leaf
                           0.2,          # alloc wood 
                           0.15,          # alloc froot
                           1.1,          # tau leaf
                           0.9,          # tau froot
                           40.0)         # tau myco)          


prefit.params.aCO2.lower.R2 <- c(0.3,
                                 0.1,
                                 0.05,
                                 1.0,          # tau leaf
                                 0.5,          # tau froot
                                 0.0)          # tau myco


prefit.params.aCO2.upper.R2 <- c(0.6,
                                 0.35,
                                 0.3,
                                 1.5,          # tau leaf
                                 2.0,          # tau froot
                                 50.0)         # tau myco


### set number of parameter variables
no.var <- length(prefit.params.aCO2.R2)


### R3
prefit.params.aCO2.R3 <- c(0.6,          # alloc leaf
                           0.25,          # alloc wood 
                           0.15,          # alloc froot
                           1.1,          # tau leaf
                           0.9,          # tau froot
                           40.0)         # tau myco)          


prefit.params.aCO2.lower.R3 <- c(0.5,
                                 0.2,
                                 0.12,
                                 1.0,          # tau leaf
                                 0.5,          # tau froot
                                 0.0)          # tau myco


prefit.params.aCO2.upper.R3 <- c(0.6,
                                 0.3,
                                 0.2,
                                 1.5,          # tau leaf
                                 2.0,          # tau froot
                                 50.0)         # tau myco


### R6
prefit.params.aCO2.R6 <- c(0.6,          # alloc leaf
                           0.25,          # alloc wood 
                           0.15,          # alloc froot
                           1.0,          # tau leaf
                           0.9,          # tau froot
                           40.0)         # tau myco)          


prefit.params.aCO2.lower.R6 <- c(0.5,
                                 0.2,
                                 0.12,
                                 1.0,          # tau leaf
                                 0.5,          # tau froot
                                 0.0)          # tau myco


prefit.params.aCO2.upper.R6 <- c(0.6,
                                 0.3,
                                 0.2,
                                 1.2,          # tau leaf
                                 2.0,          # tau froot
                                 50.0)         # tau myco

