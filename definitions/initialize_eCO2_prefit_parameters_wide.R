####################################### Define pools, parameters and fluxes

### parameter space

### R1
prefit.params.eCO2.R1 <- c(0.5,          # alloc leaf
                        0.2,          # alloc wood 
                        0.15,
                        1.25,          # tau leaf
                        1.1,          # tau froot
                        38.0)

prefit.params.eCO2.lower.R1 <- c(0.45,
                              0.15,
                              0.1,
                              1.0,          # tau leaf
                              1.0,          # tau froot
                              0.0)


prefit.params.eCO2.upper.R1 <- c(0.6,
                              0.3,
                              0.3,
                              1.5,          # tau leaf
                              2.0,          # tau froot
                              50.0)        


### R4
prefit.params.eCO2.R4 <- c(0.5,          # alloc leaf
                           0.2,          # alloc wood 
                           0.15,
                           1.1,          # tau leaf
                           1.8,          # tau froot
                           35.0)

prefit.params.eCO2.lower.R4 <- c(0.45,
                                 0.15,
                                 0.1,
                                 1.0,          # tau leaf
                                 1.0,          # tau froot
                                 0.0)


prefit.params.eCO2.upper.R4 <- c(0.6,
                                 0.3,
                                 0.3,
                                 1.5,          # tau leaf
                                 2.0,          # tau froot
                                 50.0)        


### R5
prefit.params.eCO2.R5 <- c(0.4,          # alloc leaf
                           0.16,          # alloc wood 
                           0.1,
                           1.3,          # tau leaf
                           1.8,          # tau froot
                           60.0)

prefit.params.eCO2.lower.R5 <- c(0.35,
                                 0.15,
                                 0.05,
                                 1.0,          # tau leaf
                                 1.0,          # tau froot
                                 50)


prefit.params.eCO2.upper.R5 <- c(0.45,
                                 0.2,
                                 0.1,
                                 1.5,          # tau leaf
                                 2.0,          # tau froot
                                 60.0)        
