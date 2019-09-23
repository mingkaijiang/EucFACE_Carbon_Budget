####################################### Define pools, parameters and fluxes

### parameter space

### Ring 2
params.aCO2.R2 <- c(0.55,         # alloc leaf
                    0.15,         # alloc wood
                    0.15,         # alloc froot
                    1.1,          # tau leaf
                    1.4,          # tau froot
                    10.0,         # tau myco
                    0.5,          # tau.bg.lit
                    4.0,          # tau.micr
                    0.14,         # tau.soil
                    25.0,         # C.ag.lit
                    25.0,         # C.bg.lit
                    0.3,          # frac.myco
                    0.3,          # frac.ag
                    0.3,          # frac.bg
                    0.3)          # frac.micr

params.aCO2.lower.R2 <- c(0.3,          # alloc leaf
                          0.1,          # alloc wood
                          0.05,         # alloc froot
                          1.0,          # tau leaf
                          0.8,          # tau froot
                          10.0,         # tau myco
                          0.2,          # tau.bg.lit
                          2.0,          # tau.micr.lit
                          0.05,         # tau.soil.lit
                          0.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.myco
                          0.3,          # frac.ag
                          0.3,          # frac.bg
                          0.3)          # frac.micr

  
params.aCO2.upper.R2 <- c(0.6,          # alloc leaf
                          0.3,          # alloc wood
                          0.2,          # alloc froot
                          1.5,          # tau leaf
                          2.0,          # tau froot
                          80.0,         # tau myco
                          4.0,          # tau.bg.lit
                          6.0,          # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,        # C.bg.lit
                          0.6,          # frac.myco
                          0.6,          # frac.ag
                          0.6,          # frac.bg
                          0.6)          # frac.micr


### Ring 3
params.aCO2.R3 <- c(0.55,         # alloc leaf
                    0.3,          # alloc wood
                    0.15,         # alloc froot
                    1.1,          # tau leaf
                    1.4,          # tau froot
                    10.0,         # tau myco
                    0.5,          # tau.bg.lit
                    4.0,          # tau.micr
                    0.14,         # tau.soil
                    25.0,         # C.ag.lit
                    25.0,         # C.bg.lit
                    0.3,          # frac.myco
                    0.3,          # frac.ag
                    0.3,          # frac.bg
                    0.3)          # frac.micr

params.aCO2.lower.R3 <- c(0.3,          # alloc leaf
                          0.1,          # alloc wood
                          0.05,         # alloc froot
                          1.0,          # tau leaf
                          0.8,          # tau froot
                          10.0,         # tau myco
                          0.2,          # tau.bg.lit
                          2.0,          # tau.micr.lit
                          0.05,          # tau.soil.lit
                          0.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.myco
                          0.3,          # frac.ag
                          0.3,          # frac.bg
                          0.3)          # frac.micr


params.aCO2.upper.R3 <- c(0.6,          # alloc leaf
                          0.3,          # alloc wood
                          0.2,          # alloc froot
                          1.5,          # tau leaf
                          2.0,          # tau froot
                          80.0,         # tau myco
                          4.0,          # tau.bg.lit
                          6.0,          # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,        # C.bg.lit
                          0.6,          # frac.myco
                          0.6,          # frac.ag
                          0.6,          # frac.bg
                          0.6)          # frac.micr


### Ring 6
params.aCO2.R6 <- c(0.55,         # alloc leaf
                    0.15,         # alloc wood
                    0.15,         # alloc froot
                    1.1,          # tau leaf
                    1.4,          # tau froot
                    10.0,         # tau myco
                    0.5,          # tau.bg.lit
                    4.0,          # tau.micr
                    0.14,         # tau.soil
                    25.0,         # C.ag.lit
                    25.0,         # C.bg.lit
                    0.3,          # frac.myco
                    0.3,          # frac.ag
                    0.3,          # frac.bg
                    0.3)           # frac.micr
 
params.aCO2.lower.R6 <- c(0.3,          # alloc leaf
                          0.1,          # alloc wood
                          0.05,         # alloc froot
                          1.0,          # tau leaf
                          0.8,          # tau froot
                          10.0,         # tau myco
                          0.2,          # tau.bg.lit
                          2.0,          # tau.micr.lit
                          0.05,         # tau.soil.lit
                          0.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.myco
                          0.3,          # frac.ag
                          0.3,          # frac.bg
                          0.3)          # frac.micr


params.aCO2.upper.R6 <- c(0.6,          # alloc leaf
                          0.3,          # alloc wood
                          0.2,          # alloc froot
                          1.5,          # tau leaf
                          2.0,          # tau froot
                          80.0,         # tau myco
                          4.0,          # tau.bg.lit
                          6.0,          # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,        # C.bg.lit
                          0.6,          # frac.myco
                          0.6,          # frac.ag
                          0.6,          # frac.bg
                          0.6)          # frac.micr

### set number of parameter variables
no.var <- length(params.aCO2.R2)


