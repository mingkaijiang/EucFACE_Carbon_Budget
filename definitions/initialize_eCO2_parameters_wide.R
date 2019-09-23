####################################### Define pools, parameters and fluxes

### parameter space


### Ring 1
params.eCO2.R1 <- c(0.55,         # alloc leaf
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

params.eCO2.lower.R1 <- c(0.3,          # alloc leaf
                          0.1,          # alloc wood
                          0.05,         # alloc froot
                          1.0,          # tau leaf
                          0.8,          # tau froot
                          10.0,         # tau myco
                          0.5,          # tau.bg.lit
                          2.0,          # tau.micr.lit
                          0.1,         # tau.soil.lit
                          100.0,        # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.myco
                          0.5,          # frac.ag
                          0.5,          # frac.bg
                          0.3)          # frac.micr


params.eCO2.upper.R1 <- c(0.6,          # alloc leaf
                          0.2,          # alloc wood
                          0.2,          # alloc froot
                          1.5,          # tau leaf
                          2.0,          # tau froot
                          80.0,         # tau myco
                          1.0,          # tau.bg.lit
                          6.0,          # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,        # C.bg.lit
                          0.9,          # frac.myco
                          0.8,          # frac.ag
                          0.8,          # frac.bg
                          0.9)          # frac.micr


### Ring 4
params.eCO2.R4 <- c(0.55,         # alloc leaf
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

params.eCO2.lower.R4 <- c(0.3,          # alloc leaf
                          0.1,          # alloc wood
                          0.05,         # alloc froot
                          1.0,          # tau leaf
                          0.8,          # tau froot
                          10.0,         # tau myco
                          2.0,          # tau.bg.lit
                          6.0,          # tau.micr.lit
                          0.1,          # tau.soil.lit
                          100.0,        # C.ag.lit
                          50.0,         # C.bg.lit
                          0.5,          # frac.myco
                          0.5,          # frac.ag
                          0.5,          # frac.bg
                          0.5)          # frac.micr


params.eCO2.upper.R4 <- c(0.6,          # alloc leaf
                          0.2,          # alloc wood
                          0.2,          # alloc froot
                          1.5,          # tau leaf
                          2.0,          # tau froot
                          80.0,         # tau myco
                          6.0,          # tau.bg.lit
                          40.0,         # tau.micr.lit
                          0.3,          # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,        # C.bg.lit
                          0.9,          # frac.myco
                          0.8,          # frac.ag
                          0.8,          # frac.bg
                          0.9)          # frac.micr


### Ring 5
params.eCO2.R5 <- c(0.55,         # alloc leaf
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

params.eCO2.lower.R5 <- c(0.3,          # alloc leaf
                          0.1,          # alloc wood
                          0.05,         # alloc froot
                          1.0,          # tau leaf
                          0.8,          # tau froot
                          10.0,         # tau myco
                          0.5,          # tau.bg.lit
                          2.0,          # tau.micr.lit
                          0.01,         # tau.soil.lit
                          100.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.myco
                          0.5,          # frac.ag
                          0.5,          # frac.bg
                          0.3)          # frac.micr


params.eCO2.upper.R5 <- c(0.6,          # alloc leaf
                          0.2,          # alloc wood
                          0.2,          # alloc froot
                          1.5,          # tau leaf
                          2.0,          # tau froot
                          80.0,         # tau myco
                          1.0,          # tau.bg.lit
                          6.0,         # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,          # C.bg.lit
                          0.9,          # frac.myco
                          0.8,          # frac.ag
                          0.8,          # frac.bg
                          0.9)          # frac.micr


