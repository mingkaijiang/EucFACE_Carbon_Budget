####################################### Define pools, parameters and fluxes

### parameter space

### Ring 2
params.aCO2.R2 <- c(init.parameters$alloc.leaf[init.parameters$Ring=="2"],
                    init.parameters$alloc.wood[init.parameters$Ring=="2"],
                    init.parameters$alloc.root[init.parameters$Ring=="2"],
                    init.parameters$tau.leaf[init.parameters$Ring=="2"],
                    init.parameters$tau.root[init.parameters$Ring=="2"],
                    init.parameters$tau.myco[init.parameters$Ring=="2"],
                    2.5,          # tau.bg.lit
                    2.0,          # tau.micr
                    0.15,         # tau.soil
                    150.0,        # C.ag.lit
                    50.0,         # C.bg.lit
                    0.3,          # frac.ag
                    0.3,          # frac.bg
                    0.3)          # frac.micr

params.aCO2.lower.R2 <- c(0.3,          # alloc leaf
                          0.1,          # alloc wood
                          0.05,         # alloc root
                          0.95,         # tau leaf
                          0.6,          # tau root
                          2.0,          # tau myco
                          0.2,          # tau.bg.lit
                          2.0,          # tau.micr
                          0.01,         # tau.soil
                          0.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.ag
                          0.3,          # frac.bg
                          0.3)          # frac.micr


params.aCO2.upper.R2 <- c(0.6,          # alloc leaf
                          0.3,          # alloc wood
                          0.3,          # alloc root
                          1.5,          # tau leaf
                          2.0,          # tau root
                          80.0,         # tau myco
                          4.0,          # tau.bg.lit
                          40.0,         # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,        # C.bg.lit
                          0.9,          # frac.ag
                          0.9,          # frac.bg
                          0.9)          # frac.micr


### Ring 3
params.aCO2.R3 <- c(init.parameters$alloc.leaf[init.parameters$Ring=="3"],
                    init.parameters$alloc.wood[init.parameters$Ring=="3"],
                    init.parameters$alloc.root[init.parameters$Ring=="3"],
                    init.parameters$tau.leaf[init.parameters$Ring=="3"],
                    init.parameters$tau.root[init.parameters$Ring=="3"],
                    init.parameters$tau.myco[init.parameters$Ring=="3"],
                    2.0,          # tau.bg.lit
                    9.0,          # tau.micr
                    0.08,         # tau.soil
                    150.0,        # C.ag.lit
                    50.0,         # C.bg.lit
                    0.3,          # frac.ag
                    0.3,          # frac.bg
                    0.3)          # frac.micr

params.aCO2.lower.R3 <- c(0.3,          # alloc leaf
                          0.1,          # alloc wood
                          0.05,         # alloc root
                          0.95,         # tau leaf
                          0.6,          # tau root
                          2.0,          # tau myco
                          0.2,          # tau.bg.lit
                          2.0,          # tau.micr
                          0.01,         # tau.soil
                          0.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.ag
                          0.3,          # frac.bg
                          0.3)          # frac.micr


params.aCO2.upper.R3 <- c(0.6,          # alloc leaf
                          0.3,          # alloc wood
                          0.3,          # alloc root
                          1.5,          # tau leaf
                          2.0,          # tau root
                          80.0,         # tau myco
                          4.0,          # tau.bg.lit
                          40.0,         # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,        # C.bg.lit
                          0.9,          # frac.ag
                          0.9,          # frac.bg
                          0.9)          # frac.micr


### Ring 6
params.aCO2.R6 <- c(init.parameters$alloc.leaf[init.parameters$Ring=="6"],
                    init.parameters$alloc.wood[init.parameters$Ring=="6"],
                    init.parameters$alloc.root[init.parameters$Ring=="6"],
                    init.parameters$tau.leaf[init.parameters$Ring=="6"],
                    init.parameters$tau.root[init.parameters$Ring=="6"],
                    init.parameters$tau.myco[init.parameters$Ring=="6"],
                    4.0,           # tau.bg.lit
                    10.0,          # tau.micr
                    0.1,           # tau.soil
                    150.0,         # C.ag.lit
                    50.0,          # C.bg.lit
                    0.3,           # frac.ag
                    0.3,           # frac.bg
                    0.3)           # frac.micr
 
params.aCO2.lower.R6 <- c(0.3,          # alloc leaf
                          0.1,          # alloc wood
                          0.05,         # alloc root
                          0.95,         # tau leaf
                          0.6,          # tau root
                          2.0,          # tau myco
                          0.2,          # tau.bg.lit
                          2.0,          # tau.micr
                          0.01,         # tau.soil
                          0.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.ag
                          0.3,          # frac.bg
                          0.3)          # frac.micr


params.aCO2.upper.R6 <- c(0.6,          # alloc leaf
                          0.3,          # alloc wood
                          0.3,          # alloc root
                          1.5,          # tau leaf
                          2.0,          # tau root
                          80.0,         # tau myco
                          4.0,          # tau.bg.lit
                          40.0,          # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,        # C.bg.lit
                          0.9,          # frac.ag
                          0.9,          # frac.bg
                          0.9)          # frac.micr

### set number of parameter variables
no.var <- length(params.aCO2.R2)


