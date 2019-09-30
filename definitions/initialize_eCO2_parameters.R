####################################### Define pools, parameters and fluxes

### parameter space


### Ring 1
params.eCO2.R1 <- c(init.parameters$alloc.leaf[init.parameters$Ring=="1"],
                    init.parameters$alloc.wood[init.parameters$Ring=="1"],
                    init.parameters$alloc.root[init.parameters$Ring=="1"],
                    init.parameters$tau.leaf[init.parameters$Ring=="1"],
                    init.parameters$tau.root[init.parameters$Ring=="1"],
                    init.parameters$tau.myco[init.parameters$Ring=="1"],
                    1.0,          # tau.bg.lit
                    2,            # tau.micr
                    0.2,         # tau.soil
                    150.0,        # C.ag.lit
                    150.0,         # C.bg.lit
                    0.8,          # frac.ag
                    0.8,          # frac.bg
                    0.6)          # frac.micr

params.eCO2.lower.R1 <- c(0.3,          # alloc leaf
                          0.1,          # alloc wood
                          0.05,         # alloc root
                          0.95,         # tau leaf
                          0.3,          # tau root
                          2.0,          # tau myco
                          0.2,          # tau.bg.lit
                          2.0,          # tau.micr
                          0.01,         # tau.soil
                          0.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.ag
                          0.3,          # frac.bg
                          0.3)          # frac.micr


params.eCO2.upper.R1 <- c(0.6,          # alloc leaf
                          0.3,          # alloc wood
                          0.3,          # alloc root
                          1.5,          # tau leaf
                          2.0,          # tau root
                          80.0,         # tau myco
                          4.0,          # tau.bg.lit
                          40.0,         # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          250.0,        # C.bg.lit
                          0.9,          # frac.ag
                          0.9,          # frac.bg
                          0.9)          # frac.micr


### Ring 4
params.eCO2.R4 <- c(init.parameters$alloc.leaf[init.parameters$Ring=="4"],
                    init.parameters$alloc.wood[init.parameters$Ring=="4"],
                    init.parameters$alloc.root[init.parameters$Ring=="4"],
                    init.parameters$tau.leaf[init.parameters$Ring=="4"],
                    init.parameters$tau.root[init.parameters$Ring=="4"],
                    init.parameters$tau.myco[init.parameters$Ring=="4"],
                    2.0,          # tau.bg.lit
                    20.0,         # tau.micr
                    0.12,          # tau.soil
                    150.0,        # C.ag.lit
                    150.0,         # C.bg.lit
                    0.3,          # frac.ag
                    0.3,          # frac.bg
                    0.3)          # frac.micr

params.eCO2.lower.R4 <- c(0.3,          # alloc leaf
                          0.1,          # alloc wood
                          0.05,         # alloc root
                          0.95,         # tau leaf
                          0.3,          # tau root
                          2.0,          # tau myco
                          0.2,          # tau.bg.lit
                          2.0,          # tau.micr
                          0.01,         # tau.soil
                          0.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.ag
                          0.3,          # frac.bg
                          0.3)          # frac.micr


params.eCO2.upper.R4 <- c(0.6,          # alloc leaf
                          0.3,          # alloc wood
                          0.3,          # alloc root
                          1.5,          # tau leaf
                          2.0,          # tau root
                          80.0,         # tau myco
                          4.0,          # tau.bg.lit
                          40.0,         # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          250.0,        # C.bg.lit
                          0.9,          # frac.ag
                          0.9,          # frac.bg
                          0.9)          # frac.micr


### Ring 5
params.eCO2.R5 <- c(init.parameters$alloc.leaf[init.parameters$Ring=="5"],
                    init.parameters$alloc.wood[init.parameters$Ring=="5"],
                    init.parameters$alloc.root[init.parameters$Ring=="5"],
                    init.parameters$tau.leaf[init.parameters$Ring=="5"],
                    init.parameters$tau.root[init.parameters$Ring=="5"],
                    init.parameters$tau.myco[init.parameters$Ring=="5"],
                    1.0,          # tau.bg.lit
                    2,            # tau.micr
                    0.08,          # tau.soil
                    150.0,        # C.ag.lit
                    150.0,         # C.bg.lit
                    0.3,          # frac.ag
                    0.3,          # frac.bg
                    0.3)          # frac.micr

params.eCO2.lower.R5 <- c(0.3,          # alloc leaf
                          0.1,          # alloc wood
                          0.05,         # alloc root
                          0.95,         # tau leaf
                          0.3,          # tau root
                          2.0,          # tau myco
                          0.2,          # tau.bg.lit
                          2.0,          # tau.micr
                          0.01,         # tau.soil
                          0.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.ag
                          0.3,          # frac.bg
                          0.3)          # frac.micr


params.eCO2.upper.R5 <- c(0.6,          # alloc leaf
                          0.3,          # alloc wood
                          0.3,          # alloc root
                          1.5,          # tau leaf
                          2.0,          # tau root
                          80.0,         # tau myco
                          4.0,          # tau.bg.lit
                          40.0,         # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          250.0,        # C.bg.lit
                          0.9,          # frac.ag
                          0.9,          # frac.bg
                          0.9)          # frac.micr


