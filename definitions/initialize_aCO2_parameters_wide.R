####################################### Define pools, parameters and fluxes

### parameter space

### Ring 2
params.aCO2.R2 <- c(init.parameters$alloc.leaf[init.parameters$Ring=="2"],
                    init.parameters$alloc.wood[init.parameters$Ring=="2"],
                    init.parameters$alloc.froot[init.parameters$Ring=="2"],
                    init.parameters$alloc.myco[init.parameters$Ring=="2"],
                    init.parameters$tau.leaf[init.parameters$Ring=="2"],
                    init.parameters$tau.froot[init.parameters$Ring=="2"],
                    init.parameters$tau.myco[init.parameters$Ring=="2"],
                    0.5,          # tau.bg.lit
                    2,          # tau.micr
                    0.14,         # tau.soil
                    25.0,         # C.ag.lit
                    25.0,         # C.bg.lit
                    0.6,          # frac.myco
                    0.8,          # frac.ag
                    0.8,          # frac.bg
                    0.6)          # frac.micr

params.aCO2.lower.R2 <- c(init.parameters$alloc.neg.leaf[init.parameters$Ring=="2"],
                          init.parameters$alloc.neg.wood[init.parameters$Ring=="2"],
                          init.parameters$alloc.neg.froot[init.parameters$Ring=="2"],
                          init.parameters$alloc.neg.myco[init.parameters$Ring=="2"],
                          init.parameters$tau.neg.leaf[init.parameters$Ring=="2"],
                          init.parameters$tau.neg.froot[init.parameters$Ring=="2"],
                          init.parameters$tau.neg.myco[init.parameters$Ring=="2"],
                          0.5,          # tau.bg.lit
                          2.0,          # tau.micr.lit
                          0.01,         # tau.soil.lit
                          0.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.myco
                          0.5,          # frac.ag
                          0.5,          # frac.bg
                          0.3)          # frac.micr


params.aCO2.upper.R2 <- c(init.parameters$alloc.pos.leaf[init.parameters$Ring=="2"],
                          init.parameters$alloc.pos.wood[init.parameters$Ring=="2"],
                          init.parameters$alloc.pos.froot[init.parameters$Ring=="2"],
                          init.parameters$alloc.pos.myco[init.parameters$Ring=="2"],
                          init.parameters$tau.pos.leaf[init.parameters$Ring=="2"],
                          init.parameters$tau.pos.froot[init.parameters$Ring=="2"],
                          init.parameters$tau.pos.myco[init.parameters$Ring=="2"],
                          1.0,          # tau.bg.lit
                          6.0,         # tau.micr.lit
                          0.20,         # tau.soil.lit
                          150.0,        # C.ag.lit
                          50.0,          # C.bg.lit
                          0.9,          # frac.myco
                          0.8,          # frac.ag
                          0.8,          # frac.bg
                          0.9)          # frac.micr


### Ring 3
params.aCO2.R3 <- c(init.parameters$alloc.leaf[init.parameters$Ring=="3"],
                    init.parameters$alloc.wood[init.parameters$Ring=="3"],
                    init.parameters$alloc.froot[init.parameters$Ring=="3"],
                    init.parameters$alloc.myco[init.parameters$Ring=="3"],
                    init.parameters$tau.leaf[init.parameters$Ring=="3"],
                    init.parameters$tau.froot[init.parameters$Ring=="3"],
                    init.parameters$tau.myco[init.parameters$Ring=="3"],
                    1.5,          # tau.bg.lit
                    6,            # tau.micr
                    0.15,         # tau.soil
                    91.0,         # C.ag.lit
                    50.0,         # C.bg.lit
                    0.6,          # frac.myco
                    0.8,          # frac.ag
                    0.8,          # frac.bg
                    0.8)          # frac.micr

params.aCO2.lower.R3 <- c(init.parameters$alloc.neg.leaf[init.parameters$Ring=="3"],
                          init.parameters$alloc.neg.wood[init.parameters$Ring=="3"],
                          init.parameters$alloc.neg.froot[init.parameters$Ring=="3"],
                          init.parameters$alloc.neg.myco[init.parameters$Ring=="3"],
                          init.parameters$tau.neg.leaf[init.parameters$Ring=="3"],
                          init.parameters$tau.neg.froot[init.parameters$Ring=="3"],
                          init.parameters$tau.neg.myco[init.parameters$Ring=="3"],
                          0.5,          # tau.bg.lit
                          4.0,          # tau.micr.lit
                          0.1,         # tau.soil.lit
                          0.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.myco
                          0.5,          # frac.ag
                          0.5,          # frac.bg
                          0.3)          # frac.micr


params.aCO2.upper.R3 <- c(init.parameters$alloc.pos.leaf[init.parameters$Ring=="3"],
                          init.parameters$alloc.pos.wood[init.parameters$Ring=="3"],
                          init.parameters$alloc.pos.froot[init.parameters$Ring=="3"],
                          init.parameters$alloc.pos.myco[init.parameters$Ring=="3"],
                          init.parameters$tau.pos.leaf[init.parameters$Ring=="3"],
                          init.parameters$tau.pos.froot[init.parameters$Ring=="3"],
                          init.parameters$tau.pos.myco[init.parameters$Ring=="3"],
                          2.0,          # tau.bg.lit
                          8.0,         # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,          # C.bg.lit
                          0.9,          # frac.myco
                          0.8,          # frac.ag
                          0.8,          # frac.bg
                          0.9)          # frac.micr


### Ring 6
params.aCO2.R6 <- c(init.parameters$alloc.leaf[init.parameters$Ring=="6"],
                    init.parameters$alloc.wood[init.parameters$Ring=="6"],
                    init.parameters$alloc.froot[init.parameters$Ring=="6"],
                    init.parameters$alloc.myco[init.parameters$Ring=="6"],
                    init.parameters$tau.leaf[init.parameters$Ring=="6"],
                    init.parameters$tau.froot[init.parameters$Ring=="6"],
                    init.parameters$tau.myco[init.parameters$Ring=="6"],
                    1.5,          # tau.bg.lit
                    10.0,          # tau.micr
                    0.1,         # tau.soil
                    91.0,         # C.ag.lit
                    100.0,         # C.bg.lit
                    0.3,          # frac.myco
                    0.3,          # frac.ag
                    0.3,          # frac.bg
                    0.3)          # frac.micr

params.aCO2.lower.R6 <- c(init.parameters$alloc.neg.leaf[init.parameters$Ring=="6"],
                          init.parameters$alloc.neg.wood[init.parameters$Ring=="6"],
                          init.parameters$alloc.neg.froot[init.parameters$Ring=="6"],
                          init.parameters$alloc.neg.myco[init.parameters$Ring=="6"],
                          init.parameters$tau.neg.leaf[init.parameters$Ring=="6"],
                          init.parameters$tau.neg.froot[init.parameters$Ring=="6"],
                          init.parameters$tau.neg.myco[init.parameters$Ring=="6"],
                          1.0,          # tau.bg.lit
                          4.5,          # tau.micr.lit
                          0.01,         # tau.soil.lit
                          10.0,         # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.myco
                          0.3,          # frac.ag
                          0.3,          # frac.bg
                          0.3)          # frac.micr


params.aCO2.upper.R6 <- c(init.parameters$alloc.pos.leaf[init.parameters$Ring=="6"],
                          init.parameters$alloc.pos.wood[init.parameters$Ring=="6"],
                          init.parameters$alloc.pos.froot[init.parameters$Ring=="6"],
                          init.parameters$alloc.pos.myco[init.parameters$Ring=="6"],
                          init.parameters$tau.pos.leaf[init.parameters$Ring=="6"],
                          init.parameters$tau.pos.froot[init.parameters$Ring=="6"],
                          init.parameters$tau.pos.myco[init.parameters$Ring=="6"],
                          8.0,          # tau.bg.lit
                          40.0,         # tau.micr.lit
                          0.2,         # tau.soil.lit
                          350.0,        # C.ag.lit
                          150.0,          # C.bg.lit
                          0.9,          # frac.myco
                          0.8,          # frac.ag
                          0.8,          # frac.bg
                          0.9)          # frac.micr

### set number of parameter variables
no.var <- length(params.aCO2.R2)


