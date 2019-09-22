####################################### Define pools, parameters and fluxes

### parameter space


### Ring 1
params.eCO2.R1 <- c(init.parameters$alloc.leaf[init.parameters$Ring=="1"],
                    init.parameters$alloc.wood[init.parameters$Ring=="1"],
                    init.parameters$alloc.froot[init.parameters$Ring=="1"],
                    init.parameters$alloc.myco[init.parameters$Ring=="1"],
                    init.parameters$tau.leaf[init.parameters$Ring=="1"],
                    init.parameters$tau.froot[init.parameters$Ring=="1"],
                    init.parameters$tau.myco[init.parameters$Ring=="1"],
                    1.0,          # tau.bg.lit
                    2,            # tau.micr
                    0.15,         # tau.soil
                    150.0,         # C.ag.lit
                    50.0,         # C.bg.lit
                    0.6,          # frac.myco
                    0.8,          # frac.ag
                    0.8,          # frac.bg
                    0.6)          # frac.micr

params.eCO2.lower.R1 <- c(init.parameters$alloc.neg.leaf[init.parameters$Ring=="1"],
                          init.parameters$alloc.neg.wood[init.parameters$Ring=="1"],
                          init.parameters$alloc.neg.froot[init.parameters$Ring=="1"],
                          init.parameters$alloc.neg.myco[init.parameters$Ring=="1"],
                          init.parameters$tau.neg.leaf[init.parameters$Ring=="1"],
                          init.parameters$tau.neg.froot[init.parameters$Ring=="1"],
                          init.parameters$tau.neg.myco[init.parameters$Ring=="1"],
                          0.5,          # tau.bg.lit
                          2.0,          # tau.micr.lit
                          0.01,         # tau.soil.lit
                          100.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.myco
                          0.5,          # frac.ag
                          0.5,          # frac.bg
                          0.3)          # frac.micr


params.eCO2.upper.R1 <- c(init.parameters$alloc.pos.leaf[init.parameters$Ring=="1"],
                          init.parameters$alloc.pos.wood[init.parameters$Ring=="1"],
                          init.parameters$alloc.pos.froot[init.parameters$Ring=="1"],
                          init.parameters$alloc.pos.myco[init.parameters$Ring=="1"],
                          init.parameters$tau.pos.leaf[init.parameters$Ring=="1"],
                          init.parameters$tau.pos.froot[init.parameters$Ring=="1"],
                          init.parameters$tau.pos.myco[init.parameters$Ring=="1"],
                          1.0,          # tau.bg.lit
                          6.0,         # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,          # C.bg.lit
                          0.9,          # frac.myco
                          0.8,          # frac.ag
                          0.8,          # frac.bg
                          0.9)          # frac.micr


### Ring 4
params.eCO2.R4 <- c(init.parameters$alloc.leaf[init.parameters$Ring=="4"],
                    init.parameters$alloc.wood[init.parameters$Ring=="4"],
                    init.parameters$alloc.froot[init.parameters$Ring=="4"],
                    init.parameters$alloc.myco[init.parameters$Ring=="4"],
                    init.parameters$tau.leaf[init.parameters$Ring=="4"],
                    init.parameters$tau.froot[init.parameters$Ring=="4"],
                    init.parameters$tau.myco[init.parameters$Ring=="4"],
                    4.0,          # tau.bg.lit
                    20.0,            # tau.micr
                    0.18,         # tau.soil
                    150.0,         # C.ag.lit
                    50.0,         # C.bg.lit
                    0.6,          # frac.myco
                    0.8,          # frac.ag
                    0.8,          # frac.bg
                    0.6)          # frac.micr

params.eCO2.lower.R4 <- c(init.parameters$alloc.neg.leaf[init.parameters$Ring=="4"],
                          init.parameters$alloc.neg.wood[init.parameters$Ring=="4"],
                          init.parameters$alloc.neg.froot[init.parameters$Ring=="4"],
                          init.parameters$alloc.neg.myco[init.parameters$Ring=="4"],
                          init.parameters$tau.neg.leaf[init.parameters$Ring=="4"],
                          init.parameters$tau.neg.froot[init.parameters$Ring=="4"],
                          init.parameters$tau.neg.myco[init.parameters$Ring=="4"],
                          2.0,          # tau.bg.lit
                          6.0,          # tau.micr.lit
                          0.1,         # tau.soil.lit
                          100.0,          # C.ag.lit
                          50.0,          # C.bg.lit
                          0.3,          # frac.myco
                          0.5,          # frac.ag
                          0.5,          # frac.bg
                          0.3)          # frac.micr


params.eCO2.upper.R4 <- c(init.parameters$alloc.pos.leaf[init.parameters$Ring=="4"],
                          init.parameters$alloc.pos.wood[init.parameters$Ring=="4"],
                          init.parameters$alloc.pos.froot[init.parameters$Ring=="4"],
                          init.parameters$alloc.pos.myco[init.parameters$Ring=="4"],
                          init.parameters$tau.pos.leaf[init.parameters$Ring=="4"],
                          init.parameters$tau.pos.froot[init.parameters$Ring=="4"],
                          init.parameters$tau.pos.myco[init.parameters$Ring=="4"],
                          6.0,          # tau.bg.lit
                          40.0,         # tau.micr.lit
                          0.3,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,          # C.bg.lit
                          0.9,          # frac.myco
                          0.8,          # frac.ag
                          0.8,          # frac.bg
                          0.9)          # frac.micr


### Ring 5
params.eCO2.R5 <- c(init.parameters$alloc.leaf[init.parameters$Ring=="5"],
                    init.parameters$alloc.wood[init.parameters$Ring=="5"],
                    init.parameters$alloc.froot[init.parameters$Ring=="5"],
                    init.parameters$alloc.myco[init.parameters$Ring=="5"],
                    init.parameters$tau.leaf[init.parameters$Ring=="5"],
                    init.parameters$tau.froot[init.parameters$Ring=="5"],
                    init.parameters$tau.myco[init.parameters$Ring=="5"],
                    1.0,          # tau.bg.lit
                    2,            # tau.micr
                    0.1,         # tau.soil
                    150.0,         # C.ag.lit
                    50.0,         # C.bg.lit
                    0.6,          # frac.myco
                    0.8,          # frac.ag
                    0.8,          # frac.bg
                    0.6)          # frac.micr

params.eCO2.lower.R5 <- c(init.parameters$alloc.neg.leaf[init.parameters$Ring=="5"],
                          init.parameters$alloc.neg.wood[init.parameters$Ring=="5"],
                          init.parameters$alloc.neg.froot[init.parameters$Ring=="5"],
                          init.parameters$alloc.neg.myco[init.parameters$Ring=="5"],
                          init.parameters$tau.neg.leaf[init.parameters$Ring=="5"],
                          init.parameters$tau.neg.froot[init.parameters$Ring=="5"],
                          init.parameters$tau.neg.myco[init.parameters$Ring=="5"],
                          0.5,          # tau.bg.lit
                          2.0,          # tau.micr.lit
                          0.01,         # tau.soil.lit
                          100.0,          # C.ag.lit
                          0.0,          # C.bg.lit
                          0.3,          # frac.myco
                          0.5,          # frac.ag
                          0.5,          # frac.bg
                          0.3)          # frac.micr


params.eCO2.upper.R5 <- c(init.parameters$alloc.pos.leaf[init.parameters$Ring=="5"],
                          init.parameters$alloc.pos.wood[init.parameters$Ring=="5"],
                          init.parameters$alloc.pos.froot[init.parameters$Ring=="5"],
                          init.parameters$alloc.pos.myco[init.parameters$Ring=="5"],
                          init.parameters$tau.pos.leaf[init.parameters$Ring=="5"],
                          init.parameters$tau.pos.froot[init.parameters$Ring=="5"],
                          init.parameters$tau.pos.myco[init.parameters$Ring=="5"],
                          1.0,          # tau.bg.lit
                          6.0,         # tau.micr.lit
                          0.20,         # tau.soil.lit
                          250.0,        # C.ag.lit
                          100.0,          # C.bg.lit
                          0.9,          # frac.myco
                          0.8,          # frac.ag
                          0.8,          # frac.bg
                          0.9)          # frac.micr