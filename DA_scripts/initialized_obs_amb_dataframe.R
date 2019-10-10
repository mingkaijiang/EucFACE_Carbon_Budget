initialize_obs_amb_dataframe <- function() {
    #### This script reads in csv files from C budget
    #### and make them into the format ready for MCMC modeling
    
    ##################################### read csv delta
    deltaDF1 <- read.csv("output/delta_pool.csv")
    
    deltaDF2 <- as.data.frame(matrix(ncol=12, nrow = 4))
    colnames(deltaDF2) <- c("delta.C.leaf.mean", "delta.C.wood.mean", "delta.C.root.mean",
                            "delta.C.myco.mean", #"delta.C.ag.lit.mean", 
                            "delta.C.micr.mean", "delta.C.soil.mean", 
                            "delta.C.leaf.sd", "delta.C.wood.sd", "delta.C.root.sd",
                            "delta.C.myco.sd", #"delta.C.ag.lit.sd", 
                            "delta.C.micr.sd", "delta.C.soil.sd")
    
    ### Ring 2, means
    deltaDF2[1,"delta.C.leaf.mean"] <- sum(deltaDF1$Ring_2[deltaDF1$term%in%c("Overstorey leaf", 
                                                                              "Understorey above-ground",
                                                                              "Insects")])
    
    deltaDF2[1,"delta.C.wood.mean"] <- sum(deltaDF1$Ring_2[deltaDF1$term%in%c("Overstorey wood", 
                                                                              "Coarse Root")])
    
    deltaDF2[1,"delta.C.root.mean"] <- sum(deltaDF1$Ring_2[deltaDF1$term%in%c("Fine Root",
                                                                              "Intermediate Root")])
    
    deltaDF2[1,"delta.C.myco.mean"] <- sum(deltaDF1$Ring_2[deltaDF1$term=="Mycorrhizae"])
    

    deltaDF2[1,"delta.C.micr.mean"] <- sum(deltaDF1$Ring_2[deltaDF1$term=="Microbial biomass"])
    
    deltaDF2[1,"delta.C.soil.mean"] <- sum(deltaDF1$Ring_2[deltaDF1$term=="Soil C"])
    
    
    ### Ring 3, means
    deltaDF2[2,"delta.C.leaf.mean"] <- sum(deltaDF1$Ring_3[deltaDF1$term%in%c("Overstorey leaf", 
                                                                              "Understorey above-ground",
                                                                              "Insects")])
    
    deltaDF2[2,"delta.C.wood.mean"] <- sum(deltaDF1$Ring_3[deltaDF1$term%in%c("Overstorey wood", 
                                                                              "Coarse Root")])
    
    deltaDF2[2,"delta.C.root.mean"] <- sum(deltaDF1$Ring_3[deltaDF1$term%in%c("Fine Root",
                                                                              "Intermediate Root")])
    
    deltaDF2[2,"delta.C.myco.mean"] <- sum(deltaDF1$Ring_3[deltaDF1$term=="Mycorrhizae"])
    
    deltaDF2[2,"delta.C.micr.mean"] <- sum(deltaDF1$Ring_3[deltaDF1$term=="Microbial biomass"])
    
    deltaDF2[2,"delta.C.soil.mean"] <- sum(deltaDF1$Ring_3[deltaDF1$term=="Soil C"])
    
    
    ### Ring 6, means
    deltaDF2[3,"delta.C.leaf.mean"] <- sum(deltaDF1$Ring_6[deltaDF1$term%in%c("Overstorey leaf", 
                                                                              "Understorey above-ground",
                                                                              "Insects")])
    
    deltaDF2[3,"delta.C.wood.mean"] <- sum(deltaDF1$Ring_6[deltaDF1$term%in%c("Overstorey wood", 
                                                                              "Coarse Root")])
    
    deltaDF2[3,"delta.C.root.mean"] <- sum(deltaDF1$Ring_6[deltaDF1$term%in%c("Fine Root",
                                                                              "Intermediate Root")])
    
    deltaDF2[3,"delta.C.myco.mean"] <- sum(deltaDF1$Ring_6[deltaDF1$term=="Mycorrhizae"])
    
    deltaDF2[3,"delta.C.micr.mean"] <- sum(deltaDF1$Ring_6[deltaDF1$term=="Microbial biomass"])
    
    deltaDF2[3,"delta.C.soil.mean"] <- sum(deltaDF1$Ring_6[deltaDF1$term=="Soil C"])
    
    
    ### aCO2, means
    deltaDF2[4,"delta.C.leaf.mean"] <- sum(deltaDF1$aCO2[deltaDF1$term%in%c("Overstorey leaf", 
                                                                              "Understorey above-ground",
                                                                              "Insects")])
    
    deltaDF2[4,"delta.C.wood.mean"] <- sum(deltaDF1$aCO2[deltaDF1$term%in%c("Overstorey wood", 
                                                                              "Coarse Root")])
    
    deltaDF2[4,"delta.C.root.mean"] <- sum(deltaDF1$aCO2[deltaDF1$term%in%c("Fine Root",
                                                                              "Intermediate Root")])
    
    deltaDF2[4,"delta.C.myco.mean"] <- sum(deltaDF1$aCO2[deltaDF1$term=="Mycorrhizae"])
    
    deltaDF2[4,"delta.C.micr.mean"] <- sum(deltaDF1$aCO2[deltaDF1$term=="Microbial biomass"])
    
    deltaDF2[4,"delta.C.soil.mean"] <- sum(deltaDF1$aCO2[deltaDF1$term=="Soil C"])
    
    
    
    
    ### Ring 2, sd
    deltaDF2[1,"delta.C.leaf.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term%in%c("Overstorey leaf", 
                                                                              "Understorey above-ground",
                                                                              "Insects")])
    
    deltaDF2[1,"delta.C.wood.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term%in%c("Overstorey wood", 
                                                                              "Coarse Root")])
    
    deltaDF2[1,"delta.C.root.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term%in%c("Fine Root",
                                                                              "Intermediate Root")])
    
    deltaDF2[1,"delta.C.myco.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term=="Mycorrhizae"])
    
    deltaDF2[1,"delta.C.micr.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term=="Microbial biomass"])
    
    deltaDF2[1,"delta.C.soil.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term=="Soil C"])
    
    
    ### Ring 3, sd
    deltaDF2[2,"delta.C.leaf.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term%in%c("Overstorey leaf", 
                                                                              "Understorey above-ground",
                                                                              "Insects")])
    
    deltaDF2[2,"delta.C.wood.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term%in%c("Overstorey wood", 
                                                                              "Coarse Root")])
    
    deltaDF2[2,"delta.C.root.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term%in%c("Fine Root",
                                                                              "Intermediate Root")])
    
    deltaDF2[2,"delta.C.myco.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term=="Mycorrhizae"])
    

    deltaDF2[2,"delta.C.micr.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term=="Microbial biomass"])
    
    deltaDF2[2,"delta.C.soil.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term=="Soil C"])
    
    
    ### Ring 6, sd
    deltaDF2[3,"delta.C.leaf.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term%in%c("Overstorey leaf", 
                                                                              "Understorey above-ground",
                                                                              "Insects")])
    
    deltaDF2[3,"delta.C.wood.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term%in%c("Overstorey wood", 
                                                                              "Coarse Root")])
    
    deltaDF2[3,"delta.C.root.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term%in%c("Fine Root",
                                                                              "Intermediate Root")])
    
    deltaDF2[3,"delta.C.myco.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term=="Mycorrhizae"])
    
    deltaDF2[3,"delta.C.micr.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term=="Microbial biomass"])
    
    deltaDF2[3,"delta.C.soil.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term=="Soil C"])
    
    
    
    ### aCO2, sd
    deltaDF2[4,"delta.C.leaf.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term%in%c("Overstorey leaf", 
                                                                             "Understorey above-ground",
                                                                             "Insects")])
    
    deltaDF2[4,"delta.C.wood.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term%in%c("Overstorey wood", 
                                                                             "Coarse Root")])
    
    deltaDF2[4,"delta.C.root.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term%in%c("Fine Root",
                                                                              "Intermediate Root")])
    
    deltaDF2[4,"delta.C.myco.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term=="Mycorrhizae"])

    deltaDF2[4,"delta.C.micr.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term=="Microbial biomass"])
    
    deltaDF2[4,"delta.C.soil.sd"] <- sum(deltaDF1$aCO2_sd[deltaDF1$term=="Soil C"])
    
    
    ##################################### read csv npp
    nppDF1 <- read.csv("output/npp.csv")
    
    nppDF2 <- as.data.frame(matrix(ncol=6, nrow = 4))
    colnames(nppDF2) <- c("NPP.leaf.mean", "NPP.wood.mean", "NPP.root.mean",
                          "NPP.leaf.sd", "NPP.wood.sd", "NPP.froot.sd")
    
    ### Ring 2, means
    nppDF2[1,"NPP.leaf.mean"] <- sum(nppDF1$Ring_2[nppDF1$term%in%c("Leaf NPP", 
                                                                    "Understorey NPP",
                                                                    "Leaf consumption")])
    
    nppDF2[1,"NPP.wood.mean"] <- sum(nppDF1$Ring_2[nppDF1$term%in%c("Stem NPP", "Coarse Root NPP", 
                                                                    "Other NPP")])
    
    nppDF2[1,"NPP.root.mean"] <- sum(nppDF1$Ring_2[nppDF1$term%in%c("Fine Root NPP",
                                                                    "Intermediate Root NPP")])
    
    
    ### Ring 3, means
    nppDF2[2,"NPP.leaf.mean"] <- sum(nppDF1$Ring_3[nppDF1$term%in%c("Leaf NPP", 
                                                                    "Understorey NPP",
                                                                    "Leaf consumption")])
    
    nppDF2[2,"NPP.wood.mean"] <- sum(nppDF1$Ring_3[nppDF1$term%in%c("Stem NPP", "Coarse Root NPP", 
                                                                    "Other NPP")])
    
    nppDF2[2,"NPP.root.mean"] <- sum(nppDF1$Ring_3[nppDF1$term%in%c("Fine Root NPP",
                                                                    "Intermediate Root NPP")])
    
    
    ### Ring 6, means
    nppDF2[3,"NPP.leaf.mean"] <- sum(nppDF1$Ring_6[nppDF1$term%in%c("Leaf NPP", 
                                                                    "Understorey NPP",
                                                                    "Leaf consumption")])
    
    nppDF2[3,"NPP.wood.mean"] <- sum(nppDF1$Ring_6[nppDF1$term%in%c("Stem NPP", "Coarse Root NPP", 
                                                                    "Other NPP")])
    
    nppDF2[3,"NPP.root.mean"] <- sum(nppDF1$Ring_6[nppDF1$term%in%c("Fine Root NPP",
                                                                    "Intermediate Root NPP")])
    
    
    ### aCO2, means
    nppDF2[4,"NPP.leaf.mean"] <- sum(nppDF1$aCO2[nppDF1$term%in%c("Leaf NPP", 
                                                                    "Understorey NPP",
                                                                    "Leaf consumption")])
    
    nppDF2[4,"NPP.wood.mean"] <- sum(nppDF1$aCO2[nppDF1$term%in%c("Stem NPP", "Coarse Root NPP", 
                                                                    "Other NPP")])
    
    nppDF2[4,"NPP.root.mean"] <- sum(nppDF1$aCO2[nppDF1$term%in%c("Fine Root NPP",
                                                                    "Intermediate Root NPP")])
    
    
    
    ### Ring 2, sd
    nppDF2[1,"NPP.leaf.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term%in%c("Leaf NPP", 
                                                                    "Understorey NPP",
                                                                    "Leaf consumption")])
    
    nppDF2[1,"NPP.wood.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term%in%c("Stem NPP", "Coarse Root NPP", 
                                                                    "Other NPP")])
    
    nppDF2[1,"NPP.root.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term%in%c("Fine Root NPP",
                                                                    "Intermediate Root NPP")])
    
    
    ### Ring 3, sd
    nppDF2[2,"NPP.leaf.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term%in%c("Leaf NPP", 
                                                                    "Understorey NPP",
                                                                    "Leaf consumption")])
    
    nppDF2[2,"NPP.wood.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term%in%c("Stem NPP", "Coarse Root NPP", 
                                                                    "Other NPP")])
    
    nppDF2[2,"NPP.root.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term%in%c("Fine Root NPP",
                                                                   "Intermediate Root NPP")])
    
    
    ### Ring 6, sd
    nppDF2[3,"NPP.leaf.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term%in%c("Leaf NPP", 
                                                                    "Understorey NPP",
                                                                    "Leaf consumption")])
    
    nppDF2[3,"NPP.wood.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term%in%c("Stem NPP", "Coarse Root NPP", 
                                                                    "Other NPP")])
    
    nppDF2[3,"NPP.root.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term%in%c("Fine Root NPP",
                                                                   "Intermediate Root NPP")])
    
    
    ### aCO2, sd
    nppDF2[4,"NPP.leaf.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term%in%c("Leaf NPP", 
                                                                   "Understorey NPP",
                                                                   "Leaf consumption")])
    
    nppDF2[4,"NPP.wood.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term%in%c("Stem NPP", "Coarse Root NPP", 
                                                                   "Other NPP")])
    
    nppDF2[4,"NPP.root.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term%in%c("Fine Root NPP",
                                                                   "Intermediate Root NPP")])
    
    
    
    
    ##################################### read csv GPP
    gppDF1 <- read.csv("output/inout.csv")
    
    gppDF2 <- as.data.frame(matrix(ncol=6, nrow = 4))
    colnames(gppDF2) <- c("GPP.mean", "Ra.mean", "Rhet.mean",
                          "GPP.sd", "Ra.sd", "Rhet.sd")
    
    
    ### Ring 2, means
    gppDF2[1,"GPP.mean"] <- sum(gppDF1$Ring_2[gppDF1$term%in%c("GPP overstorey", 
                                                               "GPP understorey")])
    
    gppDF2[1,"Ra.mean"] <- sum(gppDF1$Ring_2[gppDF1$term%in%c("Ra leaf", "Ra stem", 
                                                              "Ra root", "Ra understorey",
                                                              "VOC", "Rherbivore", "Rgrowth")])
    
    gppDF2[1,"Rhet.mean"] <- sum(nppDF1$Ring_2[nppDF1$term=="R hetero"])
    
    
    ### Ring 3, means
    gppDF2[2,"GPP.mean"] <- sum(gppDF1$Ring_3[gppDF1$term%in%c("GPP overstorey", 
                                                               "GPP understorey")])
    
    gppDF2[2,"Ra.mean"] <- sum(gppDF1$Ring_3[gppDF1$term%in%c("Ra leaf", "Ra stem", 
                                                              "Ra root", "Ra understorey",
                                                              "VOC", "Rherbivore", "Rgrowth")])
    
    gppDF2[2,"Rhet.mean"] <- sum(nppDF1$Ring_3[nppDF1$term=="R hetero"])
    
    
    ### Ring 6, means
    gppDF2[3,"GPP.mean"] <- sum(gppDF1$Ring_6[gppDF1$term%in%c("GPP overstorey", 
                                                               "GPP understorey")])
    
    gppDF2[3,"Ra.mean"] <- sum(gppDF1$Ring_6[gppDF1$term%in%c("Ra leaf", "Ra stem", 
                                                              "Ra root", "Ra understorey",
                                                              "VOC", "Rherbivore", "Rgrowth")])
    
    gppDF2[3,"Rhet.mean"] <- sum(nppDF1$Ring_6[nppDF1$term=="R hetero"])
    
    
    ### aCO2, means
    gppDF2[4,"GPP.mean"] <- sum(gppDF1$aCO2[gppDF1$term%in%c("GPP overstorey", 
                                                               "GPP understorey")])
    
    gppDF2[4,"Ra.mean"] <- sum(gppDF1$aCO2[gppDF1$term%in%c("Ra leaf", "Ra stem", 
                                                              "Ra root", "Ra understorey",
                                                              "VOC", "Rherbivore", "Rgrowth")])
    
    gppDF2[4,"Rhet.mean"] <- sum(nppDF1$aCO2[nppDF1$term=="R hetero"])
    
    
    
    ### Ring 2, sd
    gppDF2[1,"GPP.sd"] <- sum(gppDF1$aCO2_sd[gppDF1$term%in%c("GPP overstorey", 
                                                               "GPP understorey")])
    
    gppDF2[1,"Ra.sd"] <- sum(gppDF1$aCO2_sd[gppDF1$term%in%c("Ra leaf", "Ra stem", 
                                                              "Ra root", "Ra understorey",
                                                              "VOC", "Rherbivore", "Rgrowth")])
    
    gppDF2[1,"Rhet.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term=="R hetero"])
    
    
    ### Ring 3, sd
    gppDF2[2,"GPP.sd"] <- sum(gppDF1$aCO2_sd[gppDF1$term%in%c("GPP overstorey", 
                                                               "GPP understorey")])
    
    gppDF2[2,"Ra.sd"] <- sum(gppDF1$aCO2_sd[gppDF1$term%in%c("Ra leaf", "Ra stem", 
                                                              "Ra root", "Ra understorey",
                                                              "VOC", "Rherbivore", "Rgrowth")])
    
    gppDF2[2,"Rhet.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term=="R hetero"])
    
    
    ### Ring 6, sd
    gppDF2[3,"GPP.sd"] <- sum(gppDF1$aCO2_sd[gppDF1$term%in%c("GPP overstorey", 
                                                               "GPP understorey")])
    
    gppDF2[3,"Ra.sd"] <- sum(gppDF1$aCO2_sd[gppDF1$term%in%c("Ra leaf", "Ra stem", 
                                                              "Ra root", "Ra understorey",
                                                              "VOC", "Rherbivore", "Rgrowth")])
    
    gppDF2[3,"Rhet.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term=="R hetero"])
    
    
    ### aCO2, sd
    gppDF2[4,"GPP.sd"] <- sum(gppDF1$aCO2_sd[gppDF1$term%in%c("GPP overstorey", 
                                                              "GPP understorey")])
    
    gppDF2[4,"Ra.sd"] <- sum(gppDF1$aCO2_sd[gppDF1$term%in%c("Ra leaf", "Ra stem", 
                                                             "Ra root", "Ra understorey",
                                                             "VOC", "Rherbivore", "Rgrowth")])
    
    gppDF2[4,"Rhet.sd"] <- sum(nppDF1$aCO2_sd[nppDF1$term=="R hetero"])
    
    
    ##################################### read csv pool
    poolDF1 <- read.csv("output/pool.csv")
    
    poolDF2 <- as.data.frame(matrix(ncol=12, nrow = 4))
    colnames(poolDF2) <- c("C.leaf.mean", "C.wood.mean", "C.root.mean",
                           "C.myco.mean", 
                           "C.micr.mean", "C.soil.mean", 
                           "C.leaf.sd", "C.wood.sd", "C.root.sd",
                           "C.myco.sd", 
                           "C.micr.sd", "C.soil.sd")
    
    ### Ring 2, means
    poolDF2[1,"C.leaf.mean"] <- sum(poolDF1$Ring_2[poolDF1$term%in%c("Overstorey leaf", 
                                                                     "Understorey above-ground",
                                                                     "Insects")])
    
    poolDF2[1,"C.wood.mean"] <- sum(poolDF1$Ring_2[poolDF1$term%in%c("Overstorey wood", 
                                                                     "Coarse Root")])
    
    poolDF2[1,"C.root.mean"] <- sum(poolDF1$Ring_2[poolDF1$term%in%c("Fine Root",
                                                                     "Intermediate Root")])
    
    poolDF2[1,"C.myco.mean"] <- sum(poolDF1$Ring_2[poolDF1$term=="Mycorrhizae"])
    
    poolDF2[1,"C.micr.mean"] <- sum(poolDF1$Ring_2[poolDF1$term=="Microbial biomass"])
    
    poolDF2[1,"C.soil.mean"] <- sum(poolDF1$Ring_2[poolDF1$term=="Soil C"])
    
    
    ### Ring 3, means
    poolDF2[2,"C.leaf.mean"] <- sum(poolDF1$Ring_3[poolDF1$term%in%c("Overstorey leaf", 
                                                                     "Understorey above-ground",
                                                                     "Insects")])
    
    poolDF2[2,"C.wood.mean"] <- sum(poolDF1$Ring_3[poolDF1$term%in%c("Overstorey wood", 
                                                                     "Coarse Root")])
    
    poolDF2[2,"C.root.mean"] <- sum(poolDF1$Ring_3[poolDF1$term%in%c("Fine Root",
                                                                     "Intermediate Root")])
    
    poolDF2[2,"C.myco.mean"] <- sum(poolDF1$Ring_3[poolDF1$term=="Mycorrhizae"])
    
    poolDF2[2,"C.micr.mean"] <- sum(poolDF1$Ring_3[poolDF1$term=="Microbial biomass"])
    
    poolDF2[2,"C.soil.mean"] <- sum(poolDF1$Ring_3[poolDF1$term=="Soil C"])
    
    
    ### Ring 6, means
    poolDF2[3,"C.leaf.mean"] <- sum(poolDF1$Ring_6[poolDF1$term%in%c("Overstorey leaf", 
                                                                     "Understorey above-ground",
                                                                     "Insects")])
    
    poolDF2[3,"C.wood.mean"] <- sum(poolDF1$Ring_6[poolDF1$term%in%c("Overstorey wood", 
                                                                     "Coarse Root")])
    
    poolDF2[3,"C.root.mean"] <- sum(poolDF1$Ring_6[poolDF1$term%in%c("Fine Root",
                                                                     "Intermediate Root")])
    
    poolDF2[3,"C.myco.mean"] <- sum(poolDF1$Ring_6[poolDF1$term=="Mycorrhizae"])
    
    poolDF2[3,"C.micr.mean"] <- sum(poolDF1$Ring_6[poolDF1$term=="Microbial biomass"])
    
    poolDF2[3,"C.soil.mean"] <- sum(poolDF1$Ring_6[poolDF1$term=="Soil C"])
    
    
    ### aCO2, means
    poolDF2[4,"C.leaf.mean"] <- sum(poolDF1$aCO2[poolDF1$term%in%c("Overstorey leaf", 
                                                                     "Understorey above-ground",
                                                                     "Insects")])
    
    poolDF2[4,"C.wood.mean"] <- sum(poolDF1$aCO2[poolDF1$term%in%c("Overstorey wood", 
                                                                     "Coarse Root")])
    
    poolDF2[4,"C.root.mean"] <- sum(poolDF1$aCO2[poolDF1$term%in%c("Fine Root",
                                                                     "Intermediate Root")])
    
    poolDF2[4,"C.myco.mean"] <- sum(poolDF1$aCO2[poolDF1$term=="Mycorrhizae"])
    
    poolDF2[4,"C.micr.mean"] <- sum(poolDF1$aCO2[poolDF1$term=="Microbial biomass"])
    
    poolDF2[4,"C.soil.mean"] <- sum(poolDF1$aCO2[poolDF1$term=="Soil C"])
    
    
    
    ### Ring 2, sd
    poolDF2[1,"C.leaf.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term%in%c("Overstorey leaf", 
                                                                    "Understorey above-ground",
                                                                    "Insects")])
    
    poolDF2[1,"C.wood.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term%in%c("Overstorey wood", 
                                                                    "Coarse Root")])
    
    poolDF2[1,"C.root.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term%in%c("Fine Root",
                                                                   "Intermediate Root")])
    
    poolDF2[1,"C.myco.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Mycorrhizae"])
    
    poolDF2[1,"C.micr.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Microbial biomass"])
    
    poolDF2[1,"C.soil.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Soil C"])
    
    
    ### Ring 3, sd
    poolDF2[2,"C.leaf.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term%in%c("Overstorey leaf", 
                                                                    "Understorey above-ground",
                                                                    "Insects")])
    
    poolDF2[2,"C.wood.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term%in%c("Overstorey wood", 
                                                                    "Coarse Root")])
    
    poolDF2[2,"C.root.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term%in%c("Fine Root",
                                                                    "Intermediate Root")])
    
    poolDF2[2,"C.myco.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Mycorrhizae"])
    
    poolDF2[2,"C.micr.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Microbial biomass"])
    
    poolDF2[2,"C.soil.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Soil C"])
    
    
    ### Ring 6, sd
    poolDF2[3,"C.leaf.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term%in%c("Overstorey leaf", 
                                                                    "Understorey above-ground",
                                                                    "Insects")])
    
    poolDF2[3,"C.wood.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term%in%c("Overstorey wood", 
                                                                    "Coarse Root")])
    
    poolDF2[3,"C.root.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term%in%c("Fine Root",
                                                                    "Intermediate Root")])
    
    poolDF2[3,"C.myco.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Mycorrhizae"])
    
    poolDF2[3,"C.micr.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Microbial biomass"])
    
    poolDF2[3,"C.soil.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Soil C"])
    
    
    ### aCO2, sd
    poolDF2[4,"C.leaf.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term%in%c("Overstorey leaf", 
                                                                    "Understorey above-ground",
                                                                    "Insects")])
    
    poolDF2[4,"C.wood.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term%in%c("Overstorey wood", 
                                                                    "Coarse Root")])
    
    poolDF2[4,"C.root.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term%in%c("Fine Root",
                                                                    "Intermediate Root")])
    
    poolDF2[4,"C.myco.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Mycorrhizae"])
    
    #poolDF2[4,"C.ag.lit.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Litter"])
    
    poolDF2[4,"C.micr.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Microbial biomass"])
    
    poolDF2[4,"C.soil.sd"] <- sum(poolDF1$aCO2_sd[poolDF1$term=="Soil C"])
    
    
    ### turnover rate for leaf litter pool
    poolDF2$tau.ag.lit.mean <- 365 * c(0.0059, 0.0055, 0.0072, mean(c(0.0059, 0.0055, 0.0072)))
    poolDF2$tau.ag.lit.sd <- 365 * rep(sd(c(0.0059, 0.0055, 0.0072)), 4)
    
    ### set outDF
    out <- data.frame(gppDF2, nppDF2, deltaDF2, poolDF2)
    

    return(out)
}