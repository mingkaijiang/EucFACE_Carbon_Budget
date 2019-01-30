make_eCO2_effect_on_GPP_plot_with_covariate_predicted_alternative_plot_color <- function() {
    
    ######### Plot abs, no interactions, and change in pools
    ### read in the csv file to plot the treatment effect and confidence interval
    myDF <- read.csv("R_other/treatment_statistics_abs_no_interaction_with_covariate.csv")
    
    ### compute standard error
    myDF$se1 <- with(myDF, abs((conf_high - conf_low)/3.92))
    myDF$se2 <- with(myDF, abs((effect_size)/abs(qnorm(1 - (Trt_Pr)/2))))
    #myDF$sd <- with(myDF, se2 * sqrt((Trt_Df+1) * Trt_Df.res * (Date_Df + 1)))
    myDF$sd <- with(myDF, se2 * sqrt((Date_Df + 1)))
    
    #### assign color scheme according to treatment p-value
    myDF$trt_sig[myDF$Trt_Pr > 0.1] <- "non-sig"
    myDF$trt_sig[myDF$Trt_Pr <= 0.1] <- "sig"
    
    myDF$date_sig[myDF$Date_Pr > 0.05] <- "non-sig"
    myDF$date_sig[myDF$Date_Pr <= 0.05] <- "sig"
    
    myDF <- myDF[complete.cases(myDF$effect_size),]
    
    #### exclude all pools
    myDF$Category <- c(rep("pool", 11), # 1 - 11
                       rep("resp", 2), # 12 - 13
                       rep("prod", 1), # 14
                       rep("resp", 2), # 15 - 16
                       rep("prod", 1), # 17
                       rep("resp", 2), # 18 - 19
                       rep("prod", 1), # 20
                       rep("resp", 1), # 21
                       rep("prod", 8), # 22 - 29
                       rep("resp", 1), # 30
                       rep("prod",1),  # 31
                       rep("gpp", 1),  # 32
                       rep("resp", 2), # 33 - 34
                       #rep("prod",1),  # 35
                       rep("gpp", 1),  # 35
                       rep("change_in_pool", 11))   # 36 - 46
    
    ### Drop redundant pools and fluxes
    myDF <- subset(myDF, Variable != c("delta_understorey_c_2"))
    myDF <- subset(myDF, Variable != c("understorey_lit"))
    
    ### Drop CWD - confidence interval too large
    myDF <- subset(myDF, Variable != c("delta_cwd_c"))
    
    ### Subset GPP, NPP, change in pools, and out fluxes
    plotDF1 <- subset(myDF, Variable %in% c("over_gpp", "understorey_gpp", "ch4",
                                            "leaf_prod", "twig_prod", "bark_prod",
                                            "seed_prod", "wood_prod", "fineroot_prod",
                                            "coarseroot_prod", "understorey_prod", "herb_consump", #"mycorrhizal_prod",
                                            "over_leaf_respiration", "wood_respiration", "root_respiration",
                                            "understorey_respiration", "hetero_respiration","doc", "voc",
                                            "delta_leaf_c", "delta_wood_c", "delta_fineroot_c", 
                                            "delta_coarseroot_c", "delta_understorey_c", 
                                            "delta_litter_c","delta_soil_c","delta_microbial_c",
                                            "delta_mycorrhizal_c", "delta_litter_c", "delta_insect_c"))
    
    ### Add plot category
    plotDF1$plot.cat[plotDF1$Category=="gpp"] <- "Influxes"
    plotDF1$plot.cat[plotDF1$Category=="change_in_pool"] <- "Change_in_pools"
    plotDF1$plot.cat[plotDF1$Category=="resp"] <- "Outfluxes"
    plotDF1$plot.cat[plotDF1$Category=="prod"] <- "NPP"
    plotDF1$plot.cat[plotDF1$Variable=="ch4"] <- "Influxes"
    
    #plotDF1 <- plotDF1[,c("Variable", "effect_size", "conf_low", "conf_high", "plot.cat")]
    plotDF1 <- plotDF1[,c("Variable", "effect_size", "conf_low", "conf_high", "plot.cat", "sd", "Date_Df")]
    
    plotDF1$plot.cat[plotDF1$Variable%in%c("root_respiration", "understorey_respiration",
                                           "over_leaf_respiration", "wood_respiration", "voc")] <- "Ra"
    plotDF1$plot.cat[plotDF1$Variable=="herb_consump"] <- "NPP"
    
    
    ### Calculate totals of each plot.cat
    plotDF2 <- summaryBy(effect_size+conf_low+conf_high~plot.cat, data=plotDF1, FUN=sum, keep.names=T, na.rm=T)
    names(plotDF2)[1] <- "Variable"
    plotDF2[6,"Variable"] <- "total_outflux"
    
    ### Total of Ra and Rh and the conf. interval
    plotDF2[6,"effect_size"] <- plotDF2$effect_size[plotDF2$Variable=="Outfluxes"]+
        plotDF2$effect_size[plotDF2$Variable=="Ra"]
    
    plotDF2[6,"conf_low"] <- plotDF2$conf_low[plotDF2$Variable=="Outfluxes"]+
        plotDF2$conf_low[plotDF2$Variable=="Ra"]
    
    plotDF2[6,"conf_high"] <- plotDF2$conf_high[plotDF2$Variable=="Outfluxes"]+
        plotDF2$conf_high[plotDF2$Variable=="Ra"]
    
    
    ### revise confidence interval for each variables
    ### set number of bootstrapping
    n.b <- 1000
    
    ### perform bootstrapping for each category sum (i.e. cat == MAESPA and NPP+Ra)
    set.seed(1234)
    
    ## change in pools
    bDF1 <- data.frame(c(1:n.b), NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "delta_soil_c", "delta_leaf_c", "delta_wood_c", "delta_fineroot_c", "delta_coarseroot_c", 
                        "delta_litter_c", "delta_microbial_c", "delta_mycorrhizal_c", "delta_insect_c")
    bDF1$delta_soil_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_soil_c"],
                               sd=plotDF1$sd[plotDF1$Variable=="delta_soil_c"])
    bDF1$delta_leaf_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_leaf_c"],
                               sd=plotDF1$sd[plotDF1$Variable=="delta_leaf_c"])
    bDF1$delta_wood_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_wood_c"],
                               sd=plotDF1$sd[plotDF1$Variable=="delta_wood_c"])
    bDF1$delta_fineroot_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_fineroot_c"],
                                   sd=plotDF1$sd[plotDF1$Variable=="delta_fineroot_c"])
    bDF1$delta_coarseroot_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_coarseroot_c"],
                                     sd=plotDF1$sd[plotDF1$Variable=="delta_coarseroot_c"])
    bDF1$delta_litter_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_litter_c"],
                                 sd=plotDF1$sd[plotDF1$Variable=="delta_litter_c"])
    bDF1$delta_microbial_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_microbial_c"],
                                    sd=plotDF1$sd[plotDF1$Variable=="delta_microbial_c"])
    bDF1$delta_mycorrhizal_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_mycorrhizal_c"],
                                      sd=plotDF1$sd[plotDF1$Variable=="delta_mycorrhizal_c"])
    bDF1$delta_insect_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_insect_c"],
                                 sd=plotDF1$sd[plotDF1$Variable=="delta_insect_c"])
    bDF1$sum <- with(bDF1, delta_soil_c + delta_leaf_c + delta_wood_c + delta_fineroot_c + delta_coarseroot_c + 
                         delta_litter_c + delta_microbial_c + delta_mycorrhizal_c + delta_insect_c)
    
    #plotDF2$conf_low[plotDF2$Variable=="Change_in_pools"] <- plotDF2$effect_size[plotDF2$Variable=="Change_in_pools"] -
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    #plotDF2$conf_high[plotDF2$Variable=="Change_in_pools"] <- plotDF2$effect_size[plotDF2$Variable=="Change_in_pools"] +
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    
    plotDF2$conf_low[plotDF2$Variable=="Change_in_pools"] <- plotDF2$effect_size[plotDF2$Variable=="Change_in_pools"] -
        sd(bDF1$sum) 
    plotDF2$conf_high[plotDF2$Variable=="Change_in_pools"] <- plotDF2$effect_size[plotDF2$Variable=="Change_in_pools"] +
        sd(bDF1$sum) 
    
    ## Influxes
    bDF1 <- data.frame(c(1:n.b), NA, NA,NA)
    colnames(bDF1) <- c("bootID", "ch4", "over_gpp", "understorey_gpp")
    bDF1$ch4 <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="ch4"],
                      sd=plotDF1$sd[plotDF1$Variable=="ch4"])
    bDF1$over_gpp <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="over_gpp"],
                           sd=plotDF1$sd[plotDF1$Variable=="over_gpp"])
    bDF1$understorey_gpp <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="understorey_gpp"],
                                  sd=plotDF1$sd[plotDF1$Variable=="understorey_gpp"])
    bDF1$sum <- with(bDF1, ch4 + over_gpp + understorey_gpp)
    
    #plotDF2$conf_low[plotDF2$Variable=="Influxes"] <- plotDF2$effect_size[plotDF2$Variable=="Influxes"] -
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    #plotDF2$conf_high[plotDF2$Variable=="Influxes"] <- plotDF2$effect_size[plotDF2$Variable=="Influxes"] +
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    
    plotDF2$conf_low[plotDF2$Variable=="Influxes"] <- plotDF2$effect_size[plotDF2$Variable=="Influxes"] -
        sd(bDF1$sum) 
    plotDF2$conf_high[plotDF2$Variable=="Influxes"] <- plotDF2$effect_size[plotDF2$Variable=="Influxes"] +
        sd(bDF1$sum) 
    
    ## NPP
    bDF1 <- data.frame(c(1:n.b), NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "herb_consump", "leaf_prod", "twig_prod", "bark_prod", "seed_prod",
                        "wood_prod", "fineroot_prod", "coarseroot_prod", "understorey_prod")
    bDF1$herb_consump <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="herb_consump"],
                               sd=plotDF1$sd[plotDF1$Variable=="herb_consump"])
    bDF1$leaf_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="leaf_prod"],
                            sd=plotDF1$sd[plotDF1$Variable=="leaf_prod"])
    bDF1$twig_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="twig_prod"],
                            sd=plotDF1$sd[plotDF1$Variable=="twig_prod"])
    bDF1$bark_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="bark_prod"],
                            sd=plotDF1$sd[plotDF1$Variable=="bark_prod"])
    bDF1$seed_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="seed_prod"],
                            sd=plotDF1$sd[plotDF1$Variable=="seed_prod"])
    bDF1$wood_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="wood_prod"],
                            sd=plotDF1$sd[plotDF1$Variable=="wood_prod"])
    bDF1$fineroot_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="fineroot_prod"],
                                sd=plotDF1$sd[plotDF1$Variable=="fineroot_prod"])
    bDF1$coarseroot_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="coarseroot_prod"],
                                  sd=plotDF1$sd[plotDF1$Variable=="coarseroot_prod"])
    bDF1$understorey_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="understorey_prod"],
                                   sd=plotDF1$sd[plotDF1$Variable=="understorey_prod"])
    bDF1$sum <- with(bDF1, herb_consump + leaf_prod + twig_prod + bark_prod + seed_prod + wood_prod + fineroot_prod + coarseroot_prod + understorey_prod)
    
    #plotDF2$conf_low[plotDF2$Variable=="NPP"] <- plotDF2$effect_size[plotDF2$Variable=="NPP"] -
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    #plotDF2$conf_high[plotDF2$Variable=="NPP"] <- plotDF2$effect_size[plotDF2$Variable=="NPP"] +
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    
    plotDF2$conf_low[plotDF2$Variable=="NPP"] <- plotDF2$effect_size[plotDF2$Variable=="NPP"] -
        sd(bDF1$sum) 
    plotDF2$conf_high[plotDF2$Variable=="NPP"] <- plotDF2$effect_size[plotDF2$Variable=="NPP"] +
        sd(bDF1$sum) 
    
    
    ## Outfluxes - respirations except Ra
    bDF1 <- data.frame(c(1:n.b), NA, NA)
    colnames(bDF1) <- c("bootID", "doc", "hetero_respiration")
    bDF1$doc <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="doc"],
                      sd=plotDF1$sd[plotDF1$Variable=="doc"])
    bDF1$hetero_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="hetero_respiration"],
                                     sd=plotDF1$sd[plotDF1$Variable=="hetero_respiration"])
    
    bDF1$sum <- with(bDF1, hetero_respiration + doc )
    
    #plotDF2$conf_low[plotDF2$Variable=="Outfluxes"] <- plotDF2$effect_size[plotDF2$Variable=="Outfluxes"] -
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    #plotDF2$conf_high[plotDF2$Variable=="Outfluxes"] <- plotDF2$effect_size[plotDF2$Variable=="Outfluxes"] +
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    
    plotDF2$conf_low[plotDF2$Variable=="Outfluxes"] <- plotDF2$effect_size[plotDF2$Variable=="Outfluxes"] -
        sd(bDF1$sum)
    plotDF2$conf_high[plotDF2$Variable=="Outfluxes"] <- plotDF2$effect_size[plotDF2$Variable=="Outfluxes"] +
        sd(bDF1$sum) 
    
    ## Ra
    bDF1 <- data.frame(c(1:n.b), NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "root_respiration", "understorey_respiration", "over_leaf_respiration",
                        "wood_respiration", "voc")
    bDF1$root_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="root_respiration"],
                                   sd=plotDF1$sd[plotDF1$Variable=="root_respiration"])
    bDF1$understorey_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="understorey_respiration"],
                                          sd=plotDF1$sd[plotDF1$Variable=="understorey_respiration"])
    bDF1$over_leaf_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="over_leaf_respiration"],
                                        sd=plotDF1$sd[plotDF1$Variable=="over_leaf_respiration"])
    bDF1$wood_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="wood_respiration"],
                                   sd=plotDF1$sd[plotDF1$Variable=="wood_respiration"])
    bDF1$voc <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="voc"],
                      sd=plotDF1$sd[plotDF1$Variable=="voc"])
    
    bDF1$sum <- with(bDF1, wood_respiration + over_leaf_respiration + understorey_respiration + root_respiration + voc)
    
    #plotDF2$conf_low[plotDF2$Variable=="Ra"] <- plotDF2$effect_size[plotDF2$Variable=="Ra"] -
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    #plotDF2$conf_high[plotDF2$Variable=="Ra"] <- plotDF2$effect_size[plotDF2$Variable=="Ra"] +
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    
    plotDF2$conf_low[plotDF2$Variable=="Ra"] <- plotDF2$effect_size[plotDF2$Variable=="Ra"] -
        sd(bDF1$sum) 
    plotDF2$conf_high[plotDF2$Variable=="Ra"] <- plotDF2$effect_size[plotDF2$Variable=="Ra"] +
        sd(bDF1$sum) 
    
    ## total outfluxes
    bDF1 <- data.frame(c(1:n.b), NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "doc", "hetero_respiration", "root_respiration", "understorey_respiration", "over_leaf_respiration",
                        "wood_respiration", "voc")
    bDF1$doc <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="doc"],
                      sd=plotDF1$sd[plotDF1$Variable=="doc"])
    bDF1$hetero_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="hetero_respiration"],
                                     sd=plotDF1$sd[plotDF1$Variable=="hetero_respiration"])
    bDF1$root_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="root_respiration"],
                                   sd=plotDF1$sd[plotDF1$Variable=="root_respiration"])
    bDF1$understorey_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="understorey_respiration"],
                                          sd=plotDF1$sd[plotDF1$Variable=="understorey_respiration"])
    bDF1$over_leaf_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="over_leaf_respiration"],
                                        sd=plotDF1$sd[plotDF1$Variable=="over_leaf_respiration"])
    bDF1$wood_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="wood_respiration"],
                                   sd=plotDF1$sd[plotDF1$Variable=="wood_respiration"])
    bDF1$voc <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="voc"],
                      sd=plotDF1$sd[plotDF1$Variable=="voc"])
    bDF1$sum <- with(bDF1, hetero_respiration + doc + voc + 
                         wood_respiration + over_leaf_respiration + understorey_respiration + root_respiration)
    
    plotDF2$conf_low[plotDF2$Variable=="total_outflux"] <- plotDF2$effect_size[plotDF2$Variable=="total_outflux"] -
        se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    plotDF2$conf_high[plotDF2$Variable=="total_outflux"] <- plotDF2$effect_size[plotDF2$Variable=="total_outflux"] +
        se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    
    plotDF2$conf_low[plotDF2$Variable=="total_outflux"] <- plotDF2$effect_size[plotDF2$Variable=="total_outflux"] -
        sd(bDF1$sum) 
    plotDF2$conf_high[plotDF2$Variable=="total_outflux"] <- plotDF2$effect_size[plotDF2$Variable=="total_outflux"] +
        sd(bDF1$sum)
    
    
    #subDF <- plotDF2[plotDF2$Variable%in%c("Outfluxes","Ra"),]
    #
    #subDF$conf_low_radius <- with(subDF, effect_size - conf_low)
    #subDF$conf_high_radius <- with(subDF, conf_high - effect_size)
    #subDF$conf_low_radius_sq <- (subDF$conf_low_radius)^2
    #subDF$conf_high_radius_sq <- (subDF$conf_high_radius)^2
    #subDF$plot.cat2 <- "total_outflux"
    #
    #subDF2 <- summaryBy(conf_high_radius_sq~plot.cat2, keep.names=T, na.rm=T, data=subDF, FUN=sum)
    #subDF3 <- summaryBy(conf_low_radius_sq~plot.cat2, keep.names=T, na.rm=T, data=subDF, FUN=sum)
    #
    #subDF2$conf_high_radius <- sqrt(subDF2$conf_high_radius_sq)
    #subDF3$conf_low_radius <- sqrt(subDF3$conf_low_radius_sq)
    #
    #plotDF2[6,"conf_low"] <- plotDF2[6,"effect_size"] - subDF3$conf_low_radius
    #plotDF2[6,"conf_high"] <- plotDF2[6,"effect_size"] + subDF2$conf_high_radius
    
    
    plotDF2$plot.cat <- c("total_change_in_pool", "total_influx", "total_npp", "total_rh", "total_ra", "total_outflux")
    
    ### Combine the plots
    plotDF3 <- plotDF1[,c("Variable", "effect_size", "conf_low", "conf_high", "plot.cat")]
    plotDF <- rbind(plotDF3, plotDF2[c(1,3,5,6),])
    
    ### Assign plot cat 2
    plotDF$plot.cat2[plotDF$plot.cat=="Influxes"] <- "A"
    plotDF$plot.cat2[plotDF$plot.cat=="total_npp"] <- "B"
    plotDF$plot.cat2[plotDF$plot.cat=="total_ra"] <- "B"
    plotDF$plot.cat2[plotDF$plot.cat=="total_outflux"] <- "C"
    plotDF$plot.cat2[plotDF$plot.cat=="total_change_in_pool"] <- "C"
    plotDF$plot.cat2[plotDF$plot.cat=="NPP"] <- "D"
    plotDF$plot.cat2[plotDF$plot.cat=="Ra"] <- "E"
    plotDF$plot.cat2[plotDF$plot.cat=="Outfluxes"] <- "E"
    plotDF$plot.cat2[plotDF$plot.cat=="Change_in_pools"] <- "F"
    
    ### exclude variables with averaged CO2 effect < 1 g m-2 yr-1
    # plotDF <- subset(plotDF, effect_size >= 1)
    
    ### Order plot DF
    plotDF <- plotDF[order(plotDF$plot.cat2),]
    
    ### Plot variable ordering
    plotDF$var.order <- 1:length(plotDF$plot.cat2)
    
    ### confidence interval for each plot.cat2 categories
    ### Need to separately calculate confidence interval using sums of each category
    confDF <- summaryBy(effect_size~plot.cat2, keep.names=T, na.rm=T, data=plotDF, FUN=sum)
    
    ## update A influxes
    confDF$conf_low[confDF$plot.cat2 == "A"] <- plotDF2$conf_low[plotDF2$Variable=="Influxes"]
    confDF$conf_high[confDF$plot.cat2 == "A"] <- plotDF2$conf_high[plotDF2$Variable=="Influxes"]
    
    ## update D NPP
    confDF$conf_low[confDF$plot.cat2 == "D"] <- plotDF2$conf_low[plotDF2$Variable=="NPP"]
    confDF$conf_high[confDF$plot.cat2 == "D"] <- plotDF2$conf_high[plotDF2$Variable=="NPP"]
    
    ## update E total outfluxes
    confDF$conf_low[confDF$plot.cat2 == "E"] <- plotDF2$conf_low[plotDF2$Variable=="total_outflux"]
    confDF$conf_high[confDF$plot.cat2 == "E"] <- plotDF2$conf_high[plotDF2$Variable=="total_outflux"]
    
    ## update F change in pools
    confDF$conf_low[confDF$plot.cat2 == "F"] <- plotDF2$conf_low[plotDF2$Variable=="Change_in_pools"]
    confDF$conf_high[confDF$plot.cat2 == "F"] <- plotDF2$conf_high[plotDF2$Variable=="Change_in_pools"]
    
    ## update B NPP + Ra
    bDF1 <- data.frame(c(1:n.b), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "root_respiration", "understorey_respiration", "over_leaf_respiration", "voc",
                        "wood_respiration","herb_consump", "leaf_prod", "twig_prod", "bark_prod", "seed_prod",
                        "wood_prod", "fineroot_prod", "coarseroot_prod", "understorey_prod")
    bDF1$root_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="root_respiration"],
                                   sd=plotDF1$sd[plotDF1$Variable=="root_respiration"])
    bDF1$understorey_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="understorey_respiration"],
                                          sd=plotDF1$sd[plotDF1$Variable=="understorey_respiration"])
    bDF1$over_leaf_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="over_leaf_respiration"],
                                        sd=plotDF1$sd[plotDF1$Variable=="over_leaf_respiration"])
    bDF1$voc <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="voc"],
                      sd=plotDF1$sd[plotDF1$Variable=="voc"])
    bDF1$wood_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="wood_respiration"],
                                   sd=plotDF1$sd[plotDF1$Variable=="wood_respiration"])
    bDF1$herb_consump <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="herb_consump"],
                               sd=plotDF1$sd[plotDF1$Variable=="herb_consump"])
    bDF1$leaf_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="leaf_prod"],
                            sd=plotDF1$sd[plotDF1$Variable=="leaf_prod"])
    bDF1$twig_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="twig_prod"],
                            sd=plotDF1$sd[plotDF1$Variable=="twig_prod"])
    bDF1$bark_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="bark_prod"],
                            sd=plotDF1$sd[plotDF1$Variable=="bark_prod"])
    bDF1$seed_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="seed_prod"],
                            sd=plotDF1$sd[plotDF1$Variable=="seed_prod"])
    bDF1$wood_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="wood_prod"],
                            sd=plotDF1$sd[plotDF1$Variable=="wood_prod"])
    bDF1$fineroot_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="fineroot_prod"],
                                sd=plotDF1$sd[plotDF1$Variable=="fineroot_prod"])
    bDF1$coarseroot_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="coarseroot_prod"],
                                  sd=plotDF1$sd[plotDF1$Variable=="coarseroot_prod"])
    bDF1$understorey_prod <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="understorey_prod"],
                                   sd=plotDF1$sd[plotDF1$Variable=="understorey_prod"])
    bDF1$sum <- with(bDF1, herb_consump + leaf_prod + twig_prod + bark_prod + seed_prod + wood_prod + fineroot_prod +
                         coarseroot_prod + understorey_prod + wood_respiration + over_leaf_respiration + 
                         understorey_respiration + root_respiration + voc)
    
    #confDF$conf_low[confDF$plot.cat2 == "B"]  <- confDF$effect_size[confDF$plot.cat2 == "B"] -
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    #confDF$conf_high[confDF$plot.cat2 == "B"]  <- confDF$effect_size[confDF$plot.cat2 == "B"] +
    #   se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    
    confDF$conf_low[confDF$plot.cat2 == "B"]  <- confDF$effect_size[confDF$plot.cat2 == "B"] -
        sd(bDF1$sum) 
    confDF$conf_high[confDF$plot.cat2 == "B"]  <- confDF$effect_size[confDF$plot.cat2 == "B"] +
        sd(bDF1$sum) 
    
    ## update C change in pool + total outfluxes
    bDF1 <- data.frame(c(1:n.b), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(bDF1) <- c("bootID", "delta_soil_c", "delta_leaf_c", "delta_wood_c", "delta_fineroot_c", "delta_coarseroot_c", 
                        "delta_litter_c","delta_microbial_c", "delta_mycorrhizal_c", "delta_insect_c",
                        "doc", "hetero_respiration", "root_respiration", 
                        "understorey_respiration", "over_leaf_respiration",
                        "wood_respiration", "voc")
    bDF1$delta_soil_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_soil_c"],
                               sd=plotDF1$sd[plotDF1$Variable=="delta_soil_c"])
    bDF1$delta_leaf_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_leaf_c"],
                               sd=plotDF1$sd[plotDF1$Variable=="delta_leaf_c"])
    bDF1$delta_wood_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_wood_c"],
                               sd=plotDF1$sd[plotDF1$Variable=="delta_wood_c"])
    bDF1$delta_fineroot_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_fineroot_c"],
                                   sd=plotDF1$sd[plotDF1$Variable=="delta_fineroot_c"])
    bDF1$delta_coarseroot_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_coarseroot_c"],
                                     sd=plotDF1$sd[plotDF1$Variable=="delta_coarseroot_c"])
    bDF1$delta_litter_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_litter_c"],
                                 sd=plotDF1$sd[plotDF1$Variable=="delta_litter_c"])
    bDF1$delta_microbial_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_microbial_c"],
                                    sd=plotDF1$sd[plotDF1$Variable=="delta_microbial_c"])
    bDF1$delta_mycorrhizal_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_mycorrhizal_c"],
                                      sd=plotDF1$sd[plotDF1$Variable=="delta_mycorrhizal_c"])
    bDF1$delta_insect_c <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="delta_insect_c"],
                                 sd=plotDF1$sd[plotDF1$Variable=="delta_insect_c"])
    bDF1$doc <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="doc"],
                      sd=plotDF1$sd[plotDF1$Variable=="doc"])
    bDF1$hetero_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="hetero_respiration"],
                                     sd=plotDF1$sd[plotDF1$Variable=="hetero_respiration"])
    bDF1$root_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="root_respiration"],
                                   sd=plotDF1$sd[plotDF1$Variable=="root_respiration"])
    bDF1$understorey_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="understorey_respiration"],
                                          sd=plotDF1$sd[plotDF1$Variable=="understorey_respiration"])
    bDF1$over_leaf_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="over_leaf_respiration"],
                                        sd=plotDF1$sd[plotDF1$Variable=="over_leaf_respiration"])
    bDF1$wood_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="wood_respiration"],
                                   sd=plotDF1$sd[plotDF1$Variable=="wood_respiration"])
    bDF1$voc <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="voc"],
                      sd=plotDF1$sd[plotDF1$Variable=="voc"])
    bDF1$sum <- with(bDF1, delta_soil_c + delta_leaf_c + delta_wood_c + delta_fineroot_c + delta_coarseroot_c + 
                         delta_litter_c + hetero_respiration + doc + voc + 
                         wood_respiration + over_leaf_respiration + understorey_respiration + root_respiration)
    
    #confDF$conf_low[confDF$plot.cat2 == "C"]  <- confDF$effect_size[confDF$plot.cat2 == "C"] -
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    #confDF$conf_high[confDF$plot.cat2 == "C"]  <- confDF$effect_size[confDF$plot.cat2 == "C"] +
    #    se(bDF1$sum) * qt(0.95/2 + .5, length(bDF1$sum)-1)
    
    
    confDF$conf_low[confDF$plot.cat2 == "C"]  <- confDF$effect_size[confDF$plot.cat2 == "C"] -
        sd(bDF1$sum) 
    confDF$conf_high[confDF$plot.cat2 == "C"]  <- confDF$effect_size[confDF$plot.cat2 == "C"] +
        sd(bDF1$sum) 
    
    #plotDF$conf_low_radius <- with(plotDF, effect_size - conf_low)
    #plotDF$conf_high_radius <- with(plotDF, conf_high - effect_size)
    #plotDF$conf_low_radius_sq <- (plotDF$conf_low_radius)^2
    #plotDF$conf_high_radius_sq <- (plotDF$conf_high_radius)^2
    #
    #confDF2 <- summaryBy(conf_high_radius_sq~plot.cat2, keep.names=T, na.rm=T, data=plotDF, FUN=sum)
    #confDF3 <- summaryBy(conf_low_radius_sq~plot.cat2, keep.names=T, na.rm=T, data=plotDF, FUN=sum)
    #
    #confDF2$conf_high_radius <- sqrt(confDF2$conf_high_radius_sq)
    #confDF3$conf_low_radius <- sqrt(confDF3$conf_low_radius_sq)
    #
    #confDF$conf_low <- confDF$effect_size - confDF3$conf_low_radius
    #confDF$conf_high <- confDF$effect_size + confDF2$conf_high_radius
    
    bDF1 <- data.frame(c(1:n.b), NA, NA)
    colnames(bDF1) <- c("bootID", "hetero_respiration", "root_respiration")
    bDF1$hetero_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="hetero_respiration"],
                                     sd=plotDF1$sd[plotDF1$Variable=="hetero_respiration"])
    bDF1$root_respiration <- rnorm(n.b, mean=plotDF1$effect_size[plotDF1$Variable=="root_respiration"],
                                   sd=plotDF1$sd[plotDF1$Variable=="root_respiration"])
    bDF1$sum <- with(bDF1, hetero_respiration + root_respiration)
    soil_resp_mean <- mean(bDF1$sum)
    soil_resp_conf_low <- mean(bDF1$sum)-sd(bDF1$sum) 
    soil_resp_conf_high  <- mean(bDF1$sum)+sd(bDF1$sum) 
    
    
    y.lab1 <- c("ch4"=expression(CH[4]),                          # 1
                "over_gpp"=expression(GPP[o]),                    # 2
                "understorey_gpp"=expression(GPP[u]),             # 3
                "Change_in_pools"=expression(Delta*C[pools]),
                "NPP"="NPP",                                      # 4
                "total_outflux"="R",                                  # 5
                "Ra"=expression(R[a]),                            # 6
                "herb_consump"=expression(NPP[insect]),           # 7
                "leaf_prod"=expression(NPP[leaf]),                # 8
                "twig_prod"=expression(NPP[twig]),                # 9
                "bark_prod"=expression(NPP[bark]),                # 10
                "seed_prod"=expression(NPP[seed]),                # 11
                "wood_prod"=expression(NPP[stem]),                # 12
                "fineroot_prod"=expression(NPP[froot]),           # 13
                "coarseroot_prod"=expression(NPP[croot]),         # 14
                "understorey_prod"=expression(NPP[ua]),           # 15
                #"mycorrhizal_prod"=expression(NPP[myc]),           # 15
                "root_respiration"=expression(R[root]),           # 16
                "understorey_respiration"=expression(R[ua]),      # 17
                "doc"=expression(DOC),                            # 18
                "voc"=expression(VOC),
                "hetero_respiration"=expression(R[rh]),            # 19
                "over_leaf_respiration"=expression(R[leaf]),      # 20
                "wood_respiration"=expression(R[stem]),           # 21
                "delta_soil_c"=expression(Delta*C[soil]),         # 22
                "delta_leaf_c"=expression(Delta*C[leaf]),         # 23
                "delta_wood_c"=expression(Delta*C[stem]),         # 24
                "delta_fineroot_c"=expression(Delta*C[froot]),    # 25
                "delta_coarseroot_c"=expression(Delta*C[croot]),  # 26
                "delta_understorey_c"=expression(Delta*C[ua]),    # 27
                "delta_litter_c"=expression(Delta*C[lit]))     # 28
    
    
    ## gpp
    colfunc.inf <- colorRampPalette(c("black", "grey"))
    A.col.list <- colfunc.inf(3)
    
    # 2nd column bar - NPP + Ra 
    colfunc.inf <- colorRampPalette(c("darkgreen", "darkred"))
    B.col.list <- colfunc.inf(2)
    
    # 2nd column bar - Change in pools + all R
    colfunc.inf <- colorRampPalette(c("red", "blue"))
    C.col.list <- colfunc.inf(2)
    
    ## NPP
    colfunc.npp <- colorRampPalette(c("darkgreen", "yellow"))
    D.col.list <- colfunc.npp(9)
    
    ## R
    colfunc.R <- colorRampPalette(c("darkred", "pink"))
    E.col.list <- colfunc.R(7)
    
    ### Change in pools
    colfunc.delta <- colorRampPalette(c("darkblue", "cyan"))
    F.col.list <- colfunc.delta(7)
    
    ### Combine all color list
    col.list1 <- c("ch4"=A.col.list[1], 
                   "over_gpp"=A.col.list[2],                    
                   "understorey_gpp"=A.col.list[3], 
                   "Change_in_pools"=C.col.list[1],   
                   "NPP"=B.col.list[1],   
                   "total_outflux"=C.col.list[2],
                   "Ra"=B.col.list[2],      
                   "herb_consump"=D.col.list[1],           
                   "leaf_prod"=D.col.list[2],                
                   "twig_prod"=D.col.list[3],                
                   "bark_prod"=D.col.list[4],                
                   "seed_prod"=D.col.list[5],                
                   "wood_prod"=D.col.list[6],               
                   "fineroot_prod"=D.col.list[7],           
                   "coarseroot_prod"=D.col.list[8],         
                   "understorey_prod"=D.col.list[9],   
                   #"mycorrhizal_prod"=D.col.list[10], 
                   "over_leaf_respiration"=E.col.list[1],      
                   "wood_respiration"=E.col.list[2],           
                   "root_respiration"=E.col.list[3],           
                   "understorey_respiration"=E.col.list[4],      
                   "hetero_respiration"=E.col.list[5],            
                   "doc"=E.col.list[6],     
                   "voc"=E.col.list[7],                        
                   
                   "delta_leaf_c"=F.col.list[1],         
                   "delta_wood_c"=F.col.list[2],        
                   "delta_fineroot_c"=F.col.list[3],    
                   "delta_coarseroot_c"=F.col.list[4],  
                   "delta_understorey_c"=F.col.list[5],    
                   "delta_soil_c"=F.col.list[6],         
                   "delta_litter_c"=F.col.list[7])                         
    
    #### Plotting
    p1 <- ggplot(plotDF,
                 aes(plot.cat2, effect_size)) +   
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("Influxes", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste(Delta*C[pools], "+R")),
                                  "NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF$Variable,
                          values = col.list1,
                          labels=y.lab1) +
        theme_linedraw() +
        ylim(-500,1000)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)
    
    p2 <- ggplot(plotDF,
                 aes(plot.cat2, effect_size)) +   
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        geom_errorbar(data=confDF, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), width=0.2, size=1, color="grey") + 
        geom_point(data=confDF, mapping=aes(x=plot.cat2, y=effect_size), size=4, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("Influxes", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste(Delta*C[pools], "+R")),
                                  "NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF$Variable,
                          values = col.list1,
                          labels=y.lab1) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)
    
    
    ### Simplify the plot by eleminating fluxes that has CO2 effect < 1 g m-2 yr-1
    plotDF2 <- subset(plotDF, abs(effect_size) >= 1)
    
    ### Order plot DF
    plotDF2 <- plotDF2[order(plotDF2$plot.cat2),]
    
    ### Plot variable ordering
    plotDF2$var.order <- 1:length(plotDF2$plot.cat2)
    
    library(viridis)
    
    ## gpp
    colfunc.inf <- colorRampPalette(c( "grey21","dimgrey"))
    A.col.list <- colfunc.inf(2)
    
    # 2nd column bar - NPP + Ra 
    colfunc.inf <- colorRampPalette(c("green4", "orange"))
    B.col.list <- colfunc.inf(2)
    
    # 3rd column bar - Change in pools + all R
    colfunc.inf <- colorRampPalette(c("blue", "orangered"))
    C.col.list <- colfunc.inf(2)
    
    ## NPP
    #colfunc.npp <- colorRampPalette(c("seagreen", "green", "yellowgreen"))
    #D.col.list <- colfunc.npp(9)
    D.col.list <- c("darkgreen", "seagreen", "chartreuse3", "yellowgreen", "chartreuse1", "springgreen3", "springgreen1")
    
    ## R
    colfunc.R <- colorRampPalette(c("darkred", "pink"))
    E.col.list <- colfunc.R(5)
    
    ### Change in pools
    colfunc.delta <- colorRampPalette(c("darkblue", "cyan"))
    F.col.list <- colfunc.delta(6)
    
    v.list <- viridis(27)
    #v.list <- rainbow(26)
    
    ### Combine all color list
    col.list2 <- c("over_gpp"=A.col.list[1],                    
                   "understorey_gpp"=A.col.list[2], 
                   "Change_in_pools"=C.col.list[1],   
                   "NPP"=B.col.list[1],   
                   "total_outflux"=C.col.list[2],
                   "Ra"=B.col.list[2],      
                   "herb_consump"=D.col.list[1],           
                   "leaf_prod"=D.col.list[2],                
                   #"twig_prod"=D.col.list[3],                
                   "bark_prod"=D.col.list[3],                
                   "seed_prod"=D.col.list[4],                
                   "wood_prod"=D.col.list[6],               
                   "fineroot_prod"=D.col.list[5],           
                   #"coarseroot_prod"=D.col.list[8],         
                   "understorey_prod"=D.col.list[7], 
                   #"mycorrhizal_prod"=D.col.list[10],           
                   
                   "over_leaf_respiration"=E.col.list[4],      
                   "wood_respiration"=E.col.list[5],           
                   "root_respiration"=E.col.list[1],           
                   "understorey_respiration"=E.col.list[2],      
                   "hetero_respiration"=E.col.list[3],            
                   "delta_wood_c"=F.col.list[2],        
                   "delta_fineroot_c"=F.col.list[3],    
                   #"delta_coarseroot_c"=F.col.list[4],  
                   "delta_leaf_c"=F.col.list[4],  
                   #"delta_understorey_c"=F.col.list[5],    
                   "delta_soil_c"=F.col.list[6],         
                   "delta_litter_c"=F.col.list[1],
                   "delta_microbial_c"=F.col.list[5])     
    #col.list2 <- c("over_gpp"=v.list[1],                    
    #               "understorey_gpp"=v.list[2],  
    #               "Change_in_pools"=v.list[3],
    #               "NPP"=v.list[4],   
    #               "total_outflux"=v.list[5],
    #               "Ra"=v.list[6],      
    #               "herb_consump"=v.list[7],           
    #               "leaf_prod"=v.list[8],                
    #               "twig_prod"=v.list[9],                
    #               "bark_prod"=v.list[10],                
    #               "seed_prod"=v.list[11],                
    #               "wood_prod"=v.list[12],               
    #               "fineroot_prod"=v.list[13],   
    #               "coarseroot_prod"=v.list[14],
    #               "understorey_prod"=v.list[15], 
    #               "root_respiration"=v.list[16],           
    #               "understorey_respiration"=v.list[17],      
    #               "hetero_respiration"=v.list[18],            
    #               "over_leaf_respiration"=v.list[19],      
    #               "wood_respiration"=v.list[20],           
    #               "delta_soil_c"=v.list[21],         
    #               "delta_wood_c"=v.list[22],        
    #               "delta_fineroot_c"=v.list[23],
    #               "delta_coarseroot_c"=v.list[24],
    #               "delta_understorey_c"=v.list[25],
    #               "delta_litter_c"=v.list[26])       
    
    # y label
    y.lab2 <- c("over_gpp"=expression(GPP[o]),                    # 1
                "understorey_gpp"=expression(GPP[u]),             # 2
                "Change_in_pools"=expression(Delta*C[pools]),
                "NPP"="NPP",                                      # 3
                "total_outflux"="R",                              # 4
                "Ra"=expression(R[a]),                            # 5
                "herb_consump"=expression(NPP[insect]),           # 6
                "leaf_prod"=expression(NPP[leaf]),                # 7
                "twig_prod"=expression(NPP[twig]),                # 8
                "bark_prod"=expression(NPP[bark]),                # 9
                "seed_prod"=expression(NPP[seed]),                # 10
                "wood_prod"=expression(NPP[stem]),                # 11
                "fineroot_prod"=expression(NPP[froot]),           # 12
                "coarseroot_prod"=expression(NPP[croot]),         # 13
                "understorey_prod"=expression(NPP[ua]),           # 14
                #"mycorrhizal_prod"=expression(NPP[myc]),           # 14
                
                "root_respiration"=expression(R[root]),           # 15
                "understorey_respiration"=expression(R[ua]),      # 16
                "hetero_respiration"=expression(R[rh]),            # 17
                "over_leaf_respiration"=expression(R[leaf]),      # 18
                "wood_respiration"=expression(R[stem]),           # 19
                "delta_soil_c"=expression(Delta*C[soil]),         # 20
                "delta_wood_c"=expression(Delta*C[stem]),         # 21
                "delta_fineroot_c"=expression(Delta*C[froot]),    # 22
                #"delta_coarseroot_c"=expression(Delta*C[croot]),    # 23
                "delta_leaf_c"=expression(Delta*C[leaf]),
                "delta_understorey_c"=expression(Delta*C[ua]),    # 24
                "delta_litter_c"=expression(Delta*C[lit]),
                "delta_microbial_c"=expression(Delta*C[micr]))     # 25
    
    #for (i in 1:5) {
    #    if (confDF[i, "conf_low"] >= -50) {
    #        confDF[i, "conf_low_sc"] <- confDF[i, "conf_low"]
    #    } else {
    #        confDF[i, "conf_low_sc"] <- -50 + (confDF[i, "conf_low"] * 0.125)
    #    }
    #    
    #    if (confDF[i, "conf_high"] <= 150) {
    #        confDF[i, "conf_high_sc"] <- confDF[i, "conf_high"]
    #    } else {
    #        confDF[i, "conf_high_sc"] <- 150 + ((confDF[i, "conf_high"] - 150) * 0.1)
    #    }
    #
    #}
    
    p3 <- ggplot(plotDF2,
                 aes(plot.cat2, effect_size)) +  
        geom_hline(yintercept=0)+
        #geom_hline(yintercept=150, linetype="dashed", color="grey")+
        #geom_hline(yintercept=-50, linetype="dashed", color="grey")+
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        geom_errorbar(data=confDF, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
                      width=0.1, size=1, color="grey") + 
        geom_point(data=confDF, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("GPP", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste("R+",Delta*C[pools])),
                                  "NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$Variable,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        geom_vline(xintercept = 3.5, linetype="dashed", color="black")+
        #geom_vline(xintercept = 4.5, linetype="dashed", color="black")+
        #geom_vline(xintercept = 5.5, linetype="dashed", color="black")+
        
        scale_y_continuous(limits=c(-400, 800), 
                           breaks=c(-400, -200, -100, 0, 100, 200, 400, 800),
                           labels=c(-400, -200, -100, 0, 100, 200, 400, 800))+
        #geom_text(aes(label=Variable), position=position_stack(), stat="identity", size=3, parse=T)
        guides(fill=guide_legend(ncol=6))#+
        #ylim(-400,800)+
        #scale_y_continuous(breaks=c(-400,0,400,800), limits=c(-400, 800))
    
    #plot(p3)
    
    ### Plotting
    pdf("Output/eco2_effect_on_gpp_and_subsequent_fluxes_pools_with_covariate_bootstrapped.pdf", width=8, height=6)
    #plot(p1)
    plot(p3)
    #plot(p2)
    dev.off()
    
    
    
}