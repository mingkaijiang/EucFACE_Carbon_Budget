make_r_hb_treatment_abs_effect_statistics <- function(inDF, var.cond, 
                                                         var.col, date.as.factor,
                                                         stat.model, return.outcome) {
    
    #### Assign amb and ele factor
    for (i in (1:length(inDF$Ring))) {
        if (inDF$Ring[i]==2|inDF$Ring[i]==3|inDF$Ring[i]==6) {
            inDF$Trt[i] <- "amb"
        } else {
            inDF$Trt[i] <- "ele"
        }
    }
    
    #### Assign factors
    inDF$Trt <- as.factor(inDF$Trt)
    inDF$Yr <- year(inDF$Date)
    inDF$Ring <- as.factor(inDF$Ring)
    inDF$Datef <- as.factor(inDF$Date)

    #### Update variable name so that this function can be used across different variables
    colnames(inDF)[var.col] <- "Value"
    
    #### dataframe with annual totals in g m-2 yr-1
    ###------ Treatment interacting with date, or not
    ###------ Ring random factor
    ###------ Unit g m-2 yr-1
    
    ## Get year list and ring list
    yr.list <- unique(inDF$Yr)
    tDF <- summaryBy(Value~Trt+Ring+Yr,data=inDF,FUN=sum, keep.names=T)
    tDF$Yrf <- as.factor(tDF$Yr)
    
    ### Loop through data, return annual flux in g m-2 yr-1
    for (i in 1:6) {
        for (j in yr.list) {
            ### averaged over days within a year 
            tmp <- with(inDF[inDF$Ring == i & inDF$Yr == j, ],
                        sum(Value*ndays, na.rm=T)/sum(ndays, na.rm=T)) * 365 / 1000
            tDF[tDF$Ring == i & tDF$Yr == j, "Value"] <- tmp
        }
    }
    
    ### Pass in covariate values (assuming 1 value for each ring)
    covDF <- summaryBy(soil_p_g_m2~Ring, data=soil_p_pool, FUN=mean, keep.names=T, na.rm=T)
    
    #cov2 <- lai_variable[lai_variable$Date<="2013-01-01",]
    cov2 <- lai_variable[lai_variable$Date=="2012-10-26",]
    covDF2 <- summaryBy(lai_variable~Ring, data=cov2, FUN=mean, keep.names=T)
    
    ### Read initial basal area data
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    f12$ba <- ((f12$X20.09.2012/2)^2) * pi
    baDF <- summaryBy(ba~Ring, data=f12, FUN=sum, na.rm=T, keep.names=T)
    
    ### return in unit of cm2/m2, which is m2 ha-1
    baDF$ba_ground_area <- baDF$ba / ring_area
    
    for (i in 1:6) {
        tDF$Cov[tDF$Ring==i] <- covDF$soil_p_g_m2[covDF$Ring==i]
        tDF$Cov2[tDF$Ring==i] <- covDF2$lai_variable[covDF2$Ring==i]
        tDF$Cov3[tDF$Ring==i] <- baDF$ba_ground_area[baDF$Ring==i]
    }
    
    ### Add psyllid attack event
    tDF$Psyllid <- "Before"
    tDF$Psyllid[tDF$Yr >= 2016] <- "After"
    
    ### add annual average LAI for each year and ring
    lai <- lai_variable
    lai$year <- as.numeric(year(lai$Date))
    cov6 <- summaryBy(lai_variable~Ring+year, data=lai, FUN=mean, keep.names=T)
    
    for (i in 1:6) {
        for (j in 2012:2016) {
            tDF$Cov6[tDF$Ring==i&tDF$Yr==j] <- cov6[cov6$Ring==i&cov6$year==j,"lai_variable"]
        }
    }
    
    tDF$Cov6 <- as.numeric(unlist(tDF$Cov6))
    
    ### Analyse the variable model
    ## model 1: no interaction, year as factor, ring random factor, include pre-treatment effect
    int.m1 <- "non-interative_with_covariate"
    modelt1 <- lmer(Value~Trt + Yrf + Cov6 + (1|Ring),data=tDF)
    #modelt1 <- lmer(Value~Trt + Yrf + Cov2 + (1|Ring),data=tDF)
    
    ## anova
    m1.anova <- Anova(modelt1, test="F")
    
    ## Check ele - amb diff
    summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size1 <- coef(modelt1)[[1]][1,2]
    
    ## confidence interval 
    eff.conf1 <- confint(modelt1,"Trtele")
    
    ### Analyse the variable model
    ## model 2: interaction, year as factor, ring random factor
    int.m2 <- "interative"
    modelt2 <- lmer(Value~Trt*Yrf + Cov2 + (1|Ring),data=tDF)
    
    ## anova
    m2.anova <- Anova(modelt2, test="F")
    
    ## Check ele - amb diff
    summ2 <- summary(glht(modelt2, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size2 <- coef(modelt2)[[1]][1,2]
    
    ## confidence interval 
    eff.conf2 <- confint(modelt2,"Trtele")
    
    ### Analyse the variable model
    ## model 3: no interaction, year as factor, covariate, linear model only
    int.m3 <- "non-interative_with_linear_covariate"
    modelt3 <- lm(Value~Trt + Yrf + Cov2,data=tDF)
    
    ## anova
    m3.anova <- Anova(modelt3, test="F")
    
    ## Check ele - amb diff
    summ3 <- summary(modelt3)
    
    ## average effect size
    eff.size3 <- coef(modelt3)[[2]]
    
    ### confidence interval
    eff.conf3 <- confint(modelt3,"Trtele")
    
    ## standard error of treatment
    eff.se3 <-sqrt(diag(vcov(modelt3)))[[2]]
    
    ### Analyse the variable model
    ## model 4: no interaction, year as factor, paired t-test
    int.m4 <- "paired_t_test"
    modelt4 <- t.test(Value~Trt, data=tDF, paired=T)
    
    ## average effect size
    eff.size4 <- -modelt4$estimate[[1]]
    
    ## confidence interval 
    eff.conf4 <- cbind(as.numeric(-modelt4$conf.int[2]),as.numeric(-modelt4$conf.int[1]))
    
    ### conditional output
    if (stat.model == "no_interaction_with_covariate") {
        out <- list(int.state=int.m1,
                    mod = modelt1, 
                    anova = m1.anova,
                    diff = summ1,
                    eff = eff.size1,
                    conf = eff.conf1)
    } else if (stat.model == "interaction_with_covariate") {
        out <- list(int.state=int.m2,
                    mod = modelt2, 
                    anova = m2.anova,
                    diff = summ2,
                    eff = eff.size2,
                    conf = eff.conf2)
    } else if (stat.model == "no_interaction_with_linear_covariate") {
        out <- list(int.state=int.m3,
                    mod = modelt3, 
                    anova = m3.anova,
                    diff = summ3,
                    se = eff.se3,
                    eff = eff.size3,
                    conf = eff.conf3)
    } else if (stat.model == "paired_t_test") {
        out <- list(int.state=int.m4,
                    mod = modelt4, 
                    anova = NA,
                    diff = NA,
                    eff = eff.size4,
                    conf = eff.conf4)
    }
    
    ### Predict the model with a standard LAI value
    newDF <- tDF
    #newDF$Cov6 <- 1.14815  # initial LAI averages
    newDF$Cov6 <- 1.703864  # long-term LAI averages
    
    newDF$predicted <- predict(out$mod, newdata=newDF)
    
    if (return.outcome == "model") {
        return(out)
    } else if (return.outcome == "predicted") {
        return(newDF)
    }
}
