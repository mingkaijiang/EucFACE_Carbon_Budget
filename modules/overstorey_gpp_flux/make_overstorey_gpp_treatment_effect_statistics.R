make_overstorey_gpp_treatment_effect_statistics <- function(inDF, var.cond, 
                                                            var.col, date.as.factor,
                                                            stat.model) {

    ### Pass in covariate values (assuming 1 value for each ring)
    covDF <- summaryBy(soil_p_g_m2~Ring, data=soil_p_pool, FUN=mean, keep.names=T, na.rm=T)
    covDF$Ring <- as.numeric(covDF$Ring)
    inDF$Ring <- as.numeric(inDF$Ring)
    for (i in 1:6) {
        inDF$Cov[inDF$Ring] <- covDF$soil_p_g_m2[covDF$Ring]
    }
    
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
    tDF <- summaryBy(Value+Cov~Trt+Ring+Yr,data=inDF,FUN=sum, keep.names=T)
    tDF$Yrf <- as.factor(tDF$Yr)
    
    ### Loop through data, return annual flux in g m-2 yr-1
    for (i in 1:6) {
        for (j in yr.list) {
            ### summed of all available data within a year
            tDF[tDF$Ring == i & tDF$Yr == j, "Value"] <- inDF$Value[inDF$Ring == i & inDF$Yr == j]
        }
    }
    
    ### Analyse the variable model
    ## model 1: no interaction, year as factor, ring random factor
    int.m1 <- "non-interative"
    modelt1 <- lmer(Value~Trt+Yrf + (1|Ring),data=tDF)
    
    ## anova
    m1.anova <- Anova(modelt, test="F")
    
    ## Check ele - amb diff
    summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size1 <- coef(modelt1)[[1]][1,2]
    
    ## confidence interval 
    eff.conf1 <- confint(modelt1,"Trtele")
    
    ### Analyse the variable model
    ## model 2: no interaction, year as factor, ring random factor
    int.m2 <- "interative"
    modelt2 <- lmer(Value~Trt*Yrf + (1|Ring),data=tDF)
    
    ## anova
    m2.anova <- Anova(modelt, test="F")
    
    ## Check ele - amb diff
    summ2 <- summary(glht(modelt2, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size2 <- coef(modelt2)[[1]][1,2]
    
    ## confidence interval 
    eff.conf2 <- confint(modelt2,"Trtele")
    
    ### Analyse the variable model
    ## model 2: no interaction, year as factor, ring random factor
    int.m2 <- "interative"
    modelt2 <- lmer(Value~Trt*Yrf + (1|Ring),data=tDF)
    
    ## anova
    m2.anova <- Anova(modelt, test="F")
    
    ## Check ele - amb diff
    summ2 <- summary(glht(modelt2, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size2 <- coef(modelt2)[[1]][1,2]
    
    ## confidence interval 
    eff.conf2 <- confint(modelt2,"Trtele")
    
    ### Analyse the variable model
    ## model 3: no interaction, year as factor, no random effect
    int.m3 <- "non-interative"
    modelt3 <- lm(Value~Trt+Yrf,data=tDF)
    
    ### anova
    m3.anova <- Anova(modelt3, test="F")
    
    ### Check ele - amb diff
    summ3 <- summary(glht(modelt3, linfct = mcp(Trt = "Tukey")))
    
    ### average effect size
    eff.size3 <- coef(modelt3)[[2]]
    
    ### confidence interval 
    eff.conf3 <- confint(modelt3,"Trtele")
    
    ### Analyse the variable model
    ## model 4: no interaction, year as factor, soil P as covariate
    int.m4 <- "non-interative"
    modelt4 <- lmer(Value~Trt+Yrf+Cov + (1|Ring),data=tDF)
    
    ## anova
    m4.anova <- Anova(modelt, test="F")
    
    ## Check ele - amb diff
    summ4 <- summary(glht(modelt4, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size4 <- coef(modelt4)[[1]][1,2]
    
    ## confidence interval 
    eff.conf4 <- confint(modelt4,"Trtele")
    
    
    
    
    
    
    
    ### conditional output
    if (stat.model == "no_interaction") {
        int.m <- int.m1
        modelt <- modelt1
        m.anova <- m1.anova
        summ <- summ1
        eff.size <- eff.size1
        eff.conf <- eff.conf1
    } else if (stat.model == "interaction") {
        int.m <- int.m2
        modelt <- modelt2
        m.anova <- m2.anova
        summ <- summ2
        eff.size <- eff.size2
        eff.conf <- eff.conf2
    } else if (stat.model == "no_random") {
        int.m <- int.m3
        modelt <- modelt3
        m.anova <- m3.anova
        summ <- summ3
        eff.size <- eff.size3
        eff.conf <- eff.conf3
    } else if (stat.model == "cov") {
        int.m <- int.m4
        modelt <- modelt4
        m.anova <- m4.anova
        summ <- summ4
        eff.size <- eff.size4
        eff.conf <- eff.conf4
    }

    return(list(int.state=int.m,
                mod = modelt, 
                anova = m.anova,
                diff = summ,
                eff = eff.size,
                conf = eff.conf))

}