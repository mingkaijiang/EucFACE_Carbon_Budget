make_overstorey_gpp_treatment_effect_statistics <- function(inDF, var.cond, 
                                                            var.col, date.as.factor,
                                                            stat.model) {

    ### Pass in covariate values (assuming 1 value for each ring)
    covDF <- summaryBy(soil_p_g_m2~Ring, data=soil_p_pool, FUN=mean, keep.names=T, na.rm=T)
    covDF$Ring <- as.numeric(covDF$Ring)
    inDF$Ring <- as.numeric(inDF$Ring)
    
    covDF2 <- summaryBy(p_mineralization_mg_m2_d~ring, data=soil_p_mineralization, FUN=mean, keep.names=T, na.rm=T)
    covDF2$Ring <- as.numeric(covDF2$ring)

    for (i in 1:6) {
        inDF$Cov[inDF$Ring==i] <- covDF$soil_p_g_m2[covDF$Ring==i]
        inDF$Cov2[inDF$Ring==i] <- covDF2$p_mineralization_mg_m2_d[covDF2$Ring==i]
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
    tDF <- summaryBy(Value+PreTrt+Cov+Cov2~Trt+Ring+Yr,data=inDF,FUN=sum, keep.names=T)
    tDF$Yrf <- as.factor(tDF$Yr)
    
    ### Loop through data, return annual flux in g m-2 yr-1
    for (i in 1:6) {
        for (j in yr.list) {
            ### summed of all available data within a year
            tDF[tDF$Ring == i & tDF$Yr == j, "Value"] <- inDF$Value[inDF$Ring == i & inDF$Yr == j]
        }
    }
    
    ### Add psyllid attack event
    tDF$Psyllid <- "Before"
    tDF$Psyllid[tDF$Yr >= 2016] <- "After"

    ### Analyse the variable model
    ## model 1: no interaction, year as factor, ring random factor, include pre-treatment effect
    int.m1 <- "non-interative_with_pretreatment"
    modelt1 <- lmer(Value~Trt + Yrf + PreTrt + (1|Ring),data=tDF)
    
    ## anova
    m1.anova <- Anova(modelt1, test="F")
    
    ## Check ele - amb diff
    summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size1 <- coef(modelt1)[[1]][1,2]
    
    ## confidence interval 
    eff.conf1 <- confint(modelt1,"Trtele")
    
    ### Analyse the variable model
    ## model 2: interaction, year as factor, ring random factor, with pre-treatment
    int.m2 <- "interative_with_pretreatment"
    modelt2 <- lmer(Value~Trt*Yrf+PreTrt + (1|Ring),data=tDF)
    
    ## anova
    m2.anova <- Anova(modelt2, test="F")
    
    ## Check ele - amb diff
    summ2 <- summary(glht(modelt2, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size2 <- coef(modelt2)[[1]][1,2]
    
    ## confidence interval 
    eff.conf2 <- confint(modelt2,"Trtele")
    
    ### Analyse the variable model
    ## model 3: no interaction, year as factor, soil P as covariate
    int.m3 <- "non-interative_with_pretreatment_and_covariate"
    modelt3 <- lmer(Value~Trt + Yrf + PreTrt + Cov + (1|Ring),data=tDF)

    ## anova
    m3.anova <- Anova(modelt3, test="F")
    
    ## Check ele - amb diff
    summ3 <- summary(glht(modelt3, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size3 <- coef(modelt3)[[1]][1,2]
    
    ## confidence interval 
    eff.conf3 <- confint(modelt3,"Trtele")
    
    ### Analyse the variable model
    ## model 4: no interaction, year as factor, paired t-test
    int.m4 <- "paired_t_test"
    modelt4 <- t.test(Value~Trt, data=tDF, paired=T)
    
    ## average effect size
    eff.size4 <- -modelt4$estimate[[1]]
    
    ## confidence interval 
    eff.conf4 <- cbind(as.numeric(-modelt4$conf.int[2]),as.numeric(-modelt4$conf.int[1]))
    
    
    m6 <- aov(Value~Trt*Yrf+PreTrt,data=tDF)
    summary(m6)
    model.tables(m6, "means")
    TukeyHSD(m6)
    
    m7 <- aov(Value~Trt*Psyllid,data=tDF)
    summary(m7)
    model.tables(m7, "means")
    TukeyHSD(m7)
    
    
    tDF3 <- subset(tDF, Yr<=2015)
    testDF2 <- summaryBy(Value~Yrf+Trt, data=tDF3, FUN=mean, keep.names=T)
    m8 <- lm(Value~Trt+Yrf, data=tDF3, paired=T)
    summary(m8)
    
    m9 <- lm(Value~Trt+Yrf, data=testDF2, paired=T)
    summary(m9)
    
    m10 <- aov(Value~Trt+Psyllid + Error(Ring), data=tDF, paired=T)
    summary(m10)
    
    m11 <- aov(Value ~ Trt*Psyllid + Error(Ring), data=tDF)
    summary(m11)
    
    
    ### Summary of findings on GPP:
    ### method tested include: paired t-test of treatment means,
    ###                        mixed linear model
    ###                        two-way anova
    ### variables tested include: CO2 treatment, psyllid attach year, year, soil P as covariate
    ### non-statistically significant treatment effect across all methods
    ### But:
    ### 1. lower bound of standard error is above 0 for linear model of treatment means
    ### 2. Before psyllid attack, ambient is consistently lower than elevated treatment (treatment means),
    ###    After psyllid attack (2016), ambient is higher than elevated treatment (treatment means).
    ### 3. Psyllid attack is marginally significant
    ### 4. Year can be marginally significant
    ### So the summary is that, eCO2 does not have a statistically significant effect on GPP, 
    ### and the reason we did not detect a CO2 treatment effect is multitude: complicated by biological disturbance,
    ### inter-annual variability, and inter-treatment variability. 
    ### Question is, is this argument defensible (alternatively, what to show to make this argument defensible)? 
    ### And, what implication this might have for the following C budget? And, what message to tell?
    ###
    ### What to show: descriptive texts in the manuscript and refer to supplementary analyses
    ### Implications for the C budget: repeat the same analyses for all other variables and hopefully message is similar throughout
    ### Message: Natural experiment is complicated,
    ###          the capacity for EucFACE to respond more positively to eCO2 is constrained by multiple factors,
    ###          Future eCO2 probably won't cause enhanced C sequestration in similar mature forests
    ### Problem with introducing inter-annual variability or psyllid attack is that, we don't have good temporal coverage for all data. 
    ### Given with what we have, we either have to gap-fill missing data (data assimilation), or avoid emphasizing temporal variation. 
    
    
    ### conditional output
    if (stat.model == "no_interaction_with_pretreatment") {
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