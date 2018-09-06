make_ra_root_treatment_abs_effect_statistics <- function(inDF, var.cond, 
                                                         var.col, date.as.factor,
                                                         stat.model) {

    ### Pass in covariate values (assuming 1 value for each ring)
    covDF <- summaryBy(soil_p_g_m2~Ring, data=soil_p_pool, FUN=mean, keep.names=T, na.rm=T)
    covDF$Ring <- as.numeric(covDF$Ring)
    inDF$Ring <- as.numeric(inDF$Ring)
    
    for (i in 1:6) {
        inDF$Cov[inDF$Ring==i] <- covDF$soil_p_g_m2[covDF$Ring==i]
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
    tDF <- summaryBy(Value+Cov~Trt+Ring+Date,data=inDF,FUN=sum, keep.names=T)
    tDF$Datef <- as.factor(tDF$Date)
    
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
    
    ### conditional output
    if (stat.model == "no_interaction_with_pretreatment") {
        out <- list(int.state=int.m1,
                    mod = modelt1, 
                    anova = m1.anova,
                    diff = summ1,
                    eff = eff.size1,
                    conf = eff.conf1)
    } else if (stat.model == "interaction_with_pretreatment") {
        out <- list(int.state=int.m2,
                    mod = modelt2, 
                    anova = m2.anova,
                    diff = summ2,
                    eff = eff.size2,
                    conf = eff.conf2)
    } else if (stat.model == "no_interaction_with_pretreatment_and_covariate") {
        out <- list(int.state=int.m3,
                    mod = modelt3, 
                    anova = m3.anova,
                    diff = summ3,
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

    return(out)
}
