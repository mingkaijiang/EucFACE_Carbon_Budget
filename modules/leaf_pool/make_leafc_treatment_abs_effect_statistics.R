make_leafc_treatment_abs_effect_statistics <- function(inDF, var.cond, 
                                                   var.col, date.as.factor,
                                                   stat.model) {
    
    ### Read initial basal area data
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    f12$ba <- ((f12$X20.09.2012/2)^2) * pi
    baDF <- summaryBy(ba~Ring, data=f12, FUN=sum, na.rm=T, keep.names=T)
    
    ### return in unit of cm2/m2, which is m2 ha-1
    baDF$ba_ground_area <- baDF$ba / ring_area

    ### pass in covariate
    for (i in 1:6) {
        inDF$Cov[inDF$Ring==i] <- baDF$ba_ground_area[covDF$Ring==i]
    }
    
    cov2 <- read.csv("R_other/VOC_met_data.csv")
    cov2$Date <- as.Date(as.character(cov2$DateHour), format="%Y-%m-%d")
    covDF2 <- summaryBy(SM_R1+SM_R2+SM_R3+SM_R4+SM_R5+SM_R6~Date, data=cov2, FUN=mean, keep.names=T, na.rm=T)
    
    for (j in unique(inDF$Date)) {
        inDF$Cov2[inDF$Ring==1&inDF$Date==j] <- covDF2$SM_R1[covDF2$Date==j]
        inDF$Cov2[inDF$Ring==2&inDF$Date==j] <- covDF2$SM_R2[covDF2$Date==j]
        inDF$Cov2[inDF$Ring==3&inDF$Date==j] <- covDF2$SM_R3[covDF2$Date==j]
        inDF$Cov2[inDF$Ring==4&inDF$Date==j] <- covDF2$SM_R4[covDF2$Date==j]
        inDF$Cov2[inDF$Ring==5&inDF$Date==j] <- covDF2$SM_R5[covDF2$Date==j]
        inDF$Cov2[inDF$Ring==6&inDF$Date==j] <- covDF2$SM_R6[covDF2$Date==j]
        
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
    inDF$Ring <- as.factor(inDF$Ring)
    inDF$Datef <- as.factor(inDF$Date)

    #### Update variable name so that this function can be used across different variables
    colnames(inDF)[var.col] <- "Value"
    
    #### dataframe 
    tDF <- inDF
    
    ### Add psyllid attack event
    tDF$Psyllid <- "Before"
    tDF$Psyllid[tDF$Yr >= 2016] <- "After"

    ### Analyse the variable model
    ## model 1: no interaction, year as factor, ring random factor, include covariate
    int.m1 <- "non-interative_with_covariate"
    modelt1 <- lmer(Value~Trt + Datef + Cov + (1|Ring),data=tDF)
    
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
    int.m2 <- "interative_with_covariate"
    modelt2 <- lmer(Value~Trt*Datef+Cov + (1|Ring),data=tDF)
    
    ## anova
    m2.anova <- Anova(modelt2, test="F")
    
    ## Check ele - amb diff
    summ2 <- summary(glht(modelt2, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size2 <- coef(modelt2)[[1]][1,2]
    
    ## confidence interval 
    eff.conf2 <- confint(modelt2,"Trtele")
    
    ### Analyse the variable model
    ## model 3: no interaction, year as factor, paired t-test
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
    }  else if (stat.model == "paired_t_test") {
        out <- list(int.state=int.m4,
                    mod = modelt4, 
                    anova = NA,
                    diff = NA,
                    eff = eff.size4,
                    conf = eff.conf4)
    }

    return(out)
}
