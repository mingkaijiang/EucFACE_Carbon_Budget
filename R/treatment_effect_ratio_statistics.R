treatment_effect_ratio_statistics <- function(inDF, var.cond, var.col, date.as.factor) {
    
    #### compare treatment effect of inDF variable
    #### var.cond: == flux 
    ####           == pool
    #### var.col: which variable column to extract data points
    
    ### Assign amb and ele factor
    for (i in (1:length(inDF$Ring))) {
        if (inDF$Ring[i]==2|inDF$Ring[i]==3|inDF$Ring[i]==6) {
            inDF$Trt[i] <- "amb"
        } else {
            inDF$Trt[i] <- "ele"
        }
    }
    
    ### Assign factors
    inDF$Trt <- as.factor(inDF$Trt)
    inDF$Yr <- year(inDF$Date)
    inDF$Ring <- as.factor(inDF$Ring)
    inDF$Datef <- as.factor(inDF$Date)
    
    ### Update variable name so that this function can be used across different variables
    colnames(inDF)[var.col] <- "Value"
    inDF$log_value <- log(inDF$Value)
    inDF$log_value[is.infinite(inDF$log_value)] <- 0
    
    if (var.cond == "flux") {
        #### Analyse the variable model
        ###------ Treatment interacting with date, or not
        ###------ Ring random factor
        ###------ Unit mg m-2 d-1
        
        ## Get year list and ring list
        yr.list <- unique(inDF$Yr)
        tDF <- summaryBy(Value~Trt+Ring+Yr,data=inDF,FUN=sum, keep.names=T)
        tDF$Yrf <- as.factor(tDF$Yr)
        
        ### Loop through data, return annual flux in g m-2 yr-1
        for (i in 1:6) {
            for (j in yr.list) {
                tmp <- with(inDF[inDF$Ring == i & inDF$Yr == j, ],
                            sum(Value*ndays, na.rm=T)/sum(ndays, na.rm=T)) * 365 / 1000 
                tDF[tDF$Ring == i & tDF$Yr == j, "Value"] <- tmp
            }
        }
        
        tDF$log_value <- log(tDF$Value)
        tDF$log_value[is.infinite(tDF$log_value)] <- 0
        
        ### Analyse the variable model
        ## 1. with interaction between date and treatment
        modelt5 <- lmer(log_value~Trt*Yr + (1|Ring),data=tDF)
        ## 2. without interaction between date and treatment
        modelt6 <- lmer(log_value~Trt+Yr + (1|Ring),data=tDF)
        ## 3. with interaction, date as factor
        modelt7 <- lmer(log_value~Trt*Yrf + (1|Ring),data=tDF)
        ## 4. without interaction, date as factor
        modelt8 <- lmer(log_value~Trt+Yrf + (1|Ring),data=tDF)
        
        ### Check interaction significance to non-interaction model
        interaction.p3 <- KRmodcomp(modelt5, modelt6)
        interaction.p4 <- KRmodcomp(modelt7, modelt8)
        
        ### anova
        m5.anova <- Anova(modelt5, test="F")
        m6.anova <- Anova(modelt6, test="F")
        m7.anova <- Anova(modelt7, test="F")
        m8.anova <- Anova(modelt8, test="F")
        
        ### Check ele - amb diff
        summ5 <- summary(glht(modelt5, linfct = mcp(Trt = "Tukey")))
        summ6 <- summary(glht(modelt6, linfct = mcp(Trt = "Tukey")))
        summ7 <- summary(glht(modelt7, linfct = mcp(Trt = "Tukey")))
        summ8 <- summary(glht(modelt8, linfct = mcp(Trt = "Tukey")))
        
        ### average effect size
        eff.size5 <- exp(coef(modelt5)[[1]][1,2])
        eff.size6 <- exp(coef(modelt6)[[1]][1,2])
        eff.size7 <- exp(coef(modelt7)[[1]][1,2])
        eff.size8 <- exp(coef(modelt8)[[1]][1,2])
        
        ### confidence interval 
        ### if reporting confidence interval for annual,
        ### need to actually calculate annual fluxes!!!
        ### Also, confidence interval is too large. It doesn't mean anything now!
        eff.conf5 <- exp(confint(modelt5,"Trtele"))
        eff.conf6 <- exp(confint(modelt6,"Trtele"))
        eff.conf7 <- exp(confint(modelt7,"Trtele"))
        eff.conf8 <- exp(confint(modelt8,"Trtele"))
        
        ### all fluxes should consider date as a factor
        ### hene only need to compare and report best model of models 5-8
        ### Here we need to report the CO2 by date interaction, 
        ### hence reporting model 7
        
        return(list(mod = modelt7, 
                    anova = m7.anova,
                    diff = summ7,
                    eff = eff.size7,
                    conf = eff.conf7))
        
    } else if (var.cond == "pool") {
        #### Analyse the variable model
        ###------ Treatment interacting with date, or not
        ###------ Ring random factor
        ###------ Unit g m-2
        
        ### mixed linear model
        ## 1. with interaction between date and treatment
        modelt1 <- lmer(log_value~Trt*Date + (1|Ring),data=inDF)
        ## 2. without interaction between date and treatment
        modelt2 <- lmer(log_value~Trt+Date + (1|Ring),data=inDF)
        ## 3. with interaction, date as factor
        modelt3 <- lmer(log_value~Trt*Datef + (1|Ring),data=inDF)
        ## 4. without interaction, date as factor
        modelt4 <- lmer(log_value~Trt+Datef + (1|Ring),data=inDF)
        
        ### inspect individual coefficient p-values
        ## if p<0.05, interaction is not significant
        require(lmerTest)
        require(pbkrtest)
        interaction.p1 <- KRmodcomp(modelt1, modelt2)
        interaction.p2 <- KRmodcomp(modelt3, modelt4)
        
        ### AIC - smaller means better model
        ### if p value above tells no difference between models considering interaction and not,
        ### smaller AIC model should be picked.
        mod.aic <- AIC(modelt1, modelt2, modelt3, modelt4)
        
        ### anova
        ### Type I, II, and III explained:
        ### Type I tests sequential effect of A and B
        ### Type II tests for each main effect after the other main effect
        ### Type III tests for the presence of  a main effect after the other main effect and the interaction
        ### If the interaction is not significance, type II is more powerful
        m1.anova <- Anova(modelt1, test="F")
        m2.anova <- Anova(modelt2, test="F")
        m3.anova <- Anova(modelt3, test="F")
        m4.anova <- Anova(modelt4, test="F")
        
        ### Check ele - amb diff
        summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
        summ2 <- summary(glht(modelt2, linfct = mcp(Trt = "Tukey")))
        summ3 <- summary(glht(modelt3, linfct = mcp(Trt = "Tukey")))
        summ4 <- summary(glht(modelt4, linfct = mcp(Trt = "Tukey")))
        
        ### average effect size
        eff.size1 <- exp(coef(modelt1)[[1]][1,2])
        eff.size2 <- exp(coef(modelt2)[[1]][1,2])
        eff.size3 <- exp(coef(modelt3)[[1]][1,2])
        eff.size4 <- exp(coef(modelt4)[[1]][1,2])
        
        ### confidence interval
        eff.conf1 <- exp(confint(modelt1,"Trtele"))
        eff.conf2 <- exp(confint(modelt2,"Trtele"))
        eff.conf3 <- exp(confint(modelt3,"Trtele"))
        eff.conf4 <- exp(confint(modelt4,"Trtele"))
        
        
        if (date.as.factor==T) {
            return(list(mod = modelt3, 
                        anova = m3.anova,
                        diff = summ3,
                        eff = eff.size3,
                        conf = eff.conf3))
        } else {
            return(list(mod = modelt1, 
                        anova = m1.anova,
                        diff = summ1,
                        eff = eff.size1,
                        conf = eff.conf1))
        }
    }  # flux or pool if statement
}