treatment_effect_abs_statistics <- function(inDF, var.cond, var.col, date.as.factor) {
    
    #### compare treatment effect of inDF variable
    #### var.cond: == flux 
    ####           == pool
    #### var.col: which variable column to extract data points
    #### Compute confidence interval of the absolute values
    #### For fluxes, time should always be a factor
    #### For pools that increment over time, time doesn't have to be a factor
    
    require(lmerTest)
    require(pbkrtest)
    
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
    
    if (var.cond == "flux") {
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
                tmp <- with(inDF[inDF$Ring == i & inDF$Yr == j, ],
                              sum(Value*ndays, na.rm=T)/sum(ndays, na.rm=T)) * 365 / 1000 
                tDF[tDF$Ring == i & tDF$Yr == j, "Value"] <- tmp
            }
        }

        ### Analyse the variable model
        ## 1. with interaction between date and treatment
        #modelt5 <- lmer(Value~Trt*Yr + (1|Ring),data=tDF)
        ## 2. without interaction between date and treatment
        #modelt6 <- lmer(Value~Trt+Yr + (1|Ring),data=tDF)
        ## 3. with interaction, date as factor
        modelt7 <- lmer(Value~Trt*Yrf + (1|Ring),data=tDF)
        ## 4. without interaction, date as factor
        modelt8 <- lmer(Value~Trt+Yrf + (1|Ring),data=tDF)
        
        ### Check interaction significance to non-interaction model
        interaction.p4 <- KRmodcomp(modelt7, modelt8)
        
        ### If interaction model makes no difference, report non-interaction model
        if(interaction.p4$stats$p.value <= 0.05) {
            
            ### Non-interactive model
            int.m <- "interative"
                
            ### anova
            m.anova <- Anova(modelt7, test="F")
            
            ### Check ele - amb diff
            summ <- summary(glht(modelt7, linfct = mcp(Trt = "Tukey")))
            
            ### average effect size
            eff.size <- coef(modelt7)[[1]][1,2]
            
            ### confidence interval 
            ### if reporting confidence interval for annual,
            ### need to actually calculate annual fluxes!!!
            ### Also, confidence interval is too large. It doesn't mean anything now!
            eff.conf <- confint(modelt7,"Trtele")
            
            ### all fluxes should consider date as a factor
            return(list(int.state=int.m,
                        mod = modelt7, 
                        anova = m.anova,
                        diff = summ,
                        eff = eff.size,
                        conf = eff.conf))
            
        } else {
            ### Interactive model
            int.m <- "non-interative"
            
            ### anova
            m.anova <- Anova(modelt8, test="F")
            
            ### Check ele - amb diff
            summ <- summary(glht(modelt8, linfct = mcp(Trt = "Tukey")))
            
            ### average effect size
            eff.size <- coef(modelt8)[[1]][1,2]
            
            ### confidence interval 
            ### if reporting confidence interval for annual,
            ### need to actually calculate annual fluxes!!!
            ### Also, confidence interval is too large. It doesn't mean anything now!
            eff.conf <- confint(modelt8,"Trtele")
            
            ### all fluxes should consider date as a factor
            return(list(int.state=int.m,
                        mod = modelt8, 
                        anova = m.anova,
                        diff = summ,
                        eff = eff.size,
                        conf = eff.conf))
        }
        
    } else if (var.cond == "pool") {
        #### Analyse the variable model
        ###------ Treatment interacting with date, or not
        ###------ Ring random factor
        ###------ Unit g m-2
        
        if (date.as.factor==T) {
            ### mixed linear model
            ## 3. with interaction, date as factor
            modelt1 <- lmer(Value~Trt*Datef + (1|Ring),data=inDF)
            ## 4. without interaction, date as factor
            modelt2 <- lmer(Value~Trt+Datef + (1|Ring),data=inDF)
            
        } else {
            ### mixed linear model
            ## 1. with interaction between date and treatment
            modelt1 <- lmer(Value~Trt*Date + (1|Ring),data=inDF)
            ## 2. without interaction between date and treatment
            modelt2 <- lmer(Value~Trt+Date + (1|Ring),data=inDF)
        }
        
        ### inspect individual coefficient p-values
        ## if p>0.05, interaction is not significant
        interaction.p1 <- KRmodcomp(modelt1, modelt2)
        
        #anova(modelt1, modelt2)
        
        if(interaction.p1$stats$p.value > 0.05) {
            
            ### Non-interactive model
            int.m <- "non-interative"
            
            ### anova
            ### Type I, II, and III explained:
            ### Type I tests sequential effect of A and B
            ### Type II tests for each main effect after the other main effect
            ### Type III tests for the presence of  a main effect after the other main effect and the interaction
            ### If the interaction is not significance, type II is more powerful
            m.anova <- Anova(modelt2, test="F")
            
            ### Check ele - amb diff
            summ <- summary(glht(modelt2, linfct = mcp(Trt = "Tukey")))
            
            ### average effect size
            eff.size <- coef(modelt2)[[1]][1,2]
            
            ### confidence interval
            eff.conf <- confint(modelt2,"Trtele")
            
            return(list(int.state=int.m,
                        mod = modelt2, 
                        anova = m.anova,
                        diff = summ,
                        eff = eff.size,
                        conf = eff.conf))
        } else {
            ### Interactive model
            int.m <- "interative"
            
            ### anova
            ### Type I, II, and III explained:
            ### Type I tests sequential effect of A and B
            ### Type II tests for each main effect after the other main effect
            ### Type III tests for the presence of  a main effect after the other main effect and the interaction
            ### If the interaction is not significance, type II is more powerful
            m.anova <- Anova(modelt1, test="F")
            
            ### Check ele - amb diff
            summ <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))

            ### average effect size
            eff.size <- coef(modelt1)[[1]][1,2]

            ### confidence interval
            eff.conf <- confint(modelt1,"Trtele")

            return(list(int.state=int.m,
                        mod = modelt1, 
                        anova = m.anova,
                        diff = summ,
                        eff = eff.size,
                        conf = eff.conf))
        }
    }  # flux or pool if statement
}