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
        #### Analyse the variable model
        ###------ Treatment interacting with date, or not
        ###------ Ring random factor
        ###------ Unit mg m-2 d-1
        
    #    ### mixed linear model
    #    ## 1. with interaction between date and treatment
    #    modelt1 <- lmer(Value~Trt*Date + (1|Ring),data=inDF)
    #    ## 2. without interaction between date and treatment
    #    modelt2 <- lmer(Value~Trt+Date + (1|Ring),data=inDF)
    #    ## 3. with interaction, date as factor
    #    modelt3 <- lmer(Value~Trt*Datef + (1|Ring),data=inDF)
    #    ## 4. without interaction, date as factor
    #    modelt4 <- lmer(Value~Trt+Datef + (1|Ring),data=inDF)
    #    
    #    ### a sub-sample plot to see if date is a factor
    #    #require(lattice)
    #    #d.list <- unique(inDF$Date)
    #    #subDF <- subset(inDF, Date %in% d.list[1:2])
    #    #bwplot(Value ~ Trt | Datef, data=subDF)
    #    
    #    ### Different way of plotting
    #    #require(sciplot)
    #    #bargraph.CI(Date, Value, Trt, data=subDF, legend=TRUE)
    #    
    #    ### inspect individual coefficient p-values
    #    ## if p<0.05, interaction is not significant
    #    require(lmerTest)
    #    require(pbkrtest)
    #    interaction.p1 <- KRmodcomp(modelt1, modelt2)
    #    interaction.p2 <- KRmodcomp(modelt3, modelt4)
    #    
    #    ### Visual inspection
    #    #require(visreg)
    #    #visreg(modelt1, "Date", by="Trt", overlay=TRUE)
    #    #visreg(modelt2, "Date", by="Trt", overlay=TRUE)
    #    
    #    #visreg(modelt3, "Datef", by="Trt", overlay=TRUE)
    #    #visreg(modelt4, "Datef", by="Trt", overlay=TRUE)
    #    
    #    ### AIC - smaller means better model
    #    ### if p value above tells no difference between models considering interaction and not,
    #    ### smaller AIC model should be picked.
    #    mod.aic <- AIC(modelt1, modelt2, modelt3, modelt4)
    #    
    #    ### Plot fitted against residual
    #    #require(car)
    #    #plot(fitted(modelt1), residuals(modelt1))
    #    #abline(h=0)
    #    #qqPlot(residuals(modelt1))
    #    #
    #    #plot(fitted(modelt2), residuals(modelt2))
    #    #abline(h=0)
    #    #qqPlot(residuals(modelt2))
    #    #
    #    #plot(fitted(modelt3), residuals(modelt3))
    #    #abline(h=0)
    #    #qqPlot(residuals(modelt3))
    #    #
    #    #plot(fitted(modelt4), residuals(modelt4))
    #    #abline(h=0)
    #    #qqPlot(residuals(modelt4))
    #    
    #    ### anova
    #    ### Type I, II, and III explained:
    #    ### Type I tests sequential effect of A and B
    #    ### Type II tests for each main effect after the other main effect
    #    ### Type III tests for the presence of  a main effect after the other main effect and the interaction
    #    ### If the interaction is not significance, type II is more powerful
    #    m1.anova <- Anova(modelt1, test="F")
    #    m2.anova <- Anova(modelt2, test="F")
    #    m3.anova <- Anova(modelt3, test="F")
    #    m4.anova <- Anova(modelt4, test="F")
    #    # m5.anova <- Anova(modelt3, type="III", test="F")
    #    
    #    ### Check ele - amb diff
    #    summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    #    summ2 <- summary(glht(modelt2, linfct = mcp(Trt = "Tukey")))
    #    summ3 <- summary(glht(modelt3, linfct = mcp(Trt = "Tukey")))
    #    summ4 <- summary(glht(modelt4, linfct = mcp(Trt = "Tukey")))
    #    
    #    ### average effect size
    #    eff.size1 <- coef(modelt1)[[1]][1,2]
    #    eff.size2 <- coef(modelt2)[[1]][1,2]
    #    eff.size3 <- coef(modelt3)[[1]][1,2]
    #    eff.size4 <- coef(modelt4)[[1]][1,2]
    #    
    #    ### confidence interval
    #    eff.conf1 <- confint(modelt1,"Trtele")
    #    eff.conf2 <- confint(modelt2,"Trtele")
    #    eff.conf3 <- confint(modelt3,"Trtele")
    #    eff.conf4 <- confint(modelt4,"Trtele")
    #    
    #    ### Generate a range of p-values for individual variables
    #    #library(LMERConvenienceFunctions)
    #    #modelt1.upd <- update(modelt1, REML=FALSE)
    #    #modelt2.upd <- update(modelt2, REML=FALSE)
    #    #modelt3.upd <- update(modelt3, REML=FALSE)
    #    #modelt4.upd <- update(modelt4, REML=FALSE)
    #    #
    #    #p.range1 <- pamer.fnc(modelt1.upd)
    #    #p.range2 <- pamer.fnc(modelt2.upd)
    #    #p.range3 <- pamer.fnc(modelt3.upd)
    #    #p.range4 <- pamer.fnc(modelt4.upd)
    #    
    #    #### dataframe with annual totals in mg m-2 yr-1
    #    ###------ Treatment interacting with date, or not
    #    ###------ Ring random factor
    #    ###------ Unit mg m-2 yr-1
    #    
    #    ### This result is not annual total, because not everyday has a data point!
    #    tDF <- summaryBy(Value~Trt+Ring+Yr,data=inDF,FUN=sum, keep.names=T)
    #    tDF$Yrf <- as.factor(tDF$Yr)
    #    
    #    ### Analyse the variable model
    #    ## 1. with interaction between date and treatment
    #    modelt5 <- lmer(Value~Trt*Yr + (1|Ring),data=tDF)
    #    ## 2. without interaction between date and treatment
    #    modelt6 <- lmer(Value~Trt+Yr + (1|Ring),data=tDF)
    #    ## 3. with interaction, date as factor
    #    modelt7 <- lmer(Value~Trt*Yrf + (1|Ring),data=tDF)
    #    ## 4. without interaction, date as factor
    #    modelt8 <- lmer(Value~Trt+Yrf + (1|Ring),data=tDF)
    #    
    #    ### Check interaction significance to non-interaction model
    #    interaction.p3 <- KRmodcomp(modelt5, modelt6)
    #    interaction.p4 <- KRmodcomp(modelt7, modelt8)
    #    
    #    ### anova
    #    m5.anova <- Anova(modelt5, test="F")
    #    m6.anova <- Anova(modelt6, test="F")
    #    m7.anova <- Anova(modelt7, test="F")
    #    m8.anova <- Anova(modelt8, test="F")
    #    
    #    ### Check ele - amb diff
    #    summ5 <- summary(glht(modelt5, linfct = mcp(Trt = "Tukey")))
    #    summ6 <- summary(glht(modelt6, linfct = mcp(Trt = "Tukey")))
    #    summ7 <- summary(glht(modelt7, linfct = mcp(Trt = "Tukey")))
    #    summ8 <- summary(glht(modelt8, linfct = mcp(Trt = "Tukey")))
    #    
    #    ### average effect size
    #    eff.size5 <- coef(modelt5)[[1]][1,2]
    #    eff.size6 <- coef(modelt6)[[1]][1,2]
    #    eff.size7 <- coef(modelt7)[[1]][1,2]
    #    eff.size8 <- coef(modelt8)[[1]][1,2]
    #    
    #    ### confidence interval 
    #    ### if reporting confidence interval for annual,
    #    ### need to actually calculate annual fluxes!!!
    #    ### Also, confidence interval is too large. It doesn't mean anything now!
    #    eff.conf5 <- confint(modelt5,"Trtele")
    #    eff.conf6 <- confint(modelt6,"Trtele")
    #    eff.conf7 <- confint(modelt7,"Trtele")
    #    eff.conf8 <- confint(modelt8,"Trtele")

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