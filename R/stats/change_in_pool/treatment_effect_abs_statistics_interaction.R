treatment_effect_abs_statistics <- function(inDF, var.col, date.as.factor) {
    
    #### compare treatment effect of inDF variable
    #### var.cond: == flux 
    ####           == pool
    #### var.col: which variable column to extract data points
    #### Compute confidence interval of the absolute values
    #### For fluxes, time should always be a factor
    #### For pools that increment over time, time doesn't have to be a factor
    #### For fluxes, calculate annual sum
    #### A few points to keep in mind:
    ####       Fluxes as daily values could result in different model statistics;
    ####       The model assumption that ring as a random variable could be removed;
    ####       Within ring variability has not been considered here, as we are reporting one value per ring per date only;
    ####       Some fluxes (e.g. CH4) have both positive and negative values - effect size using log is not a good idea;
    ####       Some variables don't have a balanced data input (e.g. SLA);
    ####       Soil C pool should possibly consider time not as a factor;

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
    

    #### Analyse the variable model
    ###------ Treatment interacting with date, or not
    ###------ Ring random factor

    if (date.as.factor==T) {
        ### mixed linear model
        modelt <- lmer(Value~Trt*Datef + (1|Ring),data=inDF)
        
    } else {
        ### mixed linear model
        modelt <- lmer(Value~Trt*cumulative_ndays + (1|Ring),data=inDF)
    }
    
    ### interactive model
    int.m <- "interative"
    
    ### anova
    ### Type I, II, and III explained:
    ### Type I tests sequential effect of A and B
    ### Type II tests for each main effect after the other main effect
    ### Type III tests for the presence of  a main effect after the other main effect and the interaction
    ### If the interaction is not significance, type II is more powerful
    m.anova <- Anova(modelt, test="F")
    
    ### Check ele - amb diff
    summ <- summary(glht(modelt, linfct = mcp(Trt = "Tukey")))
    
    ### average effect size
    eff.size <- coef(modelt)[[1]][1,2]
    
    ### confidence interval
    eff.conf <- confint(modelt,"Trtele")
    
    return(list(int.state=int.m,
                mod = modelt, 
                anova = m.anova,
                diff = summ,
                eff = eff.size,
                conf = eff.conf))
}