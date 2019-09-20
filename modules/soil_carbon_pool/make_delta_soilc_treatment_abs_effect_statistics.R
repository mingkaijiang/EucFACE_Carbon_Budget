make_delta_soilc_treatment_abs_effect_statistics <- function(inDF, var.cond, 
                                                             var.col, date.as.factor,
                                                             stat.model, return.outcome) {
    
    ### subset pre-treatment data
    preDF <- subset(inDF, Date=="2012-06-17")
    inDF <- subset(inDF, Date>"2012-06-17")
    
    ### create delta DF
    deltaDF <- make_yearly_delta_pool_function(inDF, var.col)
    
    ### Pass in covariate values (assuming 1 value for each ring)
    cov2 <- lai_variable[lai_variable$Date<="2013-02-06",]
    covDF2 <- summaryBy(lai_variable~Ring, data=cov2, FUN=mean, keep.names=T)
    
    
    for (i in 1:6) {
        deltaDF$PreTrt[deltaDF$Ring==i] <- preDF$soil_carbon_pool[preDF$Ring==i]
        deltaDF$Cov2[deltaDF$Ring==i] <- covDF2$lai_variable[covDF2$Ring==i]
    }
    
    #### Assign amb and ele factor
    deltaDF$Trt[deltaDF$Ring%in%c(2,3,6)] <- "amb"
    deltaDF$Trt[deltaDF$Ring%in%c(1,4,5)] <- "ele"
    
    #### Assign factors
    deltaDF$Trt <- as.factor(deltaDF$Trt)
    deltaDF$Ring <- as.factor(deltaDF$Ring)
    deltaDF$Datef <- as.factor(deltaDF$Date)
    
    ## Get year list and ring list
    tDF <- deltaDF
    
    ### add annual average LAI for each year and ring
    tDF$Yr <- tDF$Date
    
    ### Analyse the variable model
    ## model 1: no interaction, year as factor, ring random factor, include pre-treatment effect
    int.m1 <- "non-interative_with_covariate"
    modelt1 <- lmer(delta~Trt + Datef + Cov2 + (1|Ring),data=tDF)
    
    ## anova
    m1.anova <- Anova(modelt1, test="F")
    
    ## Check ele - amb diff
    summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size1 <- coef(modelt1)[[1]][1,2]
    
    ## confidence interval 
    eff.conf1 <- confint(modelt1,"Trtele")
    
    
    out <- list(int.state=int.m1,
                mod = modelt1, 
                anova = m1.anova,
                diff = summ1,
                eff = eff.size1,
                conf = eff.conf1)
    
    
    ### Predict the model with a standard LAI value
    newDF <- tDF
    newDF$Cov2 <- mean(covDF2$lai_variable)
    
    newDF$predicted <- predict(out$mod, newdata=newDF)
    
    
    if (return.outcome == "model") {
        return(out)
    } else if (return.outcome == "predicted") {
        return(newDF)
    }
}
