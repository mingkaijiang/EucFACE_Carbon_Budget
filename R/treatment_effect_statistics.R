treatment_effect_statistics <- function(inDF, var.cond, var.col) {
    
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
    
    if (var.cond == "flux") {

        ### some plots to check data
        # with(inDF,plot(Date,Value,col=Trt))
        
        ### average treatment for each date
        trtbydate <- summaryBy(Value~Trt+Date,data=inDF, FUN=mean, rm.na=T)
        # with(trtbydate,plot(Date,Value.mean,col=Trt))
        
        ### dataframe with annual totals in mg m-2 yr-1
        ### This result is not annual total, because not everyday has a data point!
        totals <- summaryBy(Value~Trt+Ring+Yr,data=inDF,FUN=sum)
        
        #with(totals,plot(Yr,Value.sum,col=Trt,
        #                 xlab="Year",ylab="Total (mg m-2 yr-1)"))
        
        ### check means
        #avs <- summaryBy(Value.sum~Trt+Yr,data=totals,FUN=mean)
        
        ### Analyse the variable model
        ## Treatment interacting with date, Ring random factor
        #modelt1 <- lmer(Value~Trt*Datef + (1|Ring),data=inDF)
        #m1.anova <- Anova(modelt1)
        
        ## Check ele - amb diff
        #summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
        
        ## average effect size
        #eff.size1 <- exp(coef(modelt1)[[1]][1,2])
        
        ## confidence interval
        #eff.conf1 <- exp(confint(modelt1,"Trtele"))
        
        ## try log effects to get 95% CI
        modelt2 <- lmer(log(Value)~Trt+Datef+(1|Ring),data=inDF)
        m2.anova <- Anova(modelt2)

        ## Check ele - amb diff
        summ2 <- summary(glht(modelt2, linfct = mcp(Trt = "Tukey")))

        ## average effect size
        eff.size2 <- exp(coef(modelt2)[[1]][1,2])
        
        ## confidence interval
        eff.conf2 <- exp(confint(modelt2,"Trtele"))
        
        ## Analyse annual values
        #modeltot1 <- lmer(log(Value.sum)~Trt+Yr+(1|Ring),data=totals)
        #m3.anova <- Anova(modeltot1)
        
        ## average effect size
        #eff.size3 <- exp(coef(modeltot1)[[1]][1,2])
        
        ## confidence interval
        #eff.conf3 <- exp(confint(modeltot1,"Trtele"))

        return(list(mod = modelt2, 
                    anova = m2.anova,
                    diff = summ2,
                    eff = eff.size2,
                    conf = eff.conf2))
        
    } else if (var.cond == "pool") {
        
    }
    
}