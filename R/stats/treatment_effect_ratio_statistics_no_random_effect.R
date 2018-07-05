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

        ## Get year list and ring list
        yr.list <- unique(inDF$Yr)
        tDF <- summaryBy(Value~Trt+Ring+Yr,data=inDF,FUN=sum, keep.names=T)
        tDF$Yrf <- as.factor(tDF$Yr)
        
        ### Loop through data, return annual flux in g m-2 yr-1
        for (i in 1:6) {
            for (j in yr.list) {
                ### averaged over days within a year 
                #tmp <- with(inDF[inDF$Ring == i & inDF$Yr == j, ],
                #            sum(Value*ndays, na.rm=T)/sum(ndays, na.rm=T)) * 365 / 1000
                
                ### summed of all available data within a year
                tmp <- with(inDF[inDF$Ring == i & inDF$Yr == j, ],
                            sum(Value*ndays, na.rm=T)) / 1000 
                tDF[tDF$Ring == i & tDF$Yr == j, "Value"] <- tmp
            }
        }
        
        tDF$log_value <- log(tDF$Value)
        tDF$log_value[is.infinite(tDF$log_value)] <- 0
        
        ### Analyse the variable model
        modelt <- lm(log_value~Trt+Yrf,data=tDF)
        
        ### Non-interactive model
        int.m <- "non-interative"
        
        ### anova
        m.anova <- Anova(modelt, test="F")
        
        ### Check ele - amb diff
        summ <- summary(glht(modelt, linfct = mcp(Trt = "Tukey")))
        
        ### average effect size
        eff.size <- exp(coef(modelt)[[2]])
        
        ### confidence interval 
        eff.conf <- exp(confint(modelt,"Trtele"))
        
        ### all fluxes should consider date as a factor
        return(list(int.state=int.m,
                    mod = modelt, 
                    anova = m.anova,
                    diff = summ,
                    eff = eff.size,
                    conf = eff.conf))
        
        
    } else if (var.cond == "pool") {
        #### Analyse the variable model
        ###------ Treatment interacting with date, or not
        ###------ Ring random factor
        
        inDF$Yrf <- as.factor(inDF$Yr)
        
        if (date.as.factor==T) {
            ### mixed linear model
            modelt <- lm(log_value~Trt+Yrf,data=inDF)
            
        } else {
            ### mixed linear model
            modelt <- lm(log_value~Trt+Yr,data=inDF)
        }
        
        ### Non-interactive model
        int.m <- "non-interative"
        
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
        eff.size <- exp(coef(modelt)[[2]])
        
        ### confidence interval
        eff.conf <- exp(confint(modelt,"Trtele"))
        
        return(list(int.state=int.m,
                    mod = modelt, 
                    anova = m.anova,
                    diff = summ,
                    eff = eff.size,
                    conf = eff.conf))
        
    }  # flux or pool if statement
}