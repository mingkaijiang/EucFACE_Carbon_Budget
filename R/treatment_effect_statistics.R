treatment_effect_statistics <- function(inDF) {
    library(doBy)
    library(lubridate)
    library(lme4)
    library(LMERConvenienceFunctions)
    library(car)
    
    ### Assign amb and ele factor
    for (i in (1:length(inDF$Ring))) {
        if (inDF$Ring[i]==2|inDF$Ring[i]==3|inDF$Ring[i]==6) {
            inDF$trt[i] <- "amb"
        } else {
            inDF$trt[i] <- "ele"
        }
    }
    
    inDF$trt <- as.factor(inDF$trt)
    inDF$yr <- year(inDF$Date)
    inDF$Ring <- as.factor(inDF$Ring)
    inDF$Datef <- as.factor(inDF$Date)
    inDF$total <- with(inDF,twig_flux+bark_flux+seed_flux+leaf_flux)
    
    # some plots to check data
    with(inDF,plot(Date,leaf_flux,col=trt))
    with(inDF,plot(Date,twig_flux,col=trt))
    with(inDF,plot(Date,bark_flux,col=trt))
    with(inDF,plot(Date,seed_flux,col=Ring))
    with(inDF,plot(Date,total,col=trt))
    trtbydate <- summaryBy(twig_flux+bark_flux+seed_flux+leaf_flux+total~trt+Date,data=inDF, FUN=mean, rm.na=T)
    with(trtbydate,plot(Date,total.mean,col=trt))
    
    # dataframe with annual totals in g m-2 yr-1
    totals <- summaryBy(twig_flux+bark_flux+seed_flux+leaf_flux+total~trt+Ring+yr,data=inDF,FUN=sum)
    with(totals,plot(yr,total.sum,col=trt,ylim=c(0,20000),
                     xlab="Year",ylab="Total (mg m-2 d-1)"))
    with(totals,plot(yr,leaf_flux.sum,col=trt,ylim=c(0,10000),
                     xlab="Year",ylab="Leaf Litter (mg m-2 d-1)"))
    
    # check means
    avs <- summaryBy(leaf_flux.sum+twig_flux.sum+bark_flux.sum+seed_flux.sum+total.sum~trt+yr,data=totals,FUN=mean)
    
    # Analyse twig litter
    # Treatment interacting with date, Ring random factor
    modelt1 <- lmer(twig_flux~trt*Datef + (1|Ring),data=inDF)
    Anova(modelt1)
    # try log effects to get 95% CI
    modelt2 <- lmer(log(twig_flux)~trt+Datef+(1|Ring),data=inDF)
    Anova(modelt2)
    exp(coef(modelt2)[[1]][1,2])
    exp(confint(modelt2,"trtele"))
    # old CI = (0.59, 1.34 - quite wide)
    # new CI = (0.65, 1.24 - narrower)
    
    # Analyse twig + bark litter
    # Treatment interacting with date, Ring random factor
    modelt1 <- lmer(twig_flux+bark_flux~trt*Datef + (1|Ring),data=inDF)
    Anova(modelt1)
    # try log effects to get 95% CI
    modelt2 <- lmer(log(twig_flux+bark_flux)~trt+Datef+(1|Ring),data=inDF)
    Anova(modelt2)
    exp(coef(modelt2)[[1]][1,2])
    exp(confint(modelt2,"trtele"))
    # new CI = (0.74, 1.26 - narrower)
    
    
    # Analyse leaf litter
    modelleaf1 <- lmer(log(leaf_flux)~trt+Datef+(1|Ring),data=inDF)
    Anova(modelleaf1)
    exp(coef(modelleaf1)[[1]][1,2])
    exp(confint(modelleaf1,"trtele"))
    # OLD CI = (0.69, 1.04) - narrow, and rules out increases with CO2
    # NEW CI = (0.74, 1.04)
    
    # Analyse total litter
    modeltot2 <- lmer(total~trt+Datef+(1|Ring),data=inDF)
    Anova(modeltot2)
    confint(modeltot2,"trtele")
    modeltot1 <- lmer(log(total)~trt+Datef+(1|Ring),data=inDF)
    Anova(modeltot1)
    exp(confint(modeltot1,"trtele"))
    # OLD CI = (0.72, 1.12) - rules out values > 12% increase
    # NEW CI = (0.76, 1.12) - slightly narrower
    
    # Analyse annual values
    modeltot1 <- lmer(log(leaf_flux.sum)~trt+as.factor(yr)+(1|Ring),data=subset(totals,yr>2012))
    Anova(modeltot1)
    exp(confint(modeltot1,"trtele"))
    # New CI = (0.69, 1.07) - definitely low
    
    modeltot1 <- lmer(log(bark_flux.sum+twig_flux.sum)~trt+as.factor(yr)+(1|Ring),data=subset(totals,yr>2012))
    Anova(modeltot1)
    exp(confint(modeltot1,"trtele"))
    # New CI = (0.79, 1.24) - could be anything
    
    modeltot1 <- lmer(log(total.sum)~trt+as.factor(yr)+(1|Ring),data=subset(totals,yr>2012))
    Anova(modeltot1)
    exp(confint(modeltot1,"trtele"))
    # Old CI = (0.71, 1.14) - similar to analysis with monthly values
    # New CI = (0.73, 1.14) - marginally narrower
    
    # General conclusion
    # ratio of elevated to ambient total litter is significantly less than 1.14
    # litter production has not increased by 19%
    
    ## Combine litter with biomass growth to get ANPP
    agg.stk$yr <- c(rep(2013,6),rep(2014,6),rep(2015,6))
    all <- merge(agg.stk,subset(totals, yr > 2012))
    # total.sum is in g m-2
    # values is in kg ring-1 - converting to growth, in g m-2
    all$growth <- all$values * 1000 / pi / (12.5^2) 
    all$anpp <- all$total.sum + all$growth  # litterfall + biomass growth
    modelANPP <- lmer(log(anpp)~trt+yr+(1|Ring),data=all)
    Anova(modelANPP)
    exp(coef(modelANPP)[[1]][1,2])   #average effect size
    exp(confint(modelANPP,"trtele"))   #95% CI
    
    for (i in 1:length(all$Ring)) {
        all$initd[i] <- initdiams[all$Ring[i]]
    }
    with(all,plot(initd,total.sum,col=trt))
    with(subset(all,yr==2013),plot(initd,LEAF.sum,col=trt))
    head(all)
}