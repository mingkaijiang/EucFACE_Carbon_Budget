
library(lubridate)
# spatial signal?
n.ring<-6
n.tree<-3
n.leaf<-5

trt<-factor(rep(c('elevated','ambient'),each=n.ring/2*n.tree*n.leaf))
ring<-factor(rep(1:n.ring,each=n.tree*n.leaf))
tree<-rep(rep(1:n.tree,each=n.leaf),n.ring)
tree<-tree+rep(1:6*100,each=n.tree*n.leaf)
tree<-factor(tree)

effect.size<-0.15
effect<-c(rep(1+effect.size,n.ring/2*n.tree*n.leaf),rep(1,n.ring/2*n.tree*n.leaf))

rand<-rnorm(n.ring*n.tree*n.leaf,mean=0,sd=0.2)
ranef.tree<-rep(rnorm(n.ring*n.tree,mean=0,sd=0.2),each=n.leaf)

response<-ranef.tree+effect+rand

mod.alt<-lmer(response~trt+(1|tree),REML=F)
mod.nul<-lmer(response~1+(1|tree),REML=F)

tree.sd<-attr(VarCorr(mod.alt)[['tree']],'stddev')[['(Intercept)']]
resid.sd<-attr(VarCorr(mod.alt),'sc')
int.est<-attr(summary(mod.alt),'coefs')['(Intercept)','Estimate']
trt.est<-attr(summary(mod.alt),'coefs')['trtelevated','Estimate']
p.val<-anova(mod.nul,mod.alt)[['Pr(>Chisq)']][2]

out<-cbind(effect.size,n.ring,n.tree,n.leaf,p.val,int.est,trt.est,ranef.tree.sd,tree.sd,rand.sd,resid.sd)





cbind(trt,ring,tree,ranef.tree,rand,response)
plot(response~trt)
#bwplot(response~tree|trt)






## test run ##

source('eucfacePower_v1.R')
for(i in 1:1000) eucface.power('test.txt',0,5,3,0.173,0.160)

test<-read.table('test.txt',sep='\t')
names(test)<-c('effect.size','n.ring','n.tree','n.leaf','p.val','int.est','trt.est','ranef.tree.sd','tree.sd','rand.sd','resid.sd')

mean(test$p.val<0.05)

summary(test)


## real run -- foliar N ##

library(lme4)
library(AICcmodavg)

mod1.ringtree<-lmer(percentN~1+(1|Ring/Tree),data=Nfull)
mod1.ring<-lmer(percentN~1+(1|Ring),data=Nfull)
mod1.tree<-lmer(percentN~1+(1|Tree),data=Nfull)
anova(mod1.ringtree,mod1.ring,mod1.tree)
AICc(mod1.ringtree); AICc(mod1.tree); AICc(mod1.ring); AICc(lm(percentN~1,data=Nfull))
qqnorm(resid(mod1.tree)); qqline(resid(mod1.tree))

# > anova(mod1.ringtree,mod1.ring,mod1.tree)
# refitting model(s) with ML (instead of REML)
# Data: Nfull
# Models:
# mod1.ring: percentN ~ 1 + (1 | Ring)
# mod1.tree: percentN ~ 1 + (1 | Tree)
# mod1.ringtree: percentN ~ 1 + (1 | Ring/Tree)
#               Df     AIC     BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# mod1.ring      3  7.0149 14.1983 -0.5075   1.0149                             
# mod1.tree      3 -1.2549  5.9285  3.6274  -7.2549 8.2698      0     <2e-16 ***
# mod1.ringtree  4  0.6210 10.1988  3.6895  -7.3790 0.1241      1     0.7246    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > AICc(mod1.ringtree); AICc(mod1.tree); AICc(mod1.ring); AICc(lm(percentN~1,data=Nfull))
# [1] 6.108137
# [1] 4.305651
# [1] 12.70234
# [1] 5.168781
# > 
# > summary(mod1.ring)
# Linear mixed model fit by REML ['lmerMod']
# Formula: percentN ~ 1 + (1 | Ring)
#    Data: Nfull
# 
# REML criterion at convergence: 6.4
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -1.96436 -0.82970 -0.06238  0.67229  2.18244 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Ring     (Intercept) 0.00000  0.000   
#  Residual             0.06003  0.245   
# Number of obs: 81, groups: Ring, 6
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  1.72928    0.02722   63.52
# > summary(mod1.tree)
# Linear mixed model fit by REML ['lmerMod']
# Formula: percentN ~ 1 + (1 | Tree)
#    Data: Nfull
# 
# REML criterion at convergence: -2
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -1.39178 -0.52393 -0.03647  0.44052  1.54804 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Tree     (Intercept) 0.03505  0.1872  
#  Residual             0.02374  0.1541  
# Number of obs: 81, groups: Tree, 66
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  1.70891    0.02902   58.88
# > 

mod2.ringtree<-lmer(percentN~1+(1|Ring/Tree),data=GEdata)
mod2.ring<-lmer(percentN~1+(1|Ring),data=GEdata)
mod2.tree<-lmer(percentN~1+(1|Tree),data=GEdata)
anova(mod2.ringtree,mod2.ring,mod2.tree)
AICc(mod2.ringtree); AICc(mod2.tree); AICc(mod2.ring); AICc(lm(percentN~1,data=GEdata))
qqnorm(resid(mod2.tree)); qqline(resid(mod2.tree))

# > anova(mod2.ringtree,mod2.ring,mod2.tree)
# refitting model(s) with ML (instead of REML)
# Data: GEdata
# Models:
# mod2.ring: percentN ~ 1 + (1 | Ring)
# mod2.tree: percentN ~ 1 + (1 | Tree)
# mod2.ringtree: percentN ~ 1 + (1 | Ring/Tree)
#               Df      AIC    BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# mod2.ring      3  2.94526 7.3425 1.5274  -3.0547                             
# mod2.tree      3 -1.87427 2.5229 3.9371  -7.8743 4.8195      0     <2e-16 ***
# mod2.ringtree  4  0.12573 5.9887 3.9371  -7.8743 0.0000      1          1    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > AICc(mod2.ringtree); AICc(mod2.tree); AICc(mod2.ring); AICc(lm(percentN~1,data=GEdata))
# [1] 5.759782
# [1] 3.135444
# [1] 8.347811
# [1] 1.359055
# > 
# > summary(mod2.ring)
# Linear mixed model fit by REML ['lmerMod']
# Formula: percentN ~ 1 + (1 | Ring)
#    Data: GEdata
# 
# REML criterion at convergence: 1.5
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.7475 -0.6852 -0.1945  0.6886  1.7851 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Ring     (Intercept) 0.00000  0.0000  
#  Residual             0.05494  0.2344  
# Number of obs: 32, groups: Ring, 6
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  1.84559    0.04143   44.54
# > 
# > summary(mod2.tree)
# Linear mixed model fit by REML ['lmerMod']
# Formula: percentN ~ 1 + (1 | Tree)
#    Data: GEdata
# 
# REML criterion at convergence: -3.7
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.5720 -0.4241 -0.1393  0.6891  1.3002 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Tree     (Intercept) 0.02984  0.1727  
#  Residual             0.02564  0.1601  
# Number of obs: 32, groups: Tree, 17
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  1.84044    0.05079   36.24
# > 


source('eucfacePower_v1.R')

effect.size<-seq(0,0.3,0.02)
n.tree<-1:10
n.leaf<-1:10

length(effect.size)*length(n.tree)*length(n.leaf)*1000
cat(length(effect.size)*length(n.tree)*length(n.leaf)*1000/10/60/60,'hours')

for(i in 1:length(effect.size)) {
  for(j in 1:length(n.tree)) {
    for(k in 1:length(n.leaf)) {   ###FIX RING.SD, TREE.SD###
      for(l in 1:1000) eucface.power('power_percentN_20121012.txt',effect.size[i],n.tree[j],n.leaf[k],ranef.tree.sd=0.173,rand.sd=0.160,intercept=1.840)
#      for(l in 1:1000) eucface.power('power_percentN_20120930.txt',effect.size[i],n.tree[j],n.leaf[k],0.173,0.160)  # run with v1, didn't account for intercept
    }
  }
}

data<-read.table('power_percentN_20121011.txt',sep='\t')
names(data)<-c('effect.size','n.ring','n.tree','n.leaf','p.val','int.est','trt.est','ranef.ring.sd','ring.sd','ranef.tree.sd','tree.sd','rand.sd','resid.sd','converged')

#with(data,aggregate(p.val,list(effect.size),function(x) mean(x<0.05)))
data.agg<-with(data[data$converged==T,],aggregate(p.val,list(effect.size=effect.size,n.tree=n.tree,n.leaf=n.leaf),function(x) mean(x<0.05)))
names(data.agg)[4]<-'power'
#data.agg<-with(data,aggregate(p.val,list(effect.size,n.tree,n.leaf),function(x) mean(x<0.06786)))

data$signif<-ifelse(data$p.val<0.05,1,0)
mod1<-glm(signif~effect.size*n.tree*n.leaf,data=data,family=binomial)
summary(mod1)
mod2<-lm(p.val~effect.size*n.tree*n.leaf,data=data)
summary(mod2)

xyplot(power~effect.size|n.tree:n.leaf,data=data.agg)

redbluefun<-colorRampPalette(c('red','blue'))
pdf('power_percentN.pdf',height=20,width=20)
par(mfrow=c(3,3))
for(i in 1) {
  plot(power~n.tree,data=data.agg[which(data.agg$effect.size==effect.size[i] & data.agg$n.leaf==n.leaf[1]),],type='l',ylim=c(0,1),col=redbluefun(10)[10],las=1,lwd=3,xlab='# trees per ring')
  title(paste('effect size =',effect.size[i]))
  legend('topleft',legend=rev(unique(n.leaf)),title='# leaves per tree',cex=1.2,lwd=3,col=redbluefun(10),bty='n')
  abline(h=0.8,lty='dotted')
  for(j in 2:10) lines(power~n.tree,data=data.agg[which(data.agg$effect.size==effect.size[i] & data.agg$n.leaf==n.leaf[j]),],col=redbluefun(10)[10-j],lwd=3)
}
for(i in 2:length(effect.size)) {
  plot(power~n.tree,data=data.agg[which(data.agg$effect.size==effect.size[i] & data.agg$n.leaf==n.leaf[1]),],type='l',ylim=c(0,1),col=redbluefun(10)[10],las=1,lwd=3,xlab='# trees per ring')
  title(paste('effect size =',effect.size[i]))
  abline(h=0.8,lty='dotted')
  for(j in 2:10) lines(power~n.tree,data=data.agg[which(data.agg$effect.size==effect.size[i] & data.agg$n.leaf==n.leaf[j]),],col=redbluefun(10)[10-j],lwd=3)
}
dev.off()


# using actual random effects estimates

source('eucfacePower_v3.R')

effect.size<-seq(0,0.3,0.02)
n.tree<-1:10
n.leaf<-1:10
n.ring<-6

for(i in 1:length(effect.size)) {
	for(j in 1:length(n.tree)) {
		for(k in 1:length(n.leaf)) {
			for(l in 1:1000) {
				const.ranef.tree<-sample(ranef(mod2.tree)$Tree[,1],n.ring*n.tree[j],replace=T)
				eucface.power('power_percentN_20130130.txt',effect.size[i],n.tree[j],n.leaf[k],const.ranef.tree=const.ranef.tree,rand.sd=0.160,intercept=1.840)
				}
		}
	}
}



## real run -- foliar P ##

library(lme4)

# repeated measurements on individual trees are identical
# mod1.ringtree<-lmer(percentP~1+(1|Ring/Tree),data=Nfull)
mod1.ring<-lmer(percentP~1+(1|Ring),data=Nfull)
# mod1.tree<-lmer(percentP~1+(1|Tree),data=Nfull)
# anova(mod1.ringtree,mod1.ring,mod1.tree)
# AICc(mod1.ringtree); AICc(mod1.tree); AICc(mod1.ring); AICc(lm(percentP~1,data=Nfull))
AICc(mod1.ring); AICc(lm(percentP~1,data=Nfull))
qqnorm(resid(mod1.ring)); qqline(resid(mod1.ring))

# > AICc(mod1.ring); AICc(lm(percentP~1,data=Nfull))
# [1] -421.8397
# [1] -432.7233
# > 
# > summary(mod1.ring)
# Linear mixed model fit by REML ['lmerMod']
# Formula: percentP ~ 1 + (1 | Ring)
#    Data: Nfull
# 
# REML criterion at convergence: -428.2
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -1.9754 -0.8224  0.0384  0.7740  2.6208 
# 
# Random effects:
#  Groups   Name        Variance  Std.Dev.
#  Ring     (Intercept) 1.771e-05 0.004208
#  Residual             1.350e-04 0.011617
# Number of obs: 73, groups: Ring, 6
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept) 0.082730   0.002194   37.71
# > 

mod2.ringtree<-lmer(percentP~1+(1|Ring/Tree),data=GEdata)
mod2.ring<-lmer(percentP~1+(1|Ring),data=GEdata)
mod2.tree<-lmer(percentP~1+(1|Tree),data=GEdata)
anova(mod2.ringtree,mod2.ring,mod2.tree)
AICc(mod2.ringtree); AICc(mod2.tree); AICc(mod2.ring); AICc(lm(percentP~1,data=GEdata))
qqnorm(resid(mod2.tree)); qqline(resid(mod2.tree))

# > anova(mod2.ringtree,mod2.ring,mod2.tree)
# refitting model(s) with ML (instead of REML)
# Data: GEdata
# Models:
# mod2.ring: percentP ~ 1 + (1 | Ring)
# mod2.tree: percentP ~ 1 + (1 | Tree)
# mod2.ringtree: percentP ~ 1 + (1 | Ring/Tree)
#               Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# mod2.ring      3 -120.47 -117.49 63.237  -126.47                             
# mod2.tree      3 -120.60 -117.61 63.300  -126.60 0.1267      0     <2e-16 ***
# mod2.ringtree  4 -118.60 -114.62 63.300  -126.60 0.0000      1          1    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > AICc(mod2.ringtree); AICc(mod2.tree); AICc(mod2.ring); AICc(lm(percentP~1,data=GEdata))
# [1] -105.7112
# [1] -108.8548
# [1] -108.7761
# [1] -121.7676
# > 
# > summary(mod2.ringtree)
# Linear mixed model fit by REML ['lmerMod']
# Formula: percentP ~ 1 + (1 | Ring/Tree)
#    Data: GEdata
# 
# REML criterion at convergence: -116.4
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -1.42101 -0.50336 -0.01806  0.54134  1.34278 
# 
# Random effects:
#  Groups    Name        Variance  Std.Dev.
#  Tree:Ring (Intercept) 1.908e-05 0.004368
#  Ring      (Intercept) 4.860e-06 0.002205
#  Residual              8.732e-05 0.009344
# Number of obs: 20, groups: Tree:Ring, 17; Ring, 6
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept) 0.085130   0.002534   33.59
# > 



## real run -- photosynthesis ##

library(lme4)

mod1.ringtree<-lmer(Anet_390~1+(1|Ring/Tree),data=GEdata)
mod1.ring<-lmer(Anet_390~1+(1|Ring),data=GEdata)
mod1.tree<-lmer(Anet_390~1+(1|Tree),data=GEdata)
anova(mod1.ringtree,mod1.ring,mod1.tree)
AICc(mod1.ringtree); AICc(mod1.ring); AICc(mod1.tree); AICc(lm(Anet_390~1,data=GEdata))
qqnorm(resid(mod1.ring)); qqline(resid(mod1.ring))

# > anova(mod1.ringtree,mod1.ring,mod1.tree)
# refitting model(s) with ML (instead of REML)
# Data: GEdata
# Models:
# mod1.ring: Anet_390 ~ 1 + (1 | Ring)
# mod1.tree: Anet_390 ~ 1 + (1 | Tree)
# mod1.ringtree: Anet_390 ~ 1 + (1 | Ring/Tree)
#               Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# mod1.ring      3 162.64 167.04 -78.322   156.64                           
# mod1.tree      3 167.72 172.11 -80.858   161.72 0.0000      0    1.00000  
# mod1.ringtree  4 164.64 170.51 -78.322   156.64 5.0735      1    0.02429 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > AICc(mod1.ringtree); AICc(mod1.ring); AICc(mod1.tree); AICc(lm(Anet_390~1,data=GEdata))
# [1] 164.5063
# [1] 161.8819
# [1] 167.7435
# [1] 166.6328
# > 
# > summary(mod1.ring)
# Linear mixed model fit by REML ['lmerMod']
# Formula: Anet_390 ~ 1 + (1 | Ring)
#    Data: GEdata
# 
# REML criterion at convergence: 155
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -2.10061 -0.54491  0.09667  0.52822  2.09020 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Ring     (Intercept) 4.131    2.033   
#  Residual             6.083    2.466   
# Number of obs: 32, groups: Ring, 6
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  16.0248     0.9391   17.06
# > 
# > summary(mod1.tree)
# Linear mixed model fit by REML ['lmerMod']
# Formula: Anet_390 ~ 1 + (1 | Tree)
#    Data: GEdata
# 
# REML criterion at convergence: 160.9
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -2.00072 -0.43444  0.07142  0.52705  2.05432 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Tree     (Intercept) 2.538    1.593   
#  Residual             7.247    2.692   
# Number of obs: 32, groups: Tree, 17
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)  16.0528     0.6156   26.08
# > 
# > ranef(mod1.ring)
# $Ring
#   (Intercept)
# 1  -2.6920181
# 2   0.7562684
# 3   0.4752507
# 4   1.4655386
# 5   1.7198052
# 6  -1.7248448
# 
# > 
#> 
#  

mod1.ring<-lmer(Anet_390~CrownClass+height_m+(1|Ring),data=GEdata[!is.na(GEdata$height),])
mod2a.ring<-lmer(Anet_390~height_m+(1|Ring),data=GEdata[!is.na(GEdata$height),])
mod2b.ring<-lmer(Anet_390~CrownClass+(1|Ring),data=GEdata[!is.na(GEdata$height),])
mod3.ring<-lmer(Anet_390~1+(1|Ring),data=GEdata[!is.na(GEdata$height),])
AIC(mod1.ring,mod2a.ring,mod2b.ring,mod3.ring)

mod2b.ring<-lmer(Anet_390~CrownClass+(1|Ring),data=GEdata)
mod3.ring<-lmer(Anet_390~1+(1|Ring),data=GEdata)
AIC(mod2b.ring,mod3.ring)

#> AIC(mod1.ring,mod2a.ring,mod2b.ring,mod3.ring)
#df      AIC
#mod1.ring   5 143.1509
#mod2a.ring  4 145.8576
#mod2b.ring  4 141.3201
#mod3.ring   3 145.3888
#> 
#> AIC(mod2b.ring,mod3.ring)
#df      AIC
#mod2b.ring  4 157.3997
#mod3.ring   3 161.0248
#> 
#> mod2b.ring
#Linear mixed model fit by REML 
#Formula: Anet_390 ~ CrownClass + (1 | Ring) 
#Data: GEdata 
#AIC   BIC logLik deviance REMLdev
#157.4 163.3  -74.7    153.4   149.4
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Ring     (Intercept) 3.5760   1.8910  
#Residual             5.7284   2.3934  
#Number of obs: 32, groups: Ring, 6
#
#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)          18.049      1.423  12.684
#CrownClassDominant   -2.441      1.348  -1.811
#
#Correlation of Fixed Effects:
#  (Intr)
#CrwnClssDmn -0.785
#> 
#


  
source('eucfacePower_v2.R')

effect.size<-seq(from=0,to=0.3,by=0.05)  # weird result!
#> 0.15 %in% seq(from=0,to=0.3,by=0.05)
#[1] FALSE
#> 
  
effect.size<-as.numeric(as.character(seq(from=0,to=0.3,by=0.05)))
n.tree<-1
n.leaf<-1:20

#length(effect.size)*length(n.tree)*length(n.leaf)*1000
#cat(length(effect.size)*length(n.tree)*length(n.leaf)*1000/10/60/60,'hours')

for(i in 1:length(effect.size)) {
  for(j in 1:length(n.tree)) {
    for(k in 1:length(n.leaf)) {
      for(l in 1:1000) eucface.power('power_Anet390_20121011.txt',effect.size[i],n.tree[j],n.leaf[k],ranef.ring.sd=1.891,rand.sd=2.393,intercept=18.049)
#      for(l in 1:100) eucface.power('power_Anet390_20121010.txt',effect.size[i],n.tree[j],n.leaf[k],ranef.ring.sd=1.891,rand.sd=2.393)  # didn't account for intercept
    }
  }
}
  
data<-read.table('power_Anet390_20121011.txt',sep='\t')
names(data)<-c('effect.size','n.ring','n.tree','n.leaf','p.val','int.est','trt.est','ranef.ring.sd','ring.sd','ranef.tree.sd','tree.sd','rand.sd','resid.sd','converged')

boxplot(ring.sd~n.leaf,data)

#with(data,aggregate(p.val,list(effect.size),function(x) mean(x<0.05)))
data.agg<-with(data[data$converged==T,],aggregate(p.val,list(effect.size=effect.size,n.tree=n.tree,n.leaf=n.leaf),function(x) mean(x<0.05)))
names(data.agg)[4]<-'power'
# data.agg<-with(data,aggregate(p.val,list(effect.size=effect.size,n.tree=n.tree,n.leaf=n.leaf),function(x) mean(x<0.06786)))
# names(data.agg)[4]<-'power'

# write.csv(data.agg[data.agg$effect.size != 0, ], 'output/power_photosynth.csv', row.names=F)

data$signif<-ifelse(data$p.val<0.05,1,0)
mod1<-glm(signif~effect.size*n.leaf,data=data,family=binomial)
summary(mod1)
mod2<-lm(p.val~effect.size*n.leaf,data=data)
summary(mod2)

xyplot(power~effect.size|n.tree:n.leaf,data=data.agg)

redbluefun<-colorRampPalette(c('red','blue'))
pdf('power_Anet390.pdf',height=10,width=10)
par(mfrow=c(1,1))
plot(power~n.leaf,data=data.agg[which(data.agg$effect.size==effect.size[1]),],type='l',ylim=c(0,1),col=redbluefun(7)[7],las=1,lwd=3,xlab='# leaves per ring')
title('Anet_390')
legend('topleft',legend=rev(unique(effect.size)),title='effect size',cex=1.2,lwd=3,col=redbluefun(7),bty='n')
abline(h=0.8,lty='dotted')
for(j in 2:7) lines(power~n.leaf,data=data.agg[which(data.agg$effect.size==effect.size[j]),],col=redbluefun(7)[7-j],lwd=3)
dev.off()


# repeat to look at effect of increasing number of rings

source('eucfacePower_v2.R')

effect.size<-as.numeric(as.character(seq(from=0,to=0.3,by=0.05)))
n.tree<-1
n.leaf<-1:5
n.ring<-c(6,8,10,12)

for(i in 1:length(effect.size)) {
	for(j in 1:length(n.tree)) {
		for(k in 1:length(n.leaf)) {
			for(l in 1:length(n.ring)) {
				for(m in 1:1000) eucface.power('power_Anet390varRingNo_20130130.txt',effect.size[i],n.tree[j],n.leaf[k],ranef.ring.sd=1.891,rand.sd=2.393,intercept=18.049,n.ring=n.ring[l])
			}
		}
	}
}



# using actual random effects estimates

source('eucfacePower_v3.R')

effect.size<-as.numeric(as.character(seq(from=0,to=0.3,by=0.05)))
n.tree<-1
n.leaf<-1:20
const.ranef.ring<-ranef(mod2b.ring)$Ring[,1]

for(i in 1:length(effect.size)) {
	for(j in 1:length(n.tree)) {
		for(k in 1:length(n.leaf)) {
			for(l in 1:10) eucface.power('power_Anet390_20130130.txt',effect.size[i],n.tree[j],n.leaf[k],const.ranef.ring=const.ranef.ring,rand.sd=2.393,intercept=18.049)
		}
	}
}




## real run -- extracts ##

library(lme4)

### no ###

mod1.ringplot<-lmer(no~1+(1|Date)+(1|Ring/Plot),data=extracts)
mod1.ring<-lmer(no~1+(1|Date)+(1|Ring),data=extracts)
mod1.plot<-lmer(no~1+(1|Date)+(1|Plot),data=extracts)
anova(mod1.ringplot,mod1.ring,mod1.plot)
AICc(mod1.ringplot); AICc(mod1.ring); AICc(mod1.plot); AICc(lm(no~Date,data=extracts))
qqnorm(resid(mod1.ring)); qqline(resid(mod1.ring))



### nh ###

mod1.ringplot<-lmer(nh~1+(1|Date)+(1|Ring/Plot),data=extracts)
mod1.ring<-lmer(nh~1+(1|Date)+(1|Ring),data=extracts)
mod1.plot<-lmer(nh~1+(1|Date)+(1|Plot),data=extracts)
anova(mod1.ringplot,mod1.ring,mod1.plot)
AICc(mod1.ringplot); AICc(mod1.ring); AICc(mod1.plot); AICc(lm(nh~Date,data=extracts))
qqnorm(resid(mod1.ringplot)); qqline(resid(mod1.ringplot))



### po ###

mod1.ringplot<-lmer(po~1+(1|Date)+(1|Ring/Plot),data=extracts)
mod1.ring<-lmer(po~1+(1|Date)+(1|Ring),data=extracts)
mod1.plot<-lmer(po~1+(1|Date)+(1|Plot),data=extracts)
anova(mod1.ringplot,mod1.ring,mod1.plot)
AICc(mod1.ringplot); AICc(mod1.ring); AICc(mod1.plot); AICc(lm(po~Date,data=extracts))
qqnorm(resid(mod1.ringplot)); qqline(resid(mod1.ringplot))




## real run -- ion exchange resins ##

library(lme4)

### Nitrate ###

mod1.ringplot<-lmer(Nitrate~1+(1|Date)+(1|Ring/Plot),data=iems)
mod1.ring<-lmer(Nitrate~1+(1|Date)+(1|Ring),data=iems)
mod1.plot<-lmer(Nitrate~1+(1|Date)+(1|Plot),data=iems)
anova(mod1.ringplot,mod1.ring,mod1.plot)
AICc(mod1.ringplot); AICc(mod1.ring); AICc(mod1.plot); AICc(lm(Nitrate~Date,data=iems))
qqnorm(resid(mod1.ring)); qqline(resid(mod1.ring))



### Ammonium ###

mod1.ringplot<-lmer(Ammonium~1+(1|Date)+(1|Ring/Plot),data=iems)
mod1.ring<-lmer(Ammonium~1+(1|Date)+(1|Ring),data=iems)
mod1.plot<-lmer(Ammonium~1+(1|Date)+(1|Plot),data=iems)
anova(mod1.ringplot,mod1.ring,mod1.plot)
AICc(mod1.ringplot); AICc(mod1.ring); AICc(mod1.plot); AICc(lm(Ammonium~Date,data=iems))
qqnorm(resid(mod1.ring)); qqline(resid(mod1.ring))



### Phosphate ###

mod1.ringplot<-lmer(Phosphate~1+(1|Date)+(1|Ring/Plot),data=iems)
mod1.ring<-lmer(Phosphate~1+(1|Date)+(1|Ring),data=iems)
mod1.plot<-lmer(Phosphate~1+(1|Date)+(1|Plot),data=iems)
anova(mod1.ringplot,mod1.ring,mod1.plot)
AICc(mod1.ringplot); AICc(mod1.ring); AICc(mod1.plot); AICc(lm(Phosphate~Date,data=iems))
qqnorm(resid(mod1.ring)); qqline(resid(mod1.ring))




### power simulations for soil nutrients ### 

# note: did not change labels but conceptually:
#	n.leaf == n.date
#	n.tree == n.plot
#	n.ring == n.ring

source('eucfacePower_v2.R')

effect.size<-as.numeric(as.character(seq(from=0,to=0.3,by=0.05)))
n.tree<-1:8
n.leaf<-1:4

for(i in 1:length(effect.size)) {
  for(j in 1:length(n.tree)) {
    for(k in 1:length(n.leaf)) {
     	# mod.NO3extr
      for(l in 1:1000) eucface.power('power_soilNutr_20140902.txt',effect.size[i],n.tree[j],n.leaf[k],ranef.ring.sd=0.181,ranef.tree.sd=0.141,rand.sd=0.217,intercept=0.19)
    	# mod.NO3iems
      for(l in 1:1000) eucface.power('power_soilNutr_20140902.txt',effect.size[i],n.tree[j],n.leaf[k],ranef.ring.sd=0.226,ranef.tree.sd=0.168,rand.sd=0.237,intercept=2.73)
	   	# mod.NH4extr
      for(l in 1:1000) eucface.power('power_soilNutr_20140902.txt',effect.size[i],n.tree[j],n.leaf[k],ranef.ring.sd=0.059,ranef.tree.sd=0.013,rand.sd=0.097,intercept=1.04)
    	# mod.NH4iems
      for(l in 1:1000) eucface.power('power_soilNutr_20140902.txt',effect.size[i],n.tree[j],n.leaf[k],ranef.ring.sd=0.231,ranef.tree.sd=0.151,rand.sd=0.362,intercept=1.80)
    	# mod.PO4extr
      for(l in 1:1000) eucface.power('power_soilNutr_20140902.txt',effect.size[i],n.tree[j],n.leaf[k],ranef.ring.sd=0.020,ranef.tree.sd=NULL,rand.sd=0.124,intercept=0.23)
    	# mod.PO4iems
      for(l in 1:1000) eucface.power('power_soilNutr_20140902.txt',effect.size[i],n.tree[j],n.leaf[k],ranef.ring.sd=0.164,ranef.tree.sd=NULL,rand.sd=0.386,intercept=0.31)
    }
  }
}
  

## problem in script estimating intercept and effect sizes from mixed models, missing from those lines
# data<-read.table('power_soilNutr_20140902.txt',sep='\t')
# names(data)<-c('effect.size','n.ring','n.plot','n.date','p.val','int.est','trt.est','ranef.ring.sd','ring.sd','ranef.tree.sd','tree.sd','rand.sd','resid.sd','converged')
data <- readLines('power_soilNutr_20140902.txt')
data1 <- strsplit(data, '\t')
for(i in 1:length(data1)) {
	if(length(data1[[i]]) == 14) suppressWarnings(write.table(matrix(as.numeric(data1[[i]][c(1:5, 8:14)]), nrow=1), 'power_soilNutr_20140902edit.txt', sep='\t', col.names=F, row.names=F, append=T))
	if(length(data1[[i]]) == 12) suppressWarnings(write.table(matrix(as.numeric(data1[[i]]), nrow=1), 'power_soilNutr_20140902edit.txt', sep='\t', col.names=F, row.names=F, append=T))
}


dataNut<-read.table('power_soilNutr_20140902edit.txt',sep='\t')
names(dataNut)<-c('effect.size','n.ring','n.plot','n.date','p.val','ranef.ring.sd','ring.sd','ranef.plot.sd','plot.sd','rand.sd','resid.sd','converged')
dataNut$sim <- with(dataNut, factor(paste('bi', ranef.ring.sd, 'bij', ranef.plot.sd, 'eijk', rand.sd, sep='_')))
 

boxplot(ring.sd~n.plot,dataNut)

dataNut.agg<-with(dataNut[dataNut$converged==T,],aggregate(p.val,list(sim=sim,effect.size=effect.size,n.plot=n.plot,n.date=n.date),function(x) mean(x<0.05)))
#dataNut.agg<-with(dataNut,aggregate(p.val,list(effect.size,n.tree,n.leaf),function(x) mean(x<0.06786)))
names(dataNut.agg)[5]<-'power'

dataNut.spl<-split(dataNut, dataNut$sim)
x.p <- lapply(dataNut.spl, function(x) summary(lm(converged ~ effect.size * n.plot * n.date, data=x)))

# dataNut$signif<-ifelse(dataNut$p.val<0.05,1,0)

xyplot(power~effect.size|n.plot:n.date,data=dataNut.agg)

redbluefun<-colorRampPalette(c('red','blue'))
pdf('power_Anet390.pdf',height=10,width=10)
par(mfrow=c(1,1))
plot(power~n.leaf,data=data.agg[which(data.agg$effect.size==effect.size[1]),],type='l',ylim=c(0,1),col=redbluefun(7)[7],las=1,lwd=3,xlab='# leaves per ring')
title('Anet_390')
legend('topleft',legend=rev(unique(effect.size)),title='effect size',cex=1.2,lwd=3,col=redbluefun(7),bty='n')
abline(h=0.8,lty='dotted')
for(j in 2:7) lines(power~n.leaf,data=data.agg[which(data.agg$effect.size==effect.size[j]),],col=redbluefun(7)[7-j],lwd=3)
dev.off()


# repeat to look at effect of increasing number of rings

source('eucfacePower_v2.R')

effect.size<-as.numeric(as.character(seq(from=0,to=0.3,by=0.05)))
n.tree<-1
n.leaf<-1:5
n.ring<-c(6,8,10,12)

for(i in 1:length(effect.size)) {
	for(j in 1:length(n.tree)) {
		for(k in 1:length(n.leaf)) {
			for(l in 1:length(n.ring)) {
				for(m in 1:1000) eucface.power('power_Anet390varRingNo_20130130.txt',effect.size[i],n.tree[j],n.leaf[k],ranef.ring.sd=1.891,rand.sd=2.393,intercept=18.049,n.ring=n.ring[l])
			}
		}
	}
}



# using actual random effects estimates

source('eucfacePower_v3.R')

effect.size<-as.numeric(as.character(seq(from=0,to=0.3,by=0.05)))
n.tree<-1
n.leaf<-1:20
const.ranef.ring<-ranef(mod2b.ring)$Ring[,1]

for(i in 1:length(effect.size)) {
	for(j in 1:length(n.tree)) {
		for(k in 1:length(n.leaf)) {
			for(l in 1:10) eucface.power('power_Anet390_20130130.txt',effect.size[i],n.tree[j],n.leaf[k],const.ranef.ring=const.ranef.ring,rand.sd=2.393,intercept=18.049)
		}
	}
}




###### barplots showing variance components ######

tm <- c(0.1471, 0.1955, 4.6050, 0.2455)
vw <- c(0.01439, 0.03945, 0.10246, 0.03159)
no <- c(0.8956, 1.1049, 0.5320, 1.3504)
nh <- c(0, 0, 4.857, 1.290)
po <- c(0.2094, 0.2257, 0.2776, 0.3222)
gr <- c(8.134, 5.652, 12.720, 7.599)
lt <- c(5.241, 7.280, 3.939, 6.745)

lev <- c('Plot', 'Ring', 'Date', 'Resid')

data <- cbind(s.temp=tm/sum(tm), s.moist=vw/sum(vw), NO3=no/sum(no), NH4=nh/sum(nh), PO4=po/sum(po), grass=gr/sum(gr), litter=lt/sum(lt), NA, NA)
data <- data[c(3,2,1,4), ]
rownames(data) <- lev[c(3,2,1,4)]

barplot(data, las=2, legend.text=T, ylab='proportion of total variation (sd)', main='Variance components')

