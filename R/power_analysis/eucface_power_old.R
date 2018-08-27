### Notes:
### 1. Power analysis for each variable?
### 2. Which period of data to use?
### 3. What exactly are we looking for?
###    3.1 How many replicates do we need to give statistical power to rule out Type I error?
###    3.2 Is our sample size large enough to detect statistically significant treatment effect?
###    3.3 What is the likelihood of detecting a statisitcally significant treatment effect of x%, given the current design and replicates?

### To-do list:
### 1. Replicate what's in here for photosynthesis and biomass growth, using updated data.
### 2. Create placeholder for other variables. 
### 3. Discuss with Jeff and Belinda what question to explore. 
### 4. How to reduce interannual variability and intra-treatment variability, so that a change in treatment can be statisitcally detectable.  
### 5. CO2 treatment timeseries plot - is there a time lag in CO2 application in elevated rings?

 
source('R/power_analysis/eucfacePower_old_v2.R')

effect.size<-as.numeric(as.character(seq(from=0,to=0.3,by=0.05)))
n.tree<-1
n.leaf<-1:20

### variables to fill: ranef.ring.sd=1.891,rand.sd=2.393,intercept=18.049

for(i in 1:length(effect.size)) {
  for(j in 1:length(n.tree)) {
    for(k in 1:length(n.leaf)) {
      for(l in 1:2) eucface.power('power_Anet390_20121011.txt',effect.size[i],n.tree[j],n.leaf[k],ranef.ring.sd=1.891,rand.sd=2.393,intercept=18.049)
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

