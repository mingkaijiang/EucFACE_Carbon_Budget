# added code for detecting and noting lack of convergence
# added code for including random effect of ring
# fixed code for estimating response (added intercept)
# changed control options for lmer (2014-09-03)

tryCatch.converg <- function(expr) {
  converged <- T
  w.handler <- function(w) {
    converged <<- F
	invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
 				     warning = w.handler),
 				     converged = converged)
}

eucface.power<-function(file.out,effect.size,n.tree,n.leaf,ranef.ring.sd=NULL,ranef.tree.sd=NULL,rand.sd,intercept,n.ring=6) {  
  n.ring<-n.ring
  n.tree<-n.tree
  n.leaf<-n.leaf

  trt<-factor(rep(c('elevated','ambient'),each=n.ring/2*n.tree*n.leaf))
  ring<-factor(rep(1:n.ring,each=n.tree*n.leaf))
  tree<-rep(rep(1:n.tree,each=n.leaf),n.ring)
  tree<-tree+rep(1:n.ring*100,each=n.tree*n.leaf)
  tree<-factor(tree)
  
  effect.size<-effect.size
  intercept<-intercept
  effect<-intercept*c(rep(1+effect.size,n.ring/2*n.tree*n.leaf),rep(1,n.ring/2*n.tree*n.leaf))
  
  rand.sd<-rand.sd
  rand<-rnorm(n.ring*n.tree*n.leaf,mean=0,sd=rand.sd)
  ifelse(is.null(ranef.ring.sd),ranef.ring<-rep(0,n.ring*n.tree*n.leaf),ranef.ring<-rep(rnorm(n.ring,mean=0,sd=ranef.ring.sd),each=n.leaf*n.tree))
  ifelse(is.null(ranef.tree.sd),ranef.tree<-rep(0,n.ring*n.tree*n.leaf),ranef.tree<-rep(rnorm(n.ring*n.tree,mean=0,sd=ranef.tree.sd),each=n.leaf))
   
  response<-ranef.ring+ranef.tree+effect+rand
  
  if(n.tree>1 & n.leaf>1){
    if(!is.null(ranef.ring.sd) & !is.null(ranef.tree.sd)) {
      mod.alt<-tryCatch.converg(lmer(response~trt+(1|ring/tree),REML=F,control=lmerControl(optCtrl=list(maxfun=1000))))
      mod.nul<-tryCatch.converg(lmer(response~1+(1|ring/tree),REML=F,control=lmerControl(optCtrl=list(maxfun=1000))))

      ring.sd<-attr(VarCorr(mod.alt$value)[['ring']],'stddev')[['(Intercept)']]
      tree.sd<-attr(VarCorr(mod.alt$value)[['tree:ring']],'stddev')[['(Intercept)']]
    }
    if(!is.null(ranef.ring.sd) & is.null(ranef.tree.sd)) {
      mod.alt<-tryCatch.converg(lmer(response~trt+(1|ring),REML=F,control=lmerControl(optCtrl=list(maxfun=1000))))
      mod.nul<-tryCatch.converg(lmer(response~1+(1|ring),REML=F,control=lmerControl(optCtrl=list(maxfun=1000))))

      ring.sd<-attr(VarCorr(mod.alt$value)[['ring']],'stddev')[['(Intercept)']]
      tree.sd<-NA
    }
    if(is.null(ranef.ring.sd) & !is.null(ranef.tree.sd)) {
      mod.alt<-tryCatch.converg(lmer(response~trt+(1|tree),REML=F,control=lmerControl(optCtrl=list(maxfun=1000))))
      mod.nul<-tryCatch.converg(lmer(response~1+(1|tree),REML=F,control=lmerControl(optCtrl=list(maxfun=1000))))
      
      ring.sd<-NA
      tree.sd<-attr(VarCorr(mod.alt$value)[['tree:ring']],'stddev')[['(Intercept)']]
    }     
    
    resid.sd<-attr(VarCorr(mod.alt$value),'sc')
    int.est<-attr(summary(mod.alt$value),'coefs')['(Intercept)','Estimate']
    trt.est<-attr(summary(mod.alt$value),'coefs')['trtelevated','Estimate']
    m0<-mod.nul$value
    m1<-mod.alt$value
    p.val<-anova(m0,m1)[['Pr(>Chisq)']][2]
    converged<-all(mod.nul$converged,mod.alt$converved)
  }

  if(n.tree>1 & n.leaf==1){
    mod.alt<-tryCatch.converg(lmer(response~trt+(1|ring),REML=F,control=lmerControl(optCtrl=list(maxfun=1000))))
    mod.nul<-tryCatch.converg(lmer(response~1+(1|ring),REML=F,control=lmerControl(optCtrl=list(maxfun=1000))))
    
    ring.sd<-attr(VarCorr(mod.alt$value)[['ring']],'stddev')[['(Intercept)']]
    tree.sd<-NA
    resid.sd<-attr(VarCorr(mod.alt$value),'sc')
    int.est<-attr(summary(mod.alt$value),'coefs')['(Intercept)','Estimate']
    trt.est<-attr(summary(mod.alt$value),'coefs')['trtelevated','Estimate']
    m0<-mod.nul$value
    m1<-mod.alt$value
    p.val<-anova(m0,m1)[['Pr(>Chisq)']][2]
    converged<-all(mod.nul$converged,mod.alt$converved)
  }

  if(n.tree==1 & n.leaf>1){
    mod.alt<-tryCatch.converg(lmer(response~trt+(1|tree),REML=F,control=lmerControl(optCtrl=list(maxfun=1000))))
    mod.nul<-tryCatch.converg(lmer(response~1+(1|tree),REML=F,control=lmerControl(optCtrl=list(maxfun=1000))))
    	    
    if(is.null(ranef.tree.sd)) {
      ring.sd<-attr(VarCorr(mod.alt$value)[['tree']],'stddev')[['(Intercept)']]
      tree.sd<-NA
    } else {
      ring.sd<-NA
      tree.sd<-attr(VarCorr(mod.alt$value)[['tree']],'stddev')[['(Intercept)']]   
    }
    resid.sd<-attr(VarCorr(mod.alt$value),'sc')
    int.est<-attr(summary(mod.alt$value),'coefs')['(Intercept)','Estimate']
    trt.est<-attr(summary(mod.alt$value),'coefs')['trtelevated','Estimate']
    m0<-mod.nul$value
    m1<-mod.alt$value
    p.val<-anova(m0,m1)[['Pr(>Chisq)']][2]
    converged<-all(mod.nul$converged,mod.alt$converved)
  }
  
  if(n.tree==1 & n.leaf==1) {
    mod.alt<-lm(response~trt)
    mod.nul<-lm(response~1)
    
    ring.sd<-NA
    tree.sd<-NA
    resid.sd<-summary(mod.alt)[['sigma']]
    int.est<-coef(mod.alt)[['(Intercept)']]
    trt.est<-coef(mod.alt)[['trtelevated']]
    p.val<-anova(mod.nul,mod.alt)[['Pr(>F)']][2]
    converged<-T
  }
  
  if(is.null(ranef.ring.sd)) ranef.ring.sd<-NA
  if(is.null(ranef.tree.sd)) ranef.tree.sd<-NA
  
  out<-cbind(effect.size,n.ring,n.tree,n.leaf,p.val,int.est,trt.est,ranef.ring.sd,ring.sd,ranef.tree.sd,tree.sd,rand.sd,resid.sd,converged)
  write.table(out,file=file.out,append=T,sep='\t',row.names=F,col.names=F)
}
