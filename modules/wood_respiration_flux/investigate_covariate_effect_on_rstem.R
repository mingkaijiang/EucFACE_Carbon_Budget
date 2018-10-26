investigate_covariate_effect_on_rstem <- function(inDF,var.col) {
    
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
            ### averaged over days within a year 
            tmp <- with(inDF[inDF$Ring == i & inDF$Yr == j, ],
                        sum(Value*ndays, na.rm=T)/sum(ndays, na.rm=T)) * 365 / 1000
            tDF[tDF$Ring == i & tDF$Yr == j, "Value"] <- tmp
        }
    }
    
    cov2 <- lai_variable[lai_variable$Date=="2012-10-26",]
    covDF2 <- summaryBy(lai_variable~Ring, data=cov2, FUN=mean, keep.names=T)
    
    ### Read initial basal area data
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    f12$ba <- ((f12$X20.09.2012/2)^2) * pi
    
    baDF <- summaryBy(ba~Ring, data=f12, FUN=sum, na.rm=T, keep.names=T)
    
    ### return in unit of cm2/m2, which is m2 ha-1
    baDF$ba_ground_area <- baDF$ba / ring_area
    
    for (i in 1:6) {
        tDF$Cov3[tDF$Ring==i] <- baDF$ba_ground_area[baDF$Ring==i]
    }

    ### Get tree information where stem efflux were measured
    rstemDF <- read.csv(file.path(getToPath(), 
                                  "FACE_A0089_RA_STEMCO2EFLUX_L1_20171218-20171220.csv"))
    tree.list <- unique(rstemDF$Label)
    
    tmpDF <- f12[f12$Tree%in%tree.list,]
    
    baDF <- summaryBy(ba~Ring, data=tmpDF, FUN=sum, na.rm=T, keep.names=T)
    
    ### return in unit of cm2/m2, which is m2 ha-1
    baDF$ba_ground_area <- baDF$ba / ring_area
    
    for (i in 1:6) {
        tDF$Cov2[tDF$Ring==i] <- covDF2$lai_variable[covDF2$Ring==i]
        tDF$Cov4[tDF$Ring==i] <- baDF$ba_ground_area[baDF$Ring==i]
    }
    
    
    ### Add psyllid attack event
    tDF$Psyllid <- "Before"
    tDF$Psyllid[tDF$Yr >= 2016] <- "After"

    
    ### Analyse the variable model
    ## model 1: no interaction, year as factor, ring random factor
    int.m1 <- "non-interative_with_covariate"
    modelt1 <- lmer(Value~Trt + Yrf + Cov2 + (1|Ring), data=tDF)
    
    ## anova
    m1.anova <- Anova(modelt1, test="F")
    
    ## Check ele - amb diff
    summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size1 <- coef(modelt1)[[1]][1,2]
    
    ## confidence interval 
    eff.conf1 <- confint(modelt1,"Trtele")
    
    
    ### test effect of Cov3 - basal area
    ## model 1: no interaction, year as factor, ring random factor
    int.mt <- "non-interative_with_covariate"
    modeltt <- lmer(Value~Trt + Yrf + Cov4 + (1|Ring), data=tDF)
    
    ## anova
    mt.anova <- Anova(modeltt, test="F")
    
    ## Check ele - amb diff
    summt <- summary(glht(modeltt, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.sizet <- coef(modeltt)[[1]][1,2]
    
    ## confidence interval 
    eff.conft <- confint(modeltt,"Trtele")
    
 
    ### conditional output
    out1 <- list(int.state=int.m1,
                 mod = modelt1, 
                 anova = m1.anova,
                 diff = summ1,
                 eff = eff.size1,
                 conf = eff.conf1)
    
    out2 <- list(int.state=int.mt,
                 mod = modeltt, 
                 anova = mt.anova,
                 diff = summt,
                 eff = eff.sizet,
                 conf = eff.conft)

    ### Predict the model with a standard LAI value
    newDF1 <- tDF
    newDF1$Cov2 <- 1.14815  # initial LAI averages
    newDF1$predicted <- predict(out1$mod, newdata=newDF1)
    
    
    ### Predict the model with a standard basal area value
    newDF2 <- tDF
    newDF2$Cov4 <- 3.885  # initial basal area
    newDF2$predicted <- predict(out2$mod, newdata=newDF2)
    

    pdf("R_other/basal_area_individual_tree_per_ring.pdf")
    with(tmpDF, plot(ba~Ring, ylab=("BA (cm2)"), 
                     main="Basal area for selected Rstem measurement trees"))
    
    with(baDF, plot(ba~Ring,
                    ylab=("BA (cm2)"), 
                    main="Total BA for each ring, selected trees"))
    
    baDF <- summaryBy(ba~Ring, data=f12, FUN=sum, na.rm=T, keep.names=T)
    
    with(baDF, plot(ba~Ring,
                    ylab=("BA (cm2)"), 
                    main="Total BA for each ring, all trees"))

    dev.off()  
    
    raw.avg <- summaryBy(Value~Yr+Trt, FUN=mean, data=newDF1, keep.names=T)
    lai.avg <- summaryBy(predicted~Yr+Trt, FUN=mean, data=newDF1, keep.names=T)
    ba.avg <- summaryBy(predicted~Yr+Trt, FUN=mean, data=newDF2, keep.names=T)
    
    raw.sd <- summaryBy(Value~Yr+Trt, FUN=se, data=newDF1, keep.names=T)
    lai.sd <- summaryBy(predicted~Yr+Trt, FUN=se, data=newDF1, keep.names=T)
    ba.sd <- summaryBy(predicted~Yr+Trt, FUN=se, data=newDF2, keep.names=T)
    
    raw.avg$sd <- raw.sd$Value
    lai.avg$sd <- lai.sd$predicted
    ba.avg$sd <- ba.sd$predicted
    
    raw.avg$pos <- raw.avg$Value + raw.avg$sd
    raw.avg$neg <- raw.avg$Value - raw.avg$sd
    
    lai.avg$pos <- lai.avg$predicted + lai.avg$sd
    lai.avg$neg <- lai.avg$predicted - lai.avg$sd
    
    ba.avg$pos <- ba.avg$predicted + ba.avg$sd
    ba.avg$neg <- ba.avg$predicted - ba.avg$sd
    
    ### Make plots
    p1 <- ggplot(raw.avg, aes(x=as.character(Yr), y=Value))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste(R[wood], " (g C ", m^-2, yr^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        ylim(0, 600)+
        #scale_y_continuous(position="left")+
        scale_fill_manual(name="Treatment", values = c("amb" = "cyan", "ele" = "pink"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="Treatment", values = c("amb" = "blue", "ele" = "red"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    p2 <- ggplot(lai.avg, aes(x=as.character(Yr), y=predicted))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste(R[wood], " (g C ", m^-2, yr^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="none")+
        ylim(0, 600)+
        #scale_y_continuous(position="left")+
        scale_fill_manual(name="Treatment", values = c("amb" = "cyan", "ele" = "pink"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="Treatment", values = c("amb" = "blue", "ele" = "red"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    p3 <- ggplot(ba.avg, aes(x=as.character(Yr), y=predicted))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste(R[wood], " (g C ", m^-2, yr^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"),
              legend.position="bottom")+
        ylim(0, 600)+
        #scale_y_continuous(position="left")+
        scale_fill_manual(name="Treatment", values = c("amb" = "cyan", "ele" = "pink"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="Treatment", values = c("amb" = "blue", "ele" = "red"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    grid.labs <- c("(raw)", "(lai corrected)", "(ba corrected)")
    
    pdf("R_other/Rwood_annual_comparison_flux.pdf", width=8,height=12)
    plot_grid(p1, p2, 
              p3,  
              labels="", ncol=1, align="v", axis = "l", 
              rel_heights=c(0.7, 0.7, 1))
    grid.text(grid.labs,x = 0.15, y = c(0.97, 0.68, 0.38),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
}
