####### This script calls run.R and plot figures based on that

#### Call run.R program
source("run.R")

#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}


#### LAI
lai.tr <- make_treatment_effect_df(inDF=lai_variable, v=3, cond=1)
### make the plot
ac <- subset(lai.tr, Treatment == "aCO2")
ec <- subset(lai.tr, Treatment == "eCO2")

p1 <- ggplot(ac, aes(Date))+
    geom_line(data=ac, aes(y=avg, col="aCO2"))+
    geom_ribbon(data=ac,aes(ymin=neg,ymax=pos),fill="cyan", alpha=0.3)+
    geom_line(data=ec, aes(y=avg, col="eCO2"))+
    geom_ribbon(data=ec,aes(ymin=neg,ymax=pos),fill="red", alpha=0.3)+
    labs(x="Date", y="LAI")+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

pdf("output/lai_time_series.pdf", width=10, height=6)
plot(p1)
dev.off()


### SLA
sla.tr <- make_treatment_effect_df(inDF=sla_variable, v=3, cond=2)

### make the plot
ac <- subset(sla.tr, Treatment == "aCO2")
ec <- subset(sla.tr, Treatment == "eCO2")

p1 <- ggplot(ac, aes(Date))+
    geom_line(data=ac, aes(y=avg, col="aCO2"))+
    geom_ribbon(data=ac,aes(ymin=neg,ymax=pos),fill="cyan", alpha=0.3)+
    geom_line(data=ec, aes(y=avg, col="eCO2"))+
    geom_ribbon(data=ec,aes(ymin=neg,ymax=pos),fill="red", alpha=0.3)+
    labs(x="Date", y=expression(paste("SLA (cm ", g^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))


pdf("output/sla_time_series.pdf", width=10, height=6)
plot(p1)
dev.off()
