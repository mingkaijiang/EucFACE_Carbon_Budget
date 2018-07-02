####### This script calls run.R and plot combined figures

#############################################################
#### Call run.R program
source("run.R")

#### Source the function that makes treatment effect df
source("R/make_treatment_effect_df.R")

#### library
require(grid)

#############################################################
#### Plot C-related variables

###################---------------------######################
### LAI + SLA + Leaf C time series plot
##  generate treatment effect df for each variable
lai.tr <- make_treatment_effect_df(inDF=lai_variable, v=3, cond=1)
sla.tr <- make_treatment_effect_df(inDF=sla_variable, v=3, cond=2)
leafc.tr <- make_treatment_effect_df(inDF=leaf_c_pool, v=3, cond=1)
#uac.tr <- make_treatment_effect_df(inDF=understorey_aboveground_c_pool, v=3, cond=1)

## lai plot
p1 <- ggplot(lai.tr, aes(Date))+
    geom_ribbon(data=lai.tr,aes(ymin=neg,ymax=pos, fill=factor(Treatment)))+
    geom_line(data=lai.tr, aes(y=avg, color=factor(Treatment)))+
    labs(x="Date", y="LAI")+
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
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_date(date_breaks = "1 year", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-08-01','2018-12-01')))

## sla plot
p2 <- ggplot(sla.tr, aes(Date))+
    #geom_ribbon(data=sla.tr,aes(ymin=neg,ymax=pos, fill=factor(Treatment)))+
    geom_point(data=sla.tr, aes(y=avg, color=factor(Treatment)))+
    geom_segment(data=sla.tr, aes(x=Date, y=neg, xend=Date, yend=pos, color=factor(Treatment)))+
    labs(x="Date", y=expression(paste("SLA (cm2 ", g^-1, ")")))+
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
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_date(date_breaks = "1 year", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-08-01','2018-12-01')))

## leaf c plot
p3 <- ggplot(leafc.tr, aes(Date))+
    geom_ribbon(data=leafc.tr,aes(ymin=neg,ymax=pos, fill=factor(Treatment)))+
    geom_line(data=leafc.tr, aes(y=avg, color=factor(Treatment)))+
    labs(x="Date", y=expression(paste(C[leaf], " (g C ", m^-2, ")")))+
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
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_date(date_breaks = "1 year", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-08-01','2018-12-01')))

grid.labs <- c("(a)", "(b)", "(c)")

## plot 
pdf("output/LAI_SLA_LeafC_time_series.pdf", width=10,height=8)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), 
                ggplotGrob(p3), size="last"))
grid.text(grid.labs,x = 0.1, y = c(0.95, 0.65, 0.36),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()

###################---------------------######################
### Understorey C, compare two methods
uac1.tr <- make_treatment_effect_df(inDF=understorey_aboveground_c_pool, v=5, cond=1)
uac1.live.tr <- make_treatment_effect_df(inDF=understorey_aboveground_c_pool, v=3, cond=1)
uac1.dead.tr <- make_treatment_effect_df(inDF=understorey_aboveground_c_pool, v=4, cond=1)
uac2.tr <- make_treatment_effect_df(inDF=understorey_aboveground_c_pool_2, v=3, cond=1)

uac1.live.tr$Class <- "Live"
uac1.dead.tr$Class <- "Dead"
uac1.class.tr <- rbind(uac1.live.tr, uac1.dead.tr)

## uac1 plot
p1 <- ggplot(uac1.tr, aes(Date))+
    #geom_ribbon(data=uac1.tr,aes(ymin=neg, ymax=pos, fill=factor(Treatment)))+
    geom_segment(data=uac1.tr, aes(x=Date, y=neg, xend=Date, yend=pos, color=factor(Treatment)))+
    geom_point(data=uac1.tr, aes(y=avg, color=factor(Treatment)))+
    labs(x="Date", y=expression(paste(C[ua1], " (g C ", m^-2, ")")))+
    scale_x_date(date_breaks = "6 month", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2015-01-01','2017-05-01')))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="right")+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

## uac2 plot
p2 <- ggplot(uac2.tr, aes(Date))+
    geom_ribbon(data=uac2.tr,aes(ymin=neg, ymax=pos, fill=factor(Treatment)))+
    #geom_segment(data=uac2.tr, aes(x=Date, y=neg, xend=Date, yend=pos, color=factor(Treatment)))+
    geom_line(data=uac2.tr, aes(y=avg, color=factor(Treatment)))+
    labs(x="Date", y=expression(paste(C[ua2], " (g C ", m^-2, ")")))+
    scale_x_date(date_breaks = "6 month", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2015-01-01','2017-05-01')))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="right")+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

## uac1 live and dead bar plot
p3 <- ggplot(uac1.class.tr, aes(x=Date, y=avg, fill=Class))+
    geom_bar(stat="identity", position="stack")+facet_grid(~Treatment)+
    labs(x="Date", y=expression(paste(C[ua], " (g C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="right")+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_fill_manual(name="Vegetation", values = c("Live" = "green", "Dead" = "orange"),
                      labels=c("Live","Dead"))

grid.labs <- c("(a)", "(b)", "(c)", "(d)")

require(cowplot)

## plot 
pdf("output/Understorey_C_pool_time_series.pdf", width=10,height=8)
plot_grid(p1, p2, p3, labels="", ncol=1, align="v", axis = "l")

#grid.newpage()
#grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3),
#                size="last"))
grid.text(grid.labs,x = c(0.1,0.1,0.1,0.5), y = c(0.95, 0.62, 0.28, 0.28),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()


###################---------------------######################
### Leaf C, wood C, understorey aboveground C, fineroot C, coarseroot C
##  generate treatment effect df for each variable
leafc.tr <- make_treatment_effect_df(inDF=leaf_c_pool, v=3, cond=1)
woodc.tr <- make_treatment_effect_df(inDF=wood_c_pool, v=3, cond=1)
uac.tr <- make_treatment_effect_df(inDF=understorey_aboveground_c_pool, v=3, cond=1)
frc.tr <- make_treatment_effect_df(inDF=fineroot_c_pool, v=3, cond=1)
crc.tr <- make_treatment_effect_df(inDF=coarse_root_c_pool_1, v=3, cond=1)

## split into ac and ec
ac <- subset(leafc.tr, Treatment == "aCO2")
ec <- subset(leafc.tr, Treatment == "eCO2")

## leaf c plot
p1 <- ggplot(leafc.tr, aes(Date))+
    geom_ribbon(data=leafc.tr,aes(ymin=neg,ymax=pos, fill=factor(Treatment)))+
    geom_line(data=leafc.tr, aes(y=avg, color=factor(Treatment)))+
    labs(x="Date", y=expression(paste(C[leaf], " (g C ", m^-2, ")")))+
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
    scale_y_continuous(position="left")+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

plot(p1)

## understorey c plot
p2 <- ggplot(uac.tr, aes(Date))+
    geom_segment(data=uac.tr, aes(x = Date, y = neg, xend = Date, yend = pos, color=factor(Treatment)))+
    #geom_ribbon(data=uac.tr,aes(ymin=neg,ymax=pos, fill=factor(Treatment)))+
    geom_point(data=uac.tr, aes(y=avg, color=factor(Treatment)))+
    labs(x="Date", y="   ")+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.text=element_text(size=14),
          legend.title=element_text(size=16),
          panel.grid.major=element_line(color="grey"),
          legend.position="bottom",
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    scale_y_continuous(sec.axis = sec_axis(~., name = expression(paste(C[ua], " (g C ", m^-2, ")"))))+
    scale_colour_manual(name="", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_fill_manual(name="", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

plot(p2)



## Compute monthly averages
leafc.tr$Month <- format(as.Date(leafc.tr$Date), "%Y-%m")
leafc.tr$Month <- as.Date(paste0(as.character(leafc.tr$Month), "-01"), format="%Y-%m-%d")
leafc.m <- aggregate(.~Treatment+Month, data=leafc.tr, FUN=mean)

woodc.tr$Month <- format(as.Date(woodc.tr$Date), "%Y-%m")
woodc.tr$Month <- as.Date(paste0(as.character(woodc.tr$Month), "-01"), format="%Y-%m-%d")
woodc.m <- aggregate(.~Treatment+Month, data=woodc.tr, FUN=mean)

uac.tr$Month <- format(as.Date(uac.tr$Date), "%Y-%m")
uac.tr$Month <- as.Date(paste0(as.character(uac.tr$Month), "-01"), format="%Y-%m-%d")
uac.m <- aggregate(.~Treatment+Month, data=uac.tr, FUN=mean)

frc.tr$Month <- format(as.Date(frc.tr$Date), "%Y-%m")
frc.tr$Month <- as.Date(paste0(as.character(frc.tr$Month), "-01"), format="%Y-%m-%d")
frc.m <- aggregate(.~Treatment+Month, data=frc.tr, FUN=mean)

crc.tr$Month <- format(as.Date(crc.tr$Date), "%Y-%m")
crc.tr$Month <- as.Date(paste0(as.character(crc.tr$Month), "-01"), format="%Y-%m-%d")
crc.m <- aggregate(.~Treatment+Month, data=crc.tr, FUN=mean)


## assign all variable values onto the time series
s1 <- merge(leafc.m, woodc.m, by=c("Month", "Treatment"), all=T)
s2 <- merge(s1, uac.m, by=c("Month", "Treatment"), all=T)
s3 <- merge(s2, frc.m, by=c("Month", "Treatment"), all=T)
s4 <- merge(s3, crc.m, by=c("Month", "Treatment"), all=T)

colnames(s4) <- c("Month", "Treatment", "date1", "a1", "b1", "c1",
                  "lai_avg", "lai_max", "lai_min", "lai_sd", "lai_pos", "lai_neg",
                  "date2", "a2", "b2", "c2",
                  "sla_avg", "sla_max", "sla_min", "sla_sd", "sla_pos", "sla_neg",
                  "date3", "a3", "b3", "c3",
                  "leafc_avg", "leafc_max", "leafc_min", "leafc_sd", "leafc_pos", "leafc_neg",
                  "date4", "a4", "b4", "c4",
                  "uac_avg", "uac_max", "uac_min", "uac_sd", "uac_pos", "uac_neg")

## split into ac and ec
ac <- subset(s4, Treatment == "aCO2")
ec <- subset(s4, Treatment == "eCO2")

## leaf c plot
p1 <- ggplot(ac, aes(Month))+
    geom_line(data=ac, aes(y=leafc_avg, col="aCO2"))+
    geom_ribbon(data=ac,aes(ymin=leafc_neg,ymax=leafc_pos),fill="cyan", alpha=0.3)+
    geom_line(data=ec, aes(y=leafc_avg, col="eCO2"))+
    geom_ribbon(data=ec,aes(ymin=leafc_neg,ymax=leafc_pos),fill="red", alpha=0.3)+
    labs(x="Date", y=expression(paste(C[leaf], " (g C ", m^-2, ")")))+
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
    scale_y_continuous(position="left")+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

## understorey c plot
p3 <- ggplot(ac, aes(Month))+
    geom_segment(data=ac, aes(x = Month, y = uac_neg, xend = Month, yend = uac_pos), col="cyan")+
    geom_segment(data=ec, aes(x = Month, y = uac_neg, xend = Month, yend = uac_pos), col="red")+
    geom_point(data=ac, aes(y=uac_avg, col="aCO2"))+
    geom_point(data=ec, aes(y=uac_avg, col="eCO2"))+
    labs(x="Date", y="   ")+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.text=element_text(size=14),
          legend.title=element_text(size=16),
          panel.grid.major=element_line(color="grey"),
          legend.position="bottom",
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    scale_y_continuous(sec.axis = sec_axis(~., name = expression(paste(C[ua], " (g C ", m^-2, ")"))))+
    scale_colour_manual(name="", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)")

## plot 
pdf("output/Plant_C_time_series.pdf", width=14,height=8)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), 
                ggplotGrob(p3), ggplotGrob(p4), 
                ggplotGrob(p5), size="last"))
grid.text(grid.labs,x = 0.07, y = c(0.95, 0.73, 0.51, 0.29),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()