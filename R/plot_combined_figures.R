####### This script calls run.R and plot combined figures

#### Call run.R program
source("run.R")

#### Source the function that makes treatment effect df
source("R/make_treatment_effect_df.R")

#### library
require(grid)

#### Plot C-related variables
### LAI + SLA + Leaf C + understorey aboveground C time series plot
##  generate treatment effect df for each variable
lai.tr <- make_treatment_effect_df(inDF=lai_variable, v=3, cond=1)
sla.tr <- make_treatment_effect_df(inDF=sla_variable, v=3, cond=2)
leafc.tr <- make_treatment_effect_df(inDF=leaf_c_pool, v=3, cond=1)
uac.tr <- make_treatment_effect_df(inDF=understorey_aboveground_c_pool, v=3, cond=1)

## Compute monthly averages
lai.tr$Month <- format(as.Date(lai.tr$Date), "%Y-%m")
lai.tr$Month <- as.Date(paste0(as.character(lai.tr$Month), "-01"), format="%Y-%m-%d")
lai.m <- aggregate(.~Treatment+Month, data=lai.tr, FUN=mean)

sla.tr$Month <- format(as.Date(sla.tr$Date), "%Y-%m")
sla.tr$Month <- as.Date(paste0(as.character(sla.tr$Month), "-01"), format="%Y-%m-%d")
sla.m <- aggregate(.~Treatment+Month, data=sla.tr, FUN=mean)

leafc.tr$Month <- format(as.Date(leafc.tr$Date), "%Y-%m")
leafc.tr$Month <- as.Date(paste0(as.character(leafc.tr$Month), "-01"), format="%Y-%m-%d")
leafc.m <- aggregate(.~Treatment+Month, data=leafc.tr, FUN=mean)

uac.tr$Month <- format(as.Date(uac.tr$Date), "%Y-%m")
uac.tr$Month <- as.Date(paste0(as.character(uac.tr$Month), "-01"), format="%Y-%m-%d")
uac.m <- aggregate(.~Treatment+Month, data=uac.tr, FUN=mean)


## assign all variable values onto the time series
s1 <- merge(lai.m, sla.m, by=c("Month", "Treatment"), all=T)
s2 <- merge(s1, leafc.m, by=c("Month", "Treatment"), all=T)
s3 <- merge(s2, uac.m, by=c("Month", "Treatment"), all=T)
colnames(s3) <- c("Month", "Treatment", "date1", "a1", "b1", "c1",
                  "lai_avg", "lai_max", "lai_min", "lai_sd", "lai_pos", "lai_neg",
                  "date2", "a2", "b2", "c2",
                  "sla_avg", "sla_max", "sla_min", "sla_sd", "sla_pos", "sla_neg",
                  "date3", "a3", "b3", "c3",
                  "leafc_avg", "leafc_max", "leafc_min", "leafc_sd", "leafc_pos", "leafc_neg",
                  "date4", "a4", "b4", "c4",
                  "uac_avg", "uac_max", "uac_min", "uac_sd", "uac_pos", "uac_neg")

## split into ac and ec
ac <- subset(s3, Treatment == "aCO2")
ec <- subset(s3, Treatment == "eCO2")

## lai plot
p1 <- ggplot(ac, aes(Month))+
    geom_line(data=ac, aes(y=lai_avg, col="aCO2"))+
    geom_ribbon(data=ac,aes(ymin=lai_neg,ymax=lai_pos),fill="cyan", alpha=0.3)+
    geom_line(data=ec, aes(y=lai_avg, col="eCO2"))+
    geom_ribbon(data=ec,aes(ymin=lai_neg,ymax=lai_pos),fill="red", alpha=0.3)+
    labs(y="Leaf area index")+
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
                        labels=c(expression(aCO[2]), expression(eCO[2])))

## sla plot
p2 <- ggplot(NULL, aes(Month))+
    geom_segment(data=ac, aes(x = Month, y = sla_neg, xend = Month, yend = sla_pos), col="cyan")+
    geom_segment(data=ec, aes(x = Month, y = sla_neg, xend = Month, yend = sla_pos), col="red")+
    geom_point(data=ac, aes(y=sla_avg, col="aCO2"))+
    geom_point(data=ec, aes(y=sla_avg, col="eCO2"))+
    labs(x="Date", y=expression(paste("Specific leaf area (cm2 ", g^-1, ")")))+
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
                        labels=c(expression(aCO[2]), expression(eCO[2])))

## leaf c plot
p3 <- ggplot(ac, aes(Month))+
    geom_line(data=ac, aes(y=leafc_avg, col="aCO2"))+
    geom_ribbon(data=ac,aes(ymin=leafc_neg,ymax=leafc_pos),fill="cyan", alpha=0.3)+
    geom_line(data=ec, aes(y=leafc_avg, col="eCO2"))+
    geom_ribbon(data=ec,aes(ymin=leafc_neg,ymax=leafc_pos),fill="red", alpha=0.3)+
    labs(x="Date", y=expression(paste("Leaf pool (g C ", m^-2, ")")))+
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
                        labels=c(expression(aCO[2]), expression(eCO[2])))

## leaf c plot
p4 <- ggplot(ac, aes(Month))+
    geom_segment(data=ac, aes(x = Month, y = uac_neg, xend = Month, yend = uac_pos), col="cyan")+
    geom_segment(data=ec, aes(x = Month, y = uac_neg, xend = Month, yend = uac_pos), col="red")+
    geom_point(data=ac, aes(y=uac_avg, col="aCO2"))+
    geom_point(data=ec, aes(y=uac_avg, col="eCO2"))+
    labs(x="Date", y=expression(paste("Understorey aboveground pool (g C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="bottom")+
    scale_colour_manual(name="", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

## plot 
pdf("output/LAI_SLA_LeafC_UAC_time_series.pdf", width=16,height=10)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), 
                ggplotGrob(p3), ggplotGrob(p4), size = "last"))
dev.off()
