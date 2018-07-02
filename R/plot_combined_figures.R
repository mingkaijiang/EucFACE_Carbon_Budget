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
### Wood C pool
##  generate treatment effect df for each variable
woodc.tr <- make_treatment_effect_df_2(inDF=wood_c_pool)

wood.stock <- make_dbh_and_wood_stock(include.bark.effect=TRUE)
wood.stock.tr <- make_treatment_effect_df_2(inDF=wood.stock)

wood.stock.sum <- summaryBy(Wood_Stock~Ring+Date, data=wood.stock.tr, FUN=sum)
colnames(wood.stock.sum) <- c("Ring", "Date", "Wood_Stock_g")
wood.stock.sum$Wood_Stock_t <- wood.stock.sum$Wood_Stock_g / 1000 / 1000
wood.stock.sum.mean <- summaryBy(Wood_Stock_t~Ring, data=wood.stock.sum, FUN=mean)
colnames(wood.stock.sum.mean) <- c("Ring", "Wood_Stock")
wood.stock.sum.mean <- make_treatment_effect_df_2(inDF=wood.stock.sum.mean)  

woodc.tr$wood_pool <- woodc.tr$wood_pool/1000

## wood c plot
p1 <- ggplot(woodc.tr, aes(x=as.character(Date),y=wood_pool,fill=Treatment))+
    geom_boxplot(position=position_dodge(1))+
    geom_dotplot(binaxis='wood_pool', stackdir='center', 
                 position=position_dodge(1), binwidth=30, dotsize=2)+
    labs(x="Date", y=expression(paste(C[wood], " (kg C ", m^-2, ")")))+
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
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

## wood stock per ring (g C, no area involved)
p2 <- ggplot(wood.stock.tr, aes(x=as.character(Date),y=Wood_Stock/1000,fill=Treatment))+
    geom_boxplot(position=position_dodge(1))+
    labs(x="Date", y=expression(paste(C[wood], " (kg C ", tree^-1, ")")))+
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
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

p3 <- ggplot(wood.stock.sum.mean, aes(x=as.character(Ring),y=Wood_Stock, fill=as.factor(Treatment)))+
    geom_bar(stat="identity")+
    labs(x="Ring", y=expression(paste(C[wood], " (t C ", ring^-1, ")")))+
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
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))


wood.stock.sum <- summaryBy(Wood_Stock~Ring+Date+Class, data=wood.stock.tr, FUN=sum)
colnames(wood.stock.sum) <- c("Ring", "Date", "Class", "Wood_Stock_g")
wood.stock.sum$Wood_Stock_t <- wood.stock.sum$Wood_Stock_g / 1000 / 1000
wood.stock.sum.mean <- summaryBy(Wood_Stock_t~Ring+Class, data=wood.stock.sum, FUN=mean)
colnames(wood.stock.sum.mean) <- c("Ring", "Class", "Wood_Stock")
wood.stock.sum.mean <- make_treatment_effect_df_2(inDF=wood.stock.sum.mean)  

p4 <- ggplot(wood.stock.sum.mean, aes(x=as.character(Ring),y=Wood_Stock, fill=as.factor(Class)))+
    geom_bar(stat="identity", position="stack")+
    labs(x="Ring", y=expression(paste(C[wood], " (t C ", ring^-1, ")")))+
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
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Class", values = c("Dominant" = "green","Codominant" = "orange", 
                                              "Suppressed" = "brown"))


grid.labs <- c("(a)", "(b)", "(c)", "(d)")

## plot 
pdf("output/Wood_C_time_series.pdf", width=12,height=14)
plot_grid(p1, p2, p3, p4, labels="", ncol=1, align="v", axis = "l")
grid.text(grid.labs,x = 0.1, y = c(0.97, 0.72, 0.48, 0.23),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()

###################---------------------######################
### Wood C tree plot
##  generate treatment effect df for each variable
wood.stock <- make_dbh_and_wood_stock(include.bark.effect=TRUE)
wood.stock.tr <- make_treatment_effect_df_2(inDF=wood.stock)
wood.stock.tr$Wood_Stock <- wood.stock.tr$Wood_Stock / 1000

require(plot3D)
tmp <- subset(wood.stock.tr, Date == "2012-12-20")

#### Get xlim, ylim and zlim
#h.range <- c(min(tmp$Height), max(tmp$Height))
#d.range <- c(min(tmp$Diameter), max(tmp$Diameter))
#b.range <- c(min(tmp$Wood_Stock), max(tmp$Wood_Stock))
#
#### Exact individual rings
#R1 <- subset(tmp, Ring == 1)
#R2 <- subset(tmp, Ring == 2)
#R3 <- subset(tmp, Ring == 3)
#R4 <- subset(tmp, Ring == 4)
#R5 <- subset(tmp, Ring == 5)
#R6 <- subset(tmp, Ring == 6)
#
#p1 <- with(R1, scatter3D(Height, Diameter, Wood_Stock,  type = "h", phi=0,
#               ticktype = "detailed", pch = 19, cex = 0.5,
#               xlab = "HEight (m)", ylab = "Diameter (cm)", 
#               zlab = "Biomass (kg C)", xlim = h.range,
#               ylim = d.range, zlim = b.range))
#
#p2 <- with(R2, scatter3D(Height, Diameter, Wood_Stock,  type = "h", phi=0,
#                         ticktype = "detailed", pch = 19, cex = 0.5,
#                         xlab = "HEight (m)", ylab = "Diameter (cm)", 
#                         zlab = "Biomass (kg C)", xlim = h.range,
#                         ylim = d.range, zlim = b.range))
#
#p3 <- with(R3, scatter3D(Height, Diameter, Wood_Stock,  type = "h", phi=0,
#                         ticktype = "detailed", pch = 19, cex = 0.5,
#                         xlab = "HEight (m)", ylab = "Diameter (cm)", 
#                         zlab = "Biomass (kg C)", xlim = h.range,
#                         ylim = d.range, zlim = b.range))
#
#p4 <- with(R4, scatter3D(Height, Diameter, Wood_Stock,  type = "h", phi=0,
#                         ticktype = "detailed", pch = 19, cex = 0.5,
#                         xlab = "HEight (m)", ylab = "Diameter (cm)", 
#                         zlab = "Biomass (kg C)", xlim = h.range,
#                         ylim = d.range, zlim = b.range))
#
#p5 <- with(R5, scatter3D(Height, Diameter, Wood_Stock,  type = "h", phi=0,
#                         ticktype = "detailed", pch = 19, cex = 0.5,
#                         xlab = "HEight (m)", ylab = "Diameter (cm)", 
#                         zlab = "Biomass (kg C)", xlim = h.range,
#                         ylim = d.range, zlim = b.range))
#
#p6 <- with(R6, scatter3D(Height, Diameter, Wood_Stock,  type = "h", phi=0,
#                         ticktype = "detailed", pch = 19, cex = 0.5,
#                         xlab = "HEight (m)", ylab = "Diameter (cm)", 
#                         zlab = "Biomass (kg C)", xlim = h.range,
#                         ylim = d.range, zlim = b.range))

# c1 <- colorRamps::blue2red(6)
c1 <- c("#FFAA00", "#0000FF", "#0080FF", "#FF5500", "#FF0000", "#00FFFF")
colors <- c1[as.numeric(tmp$Ring)]

require(scatterplot3d)

pdf("output/3d_wood_relationships.pdf")
s3d <- scatterplot3d(tmp[,4:6], pch = 16, color=colors, type="h",
                     xlab="Height (m)", ylab = "Diameter (cm)",
                     zlab="Biomass (kg C)")

legend(s3d$xyz.convert(0, 10, 800), legend = c("R1","R2","R3","R4","R5","R6"),
       col = c("#FFAA00", "#0000FF", "#0080FF", "#FF5500", "#FF0000", "#00FFFF"), 
       pch = 16, horiz = F)
dev.off()


#with(tmp, scatter3d(x = Height, y = Diameter, z = Wood_Stock, 
#                    groups = as.factor(as.character(tmp$Ring)),
#                    surface=F, type="h"))

###################---------------------######################
### Coarse root C pool
##  generate treatment effect df for each variable