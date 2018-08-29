####### This script calls run.R and plot combined figures

#############################################################
#### Call run.R program
# source("run.R")

#### Source the function that makes treatment effect df
source("R/make_treatment_effect_df.R")
source("R/make_treatment_effect_df_2.R")

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
    geom_ribbon(data=lai.tr,aes(ymin=avg-sd,ymax=avg+sd, fill=factor(Treatment)))+
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
                 limits = as.Date(c('2012-09-01','2016-12-31')))

## sla plot
p2 <- ggplot(sla.tr, aes(Date))+
    #geom_ribbon(data=sla.tr,aes(ymin=neg,ymax=pos, fill=factor(Treatment)))+
    geom_point(data=sla.tr, aes(y=avg, color=factor(Treatment)))+
    geom_segment(data=sla.tr, aes(x=Date, y=avg-sd, xend=Date, yend=avg+sd, color=factor(Treatment)))+
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
                 limits = as.Date(c('2012-09-01','2016-12-31')))

## leaf c plot
p3 <- ggplot(leafc.tr, aes(Date))+
    geom_ribbon(data=leafc.tr,aes(ymin=avg-sd,ymax=avg+sd, fill=factor(Treatment)))+
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
                 limits = as.Date(c('2012-09-01','2016-12-31')))

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
    #geom_smooth(method="loess", aes(y=Total_g_C_m2, color=factor(Ring)))+
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
    #geom_ribbon(data=uac2.tr,aes(ymin=neg, ymax=pos, fill=factor(Treatment)))+
    #geom_segment(data=uac2.tr, aes(x=Date, y=neg, xend=Date, yend=pos, color=factor(Treatment)))+
    geom_smooth(method="loess", aes(y=avg, color=factor(Treatment), fill=factor(Treatment)))+
    geom_point(data=uac2.tr, aes(y=avg, color=factor(Treatment)))+
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
    #geom_smooth(method='lm')+
    #geom_dotplot(binaxis='wood_pool', stackdir='center', 
    #             position=position_dodge(1), binwidth=30, dotsize=2)+
    labs(x="Date", y=expression(paste(C[wood], " (kg C ", m^-2, ")")))+
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


#grid.labs <- c("(a)", "(b)", "(c)", "(d)")
grid.labs <- c("(a)", "(b)")

## plot 
pdf("output/Wood_C_time_series.pdf", width=9,height=7)
plot_grid(p1, p4, labels="", ncol=1, align="v", axis = "b")
grid.text(grid.labs,x = 0.12, y = c(0.97, 0.46),
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
crc1.tr <- make_treatment_effect_df_2(inDF=coarse_root_c_pool_1)
crc2.tr <- make_treatment_effect_df_2(inDF=coarse_root_c_pool_2)

## long format
crc1.tr$total_root_pool <- NULL
crc1.tr$Method <- "A"
crc2.tr$Method <- "B"
colnames(crc1.tr) <- c("Date", "Ring", "coarse_root_pool", "Treatment", "Method")
crc.tr <- rbind(crc1.tr, crc2.tr)

## wide format
temDF <- merge(crc1.tr, crc2.tr, by=c("Ring", "Date"), all=T)
crc.tr.w <- temDF[,c("Ring", "Date", "coarse_root_pool.x", "coarse_root_pool.y",
                     "Treatment.x")]
colnames(crc.tr.w) <- c("Ring", "Date", "M1", "M2", "Treatment")

## plotting
p1 <- ggplot(crc1.tr, aes(x=as.character(Date),y=coarse_root_pool,fill=as.factor(Treatment)))+
    geom_boxplot(position=position_dodge(1))+
    labs(x="Date", y=expression(paste(C[cr], " (g C ", m^-2, ")")))+
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
                      labels=c(expression(aCO[2]), expression(eCO[2])))

p2 <- ggplot(crc.tr.w)+
    geom_point(aes(x=M1, y=M2, color=factor(Treatment)))+
    labs(x=expression(paste(C[croot1], " (g C ", m^-2, ")")), 
         y=expression(paste(C[croot2], " (g C ", m^-2, ")")))+
    geom_abline(intercept = 0, slope = 1, color="grey", 
                linetype="dashed", size=1.5)+
    xlim(600, 1400)+ylim(600,1400)+
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
                      labels=c(expression(aCO[2]), expression(eCO[2])))

grid.labs <- c("(a)", "(b)")

pdf("output/coarseroot_biomass_method_comparison.pdf", width=6, height=8)
plot_grid(p1, p2, labels="", ncol=1, align="v", axis = "l")
grid.text(grid.labs,x = 0.2, y = c(0.95, 0.46),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()


###################---------------------######################
### Soil C pool, soil C content, Soil bulk density at different depths
##  generate treatment effect df for each variable
soil.bk.tr <- soil_bulk_density_variable
soil.bk.tr[soil.bk.tr$ring== 2|soil.bk.tr$ring==3|soil.bk.tr$ring==6,"Treatment"] <- "aCO2"
soil.bk.tr[soil.bk.tr$ring== 1|soil.bk.tr$ring==4|soil.bk.tr$ring==5,"Treatment"] <- "eCO2"

soil.bk.tr[soil.bk.tr$Depth == "0-10cm", "d.factor"] <- "0-10"
soil.bk.tr[soil.bk.tr$Depth == "10-20cm", "d.factor"] <- "10-20"
soil.bk.tr[soil.bk.tr$Depth == "20-30cm", "d.factor"] <- "20-30"

soil.bk.tr <- soil.bk.tr[complete.cases(soil.bk.tr),]

soilc.tr <- make_treatment_effect_df(inDF=soil_c_pool, v=3, cond=1)
soilr.tr <- make_treatment_effect_df(inDF=soil_respiration_flux, v=5, cond=1)


## plotting soil c pool
p1 <- ggplot(soilc.tr, aes(Date))+
    #geom_ribbon(data=soilc.tr,aes(ymin=neg, ymax=pos, fill=factor(Treatment)))+
    geom_point(data=soilc.tr, aes(y=avg, color=factor(Treatment)))+
    geom_smooth(method='lm',aes(y=avg, color=factor(Treatment), fill=factor(Treatment)))+
    labs(x="Date", y=expression(paste(C[soil], " (g C ", m^-2, ")")))+
    scale_x_date(date_breaks = "6 month", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-06-01','2015-01-01')))+
    ylim(c(0,6000))+
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

## bulk density
p2 <- ggplot(soil.bk.tr, aes(x=as.character(d.factor),y=bulk_density_kg_m3, fill=as.factor(d.factor)))+
    geom_bar(stat="identity", position="stack")+facet_grid(~ring, switch="x")+
    labs(x="Ring", y=expression(paste("Bulk density (kg ", m^-3, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="right")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Depth", values = c("0-10" = "green","10-20" = "orange", 
                                               "20-30" = "brown"),
                      labels=c("0-10cm","10-20cm","20-30cm"))


grid.labs <- c("(a)", "(b)")

## plot 
pdf("output/Soil_C_time_series.pdf", width=8,height=6)
plot_grid(p1, p2, labels="", ncol=1, align="v", axis = "l",
          rel_heights=c(1,0.8))
grid.text(grid.labs,x = 0.135, y = c(0.95, 0.4),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()

###################---------------------######################
### Soil bulk density and soil moisture at different depths
##  generate treatment effect df for each variable
soil.bk.tr <- soil_bulk_density_variable
soil.bk.tr[soil.bk.tr$ring== 2|soil.bk.tr$ring==3|soil.bk.tr$ring==6,"Treatment"] <- "aCO2"
soil.bk.tr[soil.bk.tr$ring== 1|soil.bk.tr$ring==4|soil.bk.tr$ring==5,"Treatment"] <- "eCO2"

soil.bk.tr[soil.bk.tr$Depth == "0-10cm", "d.factor"] <- "0-10"
soil.bk.tr[soil.bk.tr$Depth == "10-20cm", "d.factor"] <- "10-20"
soil.bk.tr[soil.bk.tr$Depth == "20-30cm", "d.factor"] <- "20-30"

soil.bk.tr <- soil.bk.tr[complete.cases(soil.bk.tr),]

### Soil moisture content at different depths and over time
## read in soil moisture data, takes a long time!
smDF <- prepare_soil_moisture_data(plot.image = F, monthly=T)

### Depth for ring 1, 3, 4, 5, 6
depth <- c(5, 30, 35, 75)


### combine all rings together
myDF <- rbind(smDF[[1]],smDF[[2]],smDF[[3]],smDF[[4]],smDF[[5]],smDF[[6]])
myDF$Month <- format(as.Date(myDF$Date), "%Y-%m")
myDF$Month <- as.Date(paste0(myDF$Month,"-1"), format = "%Y-%m-%d") 
myDF$Ring <- c(rep(1, length(smDF$R1$Date)),rep(2, length(smDF$R2$Date)),rep(3, length(smDF$R3$Date)),
               rep(4, length(smDF$R4$Date)),rep(5, length(smDF$R5$Date)),rep(6, length(smDF$R6$Date)))

### Calculate monthly average
mDF <- summaryBy(Theta5_1_Avg+Theta30_1_Avg+ThetaHL_1_Avg+Theta75_1_Avg~Month+Ring, 
                 data=myDF,FUN=mean, na.rm=T, keep.names=T)

### Monthly time series
m.series <- unique(mDF$Month)
x.m <- c(1:length(m.series))
d.l <- length(m.series)

### convert data into long format
plotDF <- data.frame(rep(m.series, 4), rep(depth, each=d.l), NA)
colnames(plotDF) <- c("Date", "Depth", "Theta")

### Subset data and assign data - ring 1
sub.mDF <- subset(mDF, Ring==1)
plotDF$Theta[plotDF$Depth==depth[1]] <- sub.mDF$Theta5_1_Avg
plotDF$Theta[plotDF$Depth==depth[2]] <- sub.mDF$Theta30_1_Avg
plotDF$Theta[plotDF$Depth==depth[3]] <- sub.mDF$ThetaHL_1_Avg
plotDF$Theta[plotDF$Depth==depth[4]] <- sub.mDF$Theta75_1_Avg

## Subset soil bulk density plot
bkDF <- subset(soil.bk.tr, ring==1)

## ggplot of soil bulk density
p1 <- ggplot(bkDF, aes(x=as.character(d.factor),y=bulk_density_kg_m3, fill=as.factor(d.factor)))+
    geom_bar(stat="identity", position="stack")+
    labs(x="Depth", y=expression(paste("Bulk density (kg ", m^-3, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_blank(),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Depth", values = c("0-10" = "green","10-20" = "orange", 
                                               "20-30" = "brown"),
                      labels=c("0-10cm","10-20cm","20-30cm"))

## ggplot of soil moisture profile
p2 <- ggplot(plotDF) + 
    aes(x = Date, y = Depth, z = Theta) + 
    ylim(5,75)+
    geom_tile(aes(fill=Theta)) + 
    stat_contour(aes(fill=..level..), geom="polygon", binwidth=0.005) + 
    geom_contour(color="white", alpha=0.5) +
    scale_fill_distiller(name="Soil Moisture", palette="RdBu", na.value="white", limits=c(40,0)) + 
    theme_bw()+
    labs(x="Date", y="Depth (cm)")+
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_blank(),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          legend.position="none")+
    scale_x_date(date_breaks = "1 year", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-09-01','2017-08-31')))


### Subset data and assign data - ring 3
sub.mDF <- subset(mDF, Ring==3)
plotDF$Theta[plotDF$Depth==depth[1]] <- sub.mDF$Theta5_1_Avg
plotDF$Theta[plotDF$Depth==depth[2]] <- sub.mDF$Theta30_1_Avg
plotDF$Theta[plotDF$Depth==depth[3]] <- sub.mDF$ThetaHL_1_Avg
plotDF$Theta[plotDF$Depth==depth[4]] <- sub.mDF$Theta75_1_Avg

## Subset soil bulk density plot
bkDF <- subset(soil.bk.tr, ring==3)

## ggplot of soil bulk density
p5 <- ggplot(bkDF, aes(x=as.character(d.factor),y=bulk_density_kg_m3, fill=as.factor(d.factor)))+
    geom_bar(stat="identity", position="stack")+
    labs(x="Depth", y=expression(paste("Bulk density (kg ", m^-3, ")")))+
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
    scale_fill_manual(name="Depth", values = c("0-10" = "green","10-20" = "orange", 
                                               "20-30" = "brown"),
                      labels=c("0-10cm","10-20cm","20-30cm"))

## ggplot of soil moisture profile
p6 <- ggplot(plotDF) + 
    aes(x = Date, y = Depth, z = Theta) + 
    ylim(5,75)+
    geom_tile(aes(fill=Theta)) + 
    stat_contour(aes(fill=..level..), geom="polygon", binwidth=0.005) + 
    geom_contour(color="white", alpha=0.5) +
    scale_fill_distiller(name="Soil Moisture", palette="RdBu", na.value="white", limits=c(40,0)) + 
    theme_bw()+
    labs(x="Date", y="Depth (cm)")+
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          legend.position="none")+
    scale_x_date(date_breaks = "1 year", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-09-01','2017-08-31')))


### Subset data and assign data - ring 4
sub.mDF <- subset(mDF, Ring==4)
plotDF$Theta[plotDF$Depth==depth[1]] <- sub.mDF$Theta5_1_Avg
plotDF$Theta[plotDF$Depth==depth[2]] <- sub.mDF$Theta30_1_Avg
plotDF$Theta[plotDF$Depth==depth[3]] <- sub.mDF$ThetaHL_1_Avg
plotDF$Theta[plotDF$Depth==depth[4]] <- sub.mDF$Theta75_1_Avg

## Subset soil bulk density plot
bkDF <- subset(soil.bk.tr, ring==4)

## ggplot of soil bulk density
p7 <- ggplot(bkDF, aes(x=as.character(d.factor),y=bulk_density_kg_m3, fill=as.factor(d.factor)))+
    geom_bar(stat="identity", position="stack")+
    labs(x="Depth", y=expression(paste("Bulk density (kg ", m^-3, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_blank(),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Depth", values = c("0-10" = "green","10-20" = "orange", 
                                               "20-30" = "brown"),
                      labels=c("0-10cm","10-20cm","20-30cm"))

## ggplot of soil moisture profile
p8 <- ggplot(plotDF) + 
    aes(x = Date, y = Depth, z = Theta) + 
    ylim(5,75)+
    geom_tile(aes(fill=Theta)) + 
    stat_contour(aes(fill=..level..), geom="polygon", binwidth=0.005) + 
    geom_contour(color="white", alpha=0.5) +
    scale_fill_distiller(name="Soil Moisture", palette="RdBu", na.value="white", limits=c(40,0)) + 
    theme_bw()+
    labs(x="Date", y="Depth (cm)")+
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_blank(),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          legend.position="none")+
    scale_x_date(date_breaks = "1 year", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-09-01','2017-08-31')))


### Subset data and assign data - ring 5
sub.mDF <- subset(mDF, Ring==5)
plotDF$Theta[plotDF$Depth==depth[1]] <- sub.mDF$Theta5_1_Avg
plotDF$Theta[plotDF$Depth==depth[2]] <- sub.mDF$Theta30_1_Avg
plotDF$Theta[plotDF$Depth==depth[3]] <- sub.mDF$ThetaHL_1_Avg
plotDF$Theta[plotDF$Depth==depth[4]] <- sub.mDF$Theta75_1_Avg

## Subset soil bulk density plot
bkDF <- subset(soil.bk.tr, ring==5)

## ggplot of soil bulk density
p9 <- ggplot(bkDF, aes(x=as.character(d.factor),y=bulk_density_kg_m3, fill=as.factor(d.factor)))+
    geom_bar(stat="identity", position="stack")+
    labs(x="Depth", y=expression(paste("Bulk density (kg ", m^-3, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_blank(),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Depth", values = c("0-10" = "green","10-20" = "orange", 
                                               "20-30" = "brown"),
                      labels=c("0-10cm","10-20cm","20-30cm"))

## ggplot of soil moisture profile
p10 <- ggplot(plotDF) + 
    aes(x = Date, y = Depth, z = Theta) + 
    ylim(5,75)+
    geom_tile(aes(fill=Theta)) + 
    stat_contour(aes(fill=..level..), geom="polygon", binwidth=0.005) + 
    geom_contour(color="white", alpha=0.5) +
    scale_fill_distiller(name="Soil Moisture", palette="RdBu", na.value="white", limits=c(40,0)) + 
    theme_bw()+
    labs(x="Date", y="Depth (cm)")+
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_blank(),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          legend.position="none")+
    scale_x_date(date_breaks = "1 year", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-09-01','2017-08-31')))

### convert data into long format
plotDF <- data.frame(rep(m.series, 4), rep(depth, each=d.l), NA)
colnames(plotDF) <- c("Date", "Depth", "Theta")

### Subset data and assign data - ring 6
sub.mDF <- subset(mDF, Ring==6)
plotDF$Theta[plotDF$Depth==depth[1]] <- sub.mDF$Theta5_1_Avg
plotDF$Theta[plotDF$Depth==depth[2]] <- sub.mDF$Theta30_1_Avg
plotDF$Theta[plotDF$Depth==depth[3]] <- sub.mDF$ThetaHL_1_Avg
plotDF$Theta[plotDF$Depth==depth[4]] <- sub.mDF$Theta75_1_Avg

## Subset soil bulk density plot
bkDF <- subset(soil.bk.tr, ring==6)

## ggplot of soil bulk density
p11 <- ggplot(bkDF, aes(x=as.character(d.factor),y=bulk_density_kg_m3, fill=as.factor(d.factor)))+
    geom_bar(stat="identity", position="stack")+
    labs(x="Depth", y=expression(paste("Bulk density (kg ", m^-3, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_blank(),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Depth", values = c("0-10" = "green","10-20" = "orange", 
                                               "20-30" = "brown"),
                      labels=c("0-10cm","10-20cm","20-30cm"))

## ggplot of soil moisture profile
p12 <- ggplot(plotDF) + 
    aes(x = Date, y = Depth, z = Theta) + 
    ylim(5,75)+
    geom_tile(aes(fill=Theta)) + 
    stat_contour(aes(fill=..level..), geom="polygon", binwidth=0.005) + 
    geom_contour(color="white", alpha=0.5) +
    scale_fill_distiller(name="Soil Moisture", palette="RdBu", na.value="white", limits=c(40,0)) + 
    theme_bw()+
    labs(x="Date", y="Depth (cm)")+
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_blank(),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          legend.position="bottom")+
    scale_x_date(date_breaks = "1 year", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-09-01','2017-08-31')))


### convert data into long format - note, depth for ring 2 is different
depth <- c(5, 30, 44, 75)
plotDF <- data.frame(rep(m.series, 4), rep(depth, each=d.l), NA)
colnames(plotDF) <- c("Date", "Depth", "Theta")

### Subset data and assign data - ring 2
sub.mDF <- subset(mDF, Ring==2)
plotDF$Theta[plotDF$Depth==depth[1]] <- sub.mDF$Theta5_1_Avg
plotDF$Theta[plotDF$Depth==depth[2]] <- sub.mDF$Theta30_1_Avg
plotDF$Theta[plotDF$Depth==depth[3]] <- sub.mDF$ThetaHL_1_Avg
plotDF$Theta[plotDF$Depth==depth[4]] <- sub.mDF$Theta75_1_Avg

## Subset soil bulk density plot
bkDF <- subset(soil.bk.tr, ring==2)

## ggplot of soil bulk density
p3 <- ggplot(bkDF, aes(x=as.character(d.factor),y=bulk_density_kg_m3, fill=as.factor(d.factor)))+
    geom_bar(stat="identity", position="stack")+
    labs(x="Depth", y=expression(paste("Bulk density (kg ", m^-3, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_blank(),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Depth", values = c("0-10" = "green","10-20" = "orange", 
                                               "20-30" = "brown"),
                      labels=c("0-10cm","10-20cm","20-30cm"))

## ggplot of soil moisture profile
p4 <- ggplot(plotDF) + 
    aes(x = Date, y = Depth, z = Theta) + 
    ylim(5,75)+
    geom_tile(aes(fill=Theta)) + 
    stat_contour(aes(fill=..level..), geom="polygon", binwidth=0.005) + 
    geom_contour(color="white", alpha=0.5) +
    scale_fill_distiller(name="Soil Moisture", palette="RdBu", na.value="white", limits=c(40,0)) + 
    theme_bw()+
    labs(x="Date", y="Depth (cm)")+
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_blank(),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          legend.position="none")+
    scale_x_date(date_breaks = "1 year", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-09-01','2017-08-31')))



#grid.labs <- c("(a)", "(b)")
require(cowplot)

## plot 
pdf("output/Soil_Profile_bk_sm.pdf", width=8,height=12)
plot_grid(p1, p2, 
          p3, p4, 
          p5, p6, 
          p7, p8, 
          p9, p10, 
          p11, p12,
          labels="", ncol=2, align="v", axis = "l",
          rel_widths=c(1,1.8))
#grid.text(grid.labs,x = 0.1, y = c(0.97, 0.48),
#          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()

###################---------------------######################
#### plotting fineroot C pool, at different depths
###  generate treatment effect df for each variable
froot.tr <- fineroot_c_pool
froot.tr[froot.tr$Ring== 2|froot.tr$Ring==3|froot.tr$Ring==6,"Treatment"] <- "aCO2"
froot.tr[froot.tr$Ring== 1|froot.tr$Ring==4|froot.tr$Ring==5,"Treatment"] <- "eCO2"

frDF <- reshape(froot.tr,idvar=c("Ring", "Date", "Treatment"),
                varying=list(3:5),
                direction="long")
l <- length(froot.tr$Date)

frDF$Depth <- c(rep(c("total", "0-10cm","10-30cm"), each=l))

plotDF <- frDF[frDF$Depth != "total",]

p <- ggplot(plotDF, aes(x=Treatment, y=fineroot_pool, fill=Depth))+
    geom_bar(stat="identity", position="stack")+facet_grid(~Date)+
    labs(x="Treatment", y=expression(paste(C[froot], " (g C ", m^-2, ")")))+
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
    scale_fill_manual(name="Depth", values = c("0-10cm" = "green", "10-30cm" = "orange"),
                      labels=c("0-10cm","10-30cm"))+
    scale_x_discrete(limit=c("aCO2", "eCO2"),
                     labels=c(expression(aCO[2]), expression(eCO[2])))

pdf("output/fineroot_c_pool_of_depth.pdf", width=8, height=6)
plot(p)
dev.off()

###################---------------------######################
#### plotting soil content and soil c pool
###  generate treatment effect df for each variable
froot.tr <- fineroot_c_pool
froot.tr[froot.tr$Ring== 2|froot.tr$Ring==3|froot.tr$Ring==6,"Treatment"] <- "aCO2"
froot.tr[froot.tr$Ring== 1|froot.tr$Ring==4|froot.tr$Ring==5,"Treatment"] <- "eCO2"

frDF <- reshape(froot.tr,idvar=c("Ring", "Date", "Treatment"),
                varying=list(3:5),
                direction="long")
l <- length(froot.tr$Date)

frDF$Depth <- c(rep(c("total", "0-10cm","10-30cm"), each=l))

plotDF <- frDF[frDF$Depth != "total",]

p <- ggplot(plotDF, aes(x=Treatment, y=fineroot_pool, fill=Depth))+
    geom_bar(stat="identity", position="stack")+facet_grid(~Date)+
    labs(x="Treatment", y=expression(paste(C[froot], " (g C ", m^-2, ")")))+
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
    scale_fill_manual(name="Depth", values = c("0-10cm" = "green", "10-30cm" = "orange"),
                      labels=c("0-10cm","10-30cm"))+
    scale_x_discrete(limit=c("aCO2", "eCO2"),
                     labels=c(expression(aCO[2]), expression(eCO[2])))

pdf("output/fineroot_c_pool_of_depth.pdf", width=8, height=6)
plot(p)
dev.off()

###################---------------------######################
### Soil C pool, pH, CN and CP ratios, by depth
soilc.tr <- make_soil_carbon_pool(bk_density=soil_bulk_density_variable,
                                       return="by_depths")

soilc.tr[soilc.tr$Ring== 2|soilc.tr$Ring==3|soilc.tr$Ring==6,"Treatment"] <- "aCO2"
soilc.tr[soilc.tr$Ring== 1|soilc.tr$Ring==4|soilc.tr$Ring==5,"Treatment"] <- "eCO2"

plotDF1 <- summaryBy(soil_carbon_pool+ph+cn+cp~Date+Treatment+Depth, 
                    data=soilc.tr,FUN=mean, na.rm=T, keep.names=T)

## plotting soil c pool
p1 <- ggplot(plotDF1, aes(x=Treatment, y=soil_carbon_pool, fill=Depth))+
    geom_bar(stat="identity", position="stack")+facet_grid(~Date)+
    labs(x="Treatment", y=expression(paste(C[soil], " (g C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=10),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="right")+
    scale_fill_manual(name="Depth", values = c("0-10cm" = "green", "10-20cm" = "orange", "20-30cm" = "brown"),
                      labels=c("0-10cm","10-20cm","20-30cm"))+
    scale_x_discrete(limit=c("aCO2", "eCO2"),
                     labels=c(expression(aCO[2]), expression(eCO[2])))

## prepare soil ph df
plotDF2 <- make_treatment_effect_df(inDF=soilc.tr, v=5, cond=2)

## ph
p2 <- ggplot(plotDF2, aes(x=Date))+
    geom_point(data=plotDF2, aes(y=avg, color=factor(Treatment)))+
    geom_segment(data=plotDF2, aes(x=Date, y=avg-sd, xend=Date, yend=avg+sd, color=factor(Treatment)))+
    labs(x="Treatment", y="pH")+
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
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_date(date_breaks = "6 months", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-05-01','2014-04-30')))


## soil CN 
plotDF3 <- make_treatment_effect_df(inDF=soilc.tr, v=6, cond=2)
plotDF3 <- plotDF3[complete.cases(plotDF3),]
p3 <- ggplot(plotDF3, aes(x=Date))+
    geom_point(data=plotDF3, aes(y=avg, color=factor(Treatment)))+
    geom_segment(data=plotDF3, aes(x=Date, y=avg-sd, xend=Date, yend=avg+sd, color=factor(Treatment)))+
    labs(x="Treatment", y="CN ratio")+
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
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_date(date_breaks = "6 months", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-05-01','2014-04-30')))

## soil CP 
plotDF4 <- make_treatment_effect_df(inDF=soilc.tr, v=7, cond=2)
plotDF4 <- plotDF4[complete.cases(plotDF4),]

p4 <- ggplot(plotDF4, aes(x=Date))+
    geom_point(data=plotDF4, aes(y=avg, color=factor(Treatment)))+
    geom_segment(data=plotDF4, aes(x=Date, y=avg-sd, xend=Date, yend=avg+sd, color=factor(Treatment)))+
    labs(x="Date", y="CP ratio")+
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
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_date(date_breaks = "6 months", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-05-01','2014-04-30')))

grid.labs <- c("(a)", "(b)", "(c)", "(d)")

require(gridExtra)

## plot 
pdf("output/Soil_C_pool_time_series.pdf", width=12,height=8)
grid.arrange(p1, rbind(ggplotGrob(p2), 
                       ggplotGrob(p3), ggplotGrob(p4), size="last"), 
             ncol=1)
grid.text(grid.labs,x = c(0.09, 0.075, 0.075, 0.075), y = c(0.93, 0.46, 0.31, 0.15),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()

###################---------------------######################
### Microbial and Mycorrhizal pools
mic.tr <- make_treatment_effect_df(inDF=microbial_c_pool, v=3, cond=1)
myc.tr <- make_treatment_effect_df(inDF=mycorrhizal_c_pool, v=3, cond=1)

myc.prop <- (mycorrhizal_c_pool$mycorrhizal_c_pool[mycorrhizal_c_pool$Date=="2014-03-10"] / microbial_c_pool$microbial_pool[microbial_c_pool$Date=="2014-03-10"])
myc.prop <- data.frame(c(1:6), myc.prop, c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2"))
colnames(myc.prop) <- c("Ring", "myc.prop", "Trt")


## Plot microbial
p1 <- ggplot(mic.tr, aes(x=Date))+
    geom_point(data=mic.tr, aes(y=avg, color=factor(Treatment)))+
    geom_segment(data=mic.tr, aes(x=Date, y=avg-sd, xend=Date, yend=avg+sd, color=factor(Treatment)))+
    labs(x="Date", y=expression(paste(C[mic], " (g C ", m^-2, ")")))+
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
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_date(date_breaks = "6 months", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-07-01','2016-02-28')))

p2 <- ggplot(myc.tr, aes(x=Date))+
    geom_point(data=myc.tr, aes(y=avg, color=factor(Treatment)))+
    geom_segment(data=myc.tr, aes(x=Date, y=avg-sd, xend=Date, yend=avg+sd, color=factor(Treatment)))+
    labs(x="Date", y=expression(paste(C[myc], " (g C ", m^-2, ")")))+
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
    scale_x_date(date_breaks = "6 months", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-07-01','2016-02-28')))

p3 <- ggplot(myc.prop, aes(x=as.character(Ring), y=myc.prop*100, fill=factor(Trt)))+
    geom_bar(stat = "identity")+
    labs(x="Ring", y="Mycorrhizal proportion (%)")+
    theme_linedraw() +
    ylim(0, 15)+
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="right")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

plot(p3)

grid.labs <- c("(a)", "(b)")

## plot 
pdf("output/microbial_and_mycorrhizal_pool.pdf", width=9,height=6)
#grid.newpage()
#grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p3), size="last"))
plot_grid(p1, p3, labels="", ncol=1, align="v", axis = "l")
grid.text(grid.labs, x = 0.14, y = c(0.95, 0.45),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()

###################---------------------######################
### Leaf litter
lit.prod.tr <- leaflitter_flux
lit.prod.tr[lit.prod.tr$Ring== 2|lit.prod.tr$Ring==3|lit.prod.tr$Ring==6,"Treatment"] <- "aCO2"
lit.prod.tr[lit.prod.tr$Ring== 1|lit.prod.tr$Ring==4|lit.prod.tr$Ring==5,"Treatment"] <- "eCO2"

### Decomposition rate
decp.rt <- make_leaflitter_decomposition_rate()
decp.rt[decp.rt$Ring== 2|decp.rt$Ring==3|decp.rt$Ring==6,"Treatment"] <- "aCO2"
decp.rt[decp.rt$Ring== 1|decp.rt$Ring==4|decp.rt$Ring==5,"Treatment"] <- "eCO2"

### Litter, CWD and insect
lit.tr <- make_treatment_effect_df(inDF=leaflitter_pool, v=6, cond=1)
cwd.tr <- make_treatment_effect_df(inDF=standing_dead_c_pool, v=3, cond=2)

### Plot decomposition rate
p1 <- ggplot(decp.rt, aes(x=Treatment,y=coef,fill=Treatment))+
    geom_boxplot(position=position_dodge(1))+
    labs(x="Treatment", y=expression(paste(k[leaf], " (", d^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

### Plot leaf litter production
p2 <- ggplot(lit.prod.tr, aes(x=Treatment,y=leaf_flux,fill=Treatment))+
    geom_boxplot(position=position_dodge(1))+
    labs(x="Treatment", y=expression(paste("Leaf litterfall (mg C ", m^-2, d^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_line(color="grey"),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

## Plot leaf litter pool
p3 <- ggplot(lit.tr, aes(x=Date))+
    #geom_point(data=lit.tr, aes(y=avg, color=factor(Treatment)))+
    #geom_segment(data=lit.tr, aes(x=Date, y=avg-sd, xend=Date, yend=avg+sd, color=factor(Treatment)))+
    geom_ribbon(data=lit.tr,aes(ymin=avg-sd,ymax=avg+sd, fill=factor(Treatment)))+
    geom_line(data=lit.tr, aes(y=avg, color=factor(Treatment)))+
    labs(x="Date", y=expression(paste(C[lit], " (g C ", m^-2, ")")))+
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
    scale_x_date(date_breaks = "6 months", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2013-01-01','2016-12-31')))

## plot 
pdf("output/leaflitter_pool_and_decomposition_rate.pdf", width=10,height=8)
top_row <- plot_grid(p1, p2, align = 'h')
plot_grid(top_row, p3, ncol = 1, rel_heights = c(1, 1.2))
grid.labs <- c("(a)", "(b)", "(c)")

grid.text(grid.labs,x = c(0.14, 0.625, 0.13), y = c(0.97, 0.97, 0.50),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()


###################---------------------######################
### Insect pool
insDF <- insect_pool
insDF[insDF$Ring== 2|insDF$Ring==3|insDF$Ring==6,"Treatment"] <- "aCO2"
insDF[insDF$Ring== 1|insDF$Ring==4|insDF$Ring==5,"Treatment"] <- "eCO2"

plotDF1 <- summaryBy(insect_pool~Treatment, data=insDF, FUN=mean, na.rm=T, keep.names=T)
plotDF2 <- summaryBy(insect_pool~Treatment, data=insDF, FUN=sd, na.rm=T, keep.names=T)
plotDF1$sd <- plotDF2$insect_pool

### Plot 
p1 <- ggplot(plotDF1)+
    geom_point(aes(x=Treatment, y=insect_pool, color=Treatment), size=1.5)+
    geom_segment(data=plotDF1, aes(x=Treatment, y=insect_pool-sd, 
                                   xend=Treatment, yend=insect_pool+sd, color=factor(Treatment)))+
    labs(x="", y=expression(paste(C[ins], " (g C", m^-2, ")")))+
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
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_discrete("", 
                     labels=c(expression(aCO[2]),
                              expression(eCO[2])))

pdf("output/insect_pool.pdf", width=5,height=4)
plot(p1)
dev.off()

###################---------------------######################
### NPP leaf, twigs, bark and seed
lit.prod.tr <- leaflitter_flux
lit.prod.tr[lit.prod.tr$Ring== 2|lit.prod.tr$Ring==3|lit.prod.tr$Ring==6,"Treatment"] <- "aCO2"
lit.prod.tr[lit.prod.tr$Ring== 1|lit.prod.tr$Ring==4|lit.prod.tr$Ring==5,"Treatment"] <- "eCO2"

leaf.lit <- make_treatment_effect_df(inDF=leaflitter_flux, v=6, cond=1)
seed.lit <- make_treatment_effect_df(inDF=leaflitter_flux, v=5, cond=1)
bark.lit <- make_treatment_effect_df(inDF=leaflitter_flux, v=4, cond=1)
twig.lit <- make_treatment_effect_df(inDF=leaflitter_flux, v=3, cond=1)


### plot
p1 <- ggplot(leaf.lit, aes(x=Date))+
    geom_ribbon(data=leaf.lit,aes(ymin=avg-sd,ymax=avg+sd, fill=factor(Treatment)))+
    geom_line(aes(x=Date,y=avg,color=as.factor(Treatment)))+
    labs(x="", y=expression(paste(NPP[ol], " (mg C ", m^-2, d^-1, ")")))+
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
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

p2 <- ggplot(twig.lit, aes(x=Date))+
    geom_ribbon(data=twig.lit,aes(ymin=avg-sd,ymax=avg+sd, fill=factor(Treatment)))+
    geom_line(aes(x=Date,y=avg,color=as.factor(Treatment)))+
    labs(x="", y=expression(paste(NPP[twig], " (mg C ", m^-2, d^-1, ")")))+
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
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p3 <- ggplot(bark.lit, aes(x=Date))+
    geom_ribbon(data=bark.lit,aes(ymin=avg-sd,ymax=avg+sd, fill=factor(Treatment)))+
    geom_line(aes(x=Date,y=avg,color=as.factor(Treatment)))+
    labs(x="", y=expression(paste(NPP[bark], " (mg C ", m^-2, d^-1, ")")))+
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
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p4 <- ggplot(seed.lit, aes(x=Date))+
    geom_ribbon(data=seed.lit,aes(ymin=avg-sd,ymax=avg+sd, fill=factor(Treatment)))+
    geom_line(aes(x=Date,y=avg,color=as.factor(Treatment)))+
    labs(x="Date", y=expression(paste(NPP[seed], " (mg C ", m^-2, d^-1, ")")))+
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
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))


grid.labs <- c("(a)", "(b)", "(c)", "(d)")

## plot 
pdf("output/NPP_litter.pdf", width=8,height=12)
plot_grid(p1, p2, 
          p3, p4, 
          labels="", ncol=1, align="v", axis = "l", 
          rel_heights=c(0.7, 0.7, 0.7, 1))
grid.text(grid.labs,x = 0.15, y = c(0.97, 0.74, 0.5, 0.29),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()
