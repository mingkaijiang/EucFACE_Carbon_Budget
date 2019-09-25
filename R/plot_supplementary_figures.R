####### This script calls run.R and plot combined figures

#############################################################
#### Call run.R program
# source("run.R")

#### Source the function that makes treatment effect df
source("R/make_treatment_effect_df.R")
source("R/make_treatment_effect_df_2.R")

#### library
require(grid)

ring.col <- c("red","blue","blue","red","red","blue")


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
    geom_rect(aes(xmin=as.Date("2012-09-01"), xmax=as.Date("2013-03-01"), ymin=0.0, ymax=2.5), 
              fill="lightgrey", alpha=0.4)+   
    geom_segment(aes(x=as.Date("2012-09-01"), xend=as.Date("2013-03-01"), y=2.5, yend=2.5))+
    geom_segment(aes(x=as.Date("2012-09-01"), xend=as.Date("2012-09-01"), y=2.45, yend=2.55))+
    geom_segment(aes(x=as.Date("2013-03-01"), xend=as.Date("2013-03-01"), y=2.45, yend=2.55))+
    geom_ribbon(data=lai.tr,aes(ymin=avg-se,ymax=avg+se, fill=factor(Treatment)))+
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
          panel.grid.major=element_blank(),
          legend.position="none")+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_date(date_breaks = "1 year", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-09-01','2016-12-31')))

#plot(p1)
## sla plot
p2 <- ggplot(sla.tr, aes(Date))+
    geom_rect(aes(xmin=as.Date("2012-09-01"), xmax=as.Date("2013-03-01"), ymin=0.0, ymax=80), 
              fill="lightgrey", alpha=0.4)+  
    #geom_ribbon(data=sla.tr,aes(ymin=neg,ymax=pos, fill=factor(Treatment)))+
    geom_point(data=sla.tr, aes(y=avg, color=factor(Treatment)))+
    geom_segment(data=sla.tr, aes(x=Date, y=avg-se, xend=Date, yend=avg+se, color=factor(Treatment)))+
    labs(x="Date", y=expression(paste("SLA (", cm^2, " ", g^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
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
    geom_rect(aes(xmin=as.Date("2012-09-01"), xmax=as.Date("2013-03-01"), ymin=0.0, ymax=300), 
              fill="lightgrey", alpha=0.4)+  
    geom_ribbon(data=leafc.tr,aes(ymin=avg-se,ymax=avg+se, fill=factor(Treatment)))+
    geom_line(data=leafc.tr, aes(y=avg, color=factor(Treatment)))+
    labs(x="Date", y=expression(paste(C[ol], " (g C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
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
pdf("output/Figure_S3.pdf", width=10,height=8)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), 
                ggplotGrob(p3), size="last"))
grid.text(grid.labs,x = 0.09, y = c(0.96, 0.66, 0.37),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()

###################---------------------######################
### Understorey C, compare two methods
#uac1.tr <- make_treatment_effect_df(inDF=understorey_aboveground_c_pool, v=5, cond=1)
#uac1.live.tr <- make_treatment_effect_df(inDF=understorey_aboveground_c_pool, v=3, cond=1)
#uac1.dead.tr <- make_treatment_effect_df(inDF=understorey_aboveground_c_pool, v=4, cond=1)
uac2.tr <- make_treatment_effect_df(inDF=understorey_aboveground_c_pool_2, v=3, cond=1)

uac2.tr$year <- year(uac2.tr$Date)
uac2.yr <- summaryBy(avg~year+Treatment, FUN=c(mean, se), data=uac2.tr, keep.names=T)

p1 <- ggplot(uac2.yr, aes(x=year, y=avg.mean))+
    geom_bar(stat = "identity", aes(fill=Treatment), position="dodge") +
    geom_errorbar(aes(ymax=(avg.mean+avg.se), ymin=(avg.mean+avg.se), color=factor(Treatment)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="Date", y=expression(paste(C[ua2], " (g C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    #ylim(0, 0.3)+
    #scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue3", "eCO2" = "red2"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p1 <- ggplot(uac2.yr, aes(x=as.character(year), y=avg.mean))+
    geom_bar(stat = "identity", aes(fill=Treatment), position="dodge")+
    geom_errorbar(aes(ymax=avg.mean+avg.se, ymin=avg.mean-avg.se, color=factor(Treatment)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(C[ua], " (g C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="bottom")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

plot(p1)

pdf("output/Figure_S5.pdf")
plot(p1)
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
    labs(x="Date", y=expression(paste(C[stem], " (kg C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

## wood stock per ring (g C, no area involved)
p2 <- ggplot(wood.stock.tr, aes(x=as.character(Date),y=Wood_Stock/1000,fill=Treatment))+
    geom_boxplot(position=position_dodge(1))+
    labs(x="Date", y=expression(paste(C[stem], " (kg C ", tree^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="bottom")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

p3 <- ggplot(wood.stock.sum.mean, aes(x=as.character(Ring),y=Wood_Stock, fill=as.factor(Treatment)))+
    geom_bar(stat="identity")+
    labs(x="Plot", y=expression(paste(C[stem], " (t C ", plot^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
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
### convert unit
wood.stock.sum.mean$Wood_Stock_gCm2 <- wood.stock.sum.mean$Wood_Stock*1000000/ring_area
wood.stock.sum.mean$Lab[wood.stock.sum.mean$Class=="Codominant"] <- "2"
wood.stock.sum.mean$Lab[wood.stock.sum.mean$Class=="Dominant"] <- "1"
wood.stock.sum.mean$Lab[wood.stock.sum.mean$Class=="Suppressed"] <- "3"

p4 <- ggplot(wood.stock.sum.mean, aes(x=as.character(Ring),y=Wood_Stock_gCm2, fill=as.factor(Lab)))+
    geom_bar(stat="identity", position="stack")+
    labs(x="Plot", y=expression(paste(C[stem], " (g C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Class", values = c("1" = "green","2" = "orange", 
                                              "3" = "brown"),
                      label = c("Dominant", "Co-dominant", "Suppressed"))

### Time series of wood plot per ring
woodc.ring <- summaryBy(Wood_Stock_t~Ring+Date, FUN=sum, data=wood.stock.sum, keep.names=T)
woodc.ring$Wood_Stock_gCm2 <- woodc.ring$Wood_Stock_t*1000000/ring_area

p5 <- ggplot(woodc.ring, aes(x=Date, y=Wood_Stock_gCm2, color=as.factor(Ring)))+
    geom_point()+
    geom_smooth(mapping=aes(x=Date, y=Wood_Stock_gCm2, color=as.factor(Ring)), method=lm)+
    labs(x="Date", y=expression(paste(C[stem], " (g C ", m^-2, ")")))+
    theme_linedraw() + ylim(2000, 7000)+
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_color_manual(name="Plot", values = c("pink", "cyan", "blue", "red", "orange", "darkblue"),
                      labels=c("Ring1", "Ring2", "Ring3", "Ring4", "Ring5", "Ring6"))


#grid.labs <- c("(a)", "(b)", "(c)", "(d)")
grid.labs <- c("(a)", "(b)")

## plot 
pdf("output/Figure_S4.pdf", width=6,height=6)
plot_grid(p4, p5, labels="", ncol=1, align="v", axis = "b")
grid.text(grid.labs,x = 0.2, y = c(0.96, 0.46),
          gp=gpar(fontsize=14, col="black", fontface="bold"))
dev.off()


###################---------------------######################
### Plot coarse root and fine root together for each ring
frDF <- summaryBy(fineroot_pool+fineroot_0_10_cm+fineroot_10_30_cm~Ring, FUN=mean, data=fineroot_c_pool,
                  keep.names=T)

frDF.se <- summaryBy(fineroot_pool+fineroot_0_10_cm+fineroot_10_30_cm~Ring, FUN=se, data=fineroot_c_pool,
                  keep.names=T)

crDF <- summaryBy(coarse_root_pool~Ring, FUN=mean, data=coarse_root_c_pool_1, keep.names=T)
crDF.se <- summaryBy(coarse_root_pool~Ring, FUN=se, data=coarse_root_c_pool_1, keep.names=T)

plotDF <- data.frame(rep(1:6, 3), NA, NA, NA)
colnames(plotDF) <- c("Ring", "Value", "Tissue", "Sd")
plotDF$Component <- rep(c("fr0_10", "fr10_30", "cr"), each=6)

plotDF$Value[plotDF$Component=="fr0_10"] <- frDF$fineroot_0_10_cm
plotDF$Value[plotDF$Component=="fr10_30"] <- frDF$fineroot_10_30_cm
plotDF$Value[plotDF$Component=="cr"] <- crDF$coarse_root_pool

plotDF$Se[plotDF$Component=="fr0_10"] <- frDF.se$fineroot_0_10_cm
plotDF$Se[plotDF$Component=="fr10_30"] <- frDF.se$fineroot_10_30_cm
plotDF$Se[plotDF$Component=="cr"] <- crDF.se$coarse_root_pool


p <- ggplot(plotDF, aes(x=as.character(Ring), y=Value, fill=Component))+
    geom_bar(stat="identity", position="stack")+
    labs(x="Plot", y=expression(paste(C[root], " (g C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_fill_manual(limit=c("fr0_10", "fr10_30", "cr"),
                      values=c("orange", "green", "brown"),
                      labels=c(expression(C[fr0_10]), expression(C[fr10_30]), expression(C[croot])))

#plot(p)

pdf("output/Figure_S6.pdf", width=8, height=6)
plot(p)
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

soilc.ring <- summaryBy(soil_carbon_pool~Ring, FUN=mean, data=soil_c_pool, keep.names=T)
soilc.ring$Trt[soilc.ring$Ring%in%c(2,3,6)] <- "aCO2"
soilc.ring$Trt[soilc.ring$Ring%in%c(1,4,5)] <- "eCO2"
soilc.ring.se <- summaryBy(soil_carbon_pool~Ring, FUN=se, data=soil_c_pool, keep.names=T)
soilc.ring$se <- soilc.ring.se$soil_carbon_pool
names(soilc.ring)[4] <- "se"

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
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

## bulk density
p2 <- ggplot(soil.bk.tr, aes(x=as.character(ring),y=bulk_density_kg_m3, fill=as.factor(d.factor)))+
    geom_bar(stat="identity", position=position_dodge(width=0.95))+#facet_grid(~ring, switch="x")+
    geom_errorbar(data=soil.bk.tr, mapping=aes(x=as.character(ring),
                                               ymin=bulk_density_kg_m3-bulk_density_kg_m3_se, 
                                               ymax=bulk_density_kg_m3+bulk_density_kg_m3_se),
                  position = position_dodge(0.9),
                  width=0.2, size=1, color="black")+
    labs(x="Plot", y=expression(paste("BK (kg ", m^-3, ")")))+
    theme_linedraw() + 
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    coord_cartesian(ylim = c(1000,2000)) +
    scale_fill_manual(name="Depth", values = c("0-10" = "green","10-20" = "orange", 
                                               "20-30" = "brown"),
                      labels=c("0-10cm","10-20cm","20-30cm"))

p3 <-  ggplot(soilc.ring)+
    geom_bar(aes(x=as.character(Ring),y=soil_carbon_pool, fill=Trt),
             stat="identity", position="stack")+
    geom_errorbar(data=soilc.ring, mapping=aes(x=as.character(Ring),
                      ymin=soil_carbon_pool-se, ymax=soil_carbon_pool+se),
                  width=0.2, size=1, color="black")+
    labs(x="Plot", y=expression(paste(C[soil], " (g C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    coord_cartesian(ylim = c(1000,3000)) +
    scale_fill_manual(name="Treatment", 
                      values = c("aCO2" = "blue", "eCO2" = "red"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

soilc.tr <- make_soil_carbon_pool(bk_density=soil_bulk_density_variable,
                                  return="by_depths")
soilc.ph <- summaryBy(ph~Ring, FUN=mean, data=soilc.tr, keep.names=T, na.rm=T)
soilc.ph.se <- summaryBy(ph~Ring, FUN=se, data=soilc.tr, keep.names=T, na.rm=T)
soilc.ph$se <- soilc.ph.se$ph
soilc.ph$Trt[soilc.ph$Ring%in%c(2,3,6)] <- "aCO2"
soilc.ph$Trt[soilc.ph$Ring%in%c(1,4,5)] <- "eCO2"

soilc.cn <- summaryBy(cn~Ring, FUN=mean, data=soilc.tr, keep.names=T, na.rm=T)
soilc.cn.se <- summaryBy(cn~Ring, FUN=se, data=soilc.tr, keep.names=T, na.rm=T)
soilc.cn$se <- soilc.cn.se$cn
soilc.cn$Trt[soilc.cn$Ring%in%c(2,3,6)] <- "aCO2"
soilc.cn$Trt[soilc.cn$Ring%in%c(1,4,5)] <- "eCO2"

soilc.cp <- summaryBy(cp~Ring, FUN=mean, data=soilc.tr, keep.names=T, na.rm=T)
soilc.cp.se <- summaryBy(cp~Ring, FUN=se, data=soilc.tr, keep.names=T, na.rm=T)
soilc.cp$se <- soilc.cp.se$cp
soilc.cp$Trt[soilc.cp$Ring%in%c(2,3,6)] <- "aCO2"
soilc.cp$Trt[soilc.cp$Ring%in%c(1,4,5)] <- "eCO2"

## ph
p4 <- ggplot(soilc.ph)+
    geom_bar(aes(x=as.character(Ring),y=ph, fill=Trt),
             stat="identity", position="stack")+
    geom_errorbar(data=soilc.ph, mapping=aes(x=as.character(Ring),
                                               ymin=ph-se, ymax=ph+se),
                  width=0.2, size=1, color="black")+
    labs(x="Plot", y="Soil pH (water)")+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    coord_cartesian(ylim = c(5,6)) +
    scale_fill_manual(name="Treatment", 
                      values = c("aCO2" = "blue", "eCO2" = "red"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))


## soil CN 
p5 <- ggplot(soilc.cn)+
    geom_bar(aes(x=as.character(Ring),y=cn, fill=Trt),
             stat="identity", position="stack")+
    geom_errorbar(data=soilc.cn, mapping=aes(x=as.character(Ring),
                                             ymin=cn-se, ymax=cn+se),
                  width=0.2, size=1, color="black")+
    labs(x="Plot", y="Soil CN")+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    coord_cartesian(ylim = c(10,20)) +
    scale_fill_manual(name="Treatment", 
                      values = c("aCO2" = "blue", "eCO2" = "red"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))


## soil CP 
p6 <- ggplot(soilc.cp)+
    geom_bar(aes(x=as.character(Ring),y=cp, fill=Trt),
             stat="identity", position="stack")+
    geom_errorbar(data=soilc.cp, mapping=aes(x=as.character(Ring),
                                             ymin=cp-se, ymax=cp+se),
                  width=0.2, size=1, color="black")+
    labs(x="Plot", y="Soil CP")+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    coord_cartesian(ylim = c(100,300)) +
    scale_fill_manual(name="Treatment", 
                      values = c("aCO2" = "blue", "eCO2" = "red"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)")

## plot 
pdf("output/Figure_S2.pdf", width=8,height=10)
plot_grid(p2, p3, p4, p5, p6, labels="", ncol=1, align="v", axis = "l",
          rel_heights = c(0.8, 0.8, 0.8, 0.8, 1))
grid.text(grid.labs,x = 0.135, y = c(0.97, 0.79, 0.59, 0.4, 0.215),
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
    geom_segment(data=mic.tr, aes(x=Date, y=avg-se, xend=Date, yend=avg+se, color=factor(Treatment)))+
    labs(x="Date", y=expression(paste(C[micr], " (g C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_date(date_breaks = "6 months", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2012-07-01','2016-02-28')))+
    ylim(0, 200)

p2 <- ggplot(myc.tr, aes(x=Date))+
    geom_point(data=myc.tr, aes(y=avg, color=factor(Treatment)))+
    geom_segment(data=myc.tr, aes(x=Date, y=avg-se, xend=Date, yend=avg+se, color=factor(Treatment)))+
    labs(x="Date", y=expression(paste(C[myc], " (g C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
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
    labs(x="Plot", y="Mycorrhizal proportion (%)")+
    theme_linedraw() +
    ylim(0, 15)+
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

#plot(p3)

grid.labs <- c("(a)", "(b)")

## plot 
pdf("output/Figure_S7.pdf", width=9,height=6)
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
lit.prod <- summaryBy(leaf_flux~Ring+Treatment, data=lit.prod.tr, FUN=mean, keep.names=T)
lit.prod.se <- summaryBy(leaf_flux~Ring+Treatment, data=lit.prod.tr, FUN=se, keep.names=T)
lit.prod$se <- lit.prod.se$leaf_flux

### Decomposition rate
decp.rt <- make_leaflitter_decomposition_rate()
decp.rt[decp.rt$Ring== 2|decp.rt$Ring==3|decp.rt$Ring==6,"Treatment"] <- "aCO2"
decp.rt[decp.rt$Ring== 1|decp.rt$Ring==4|decp.rt$Ring==5,"Treatment"] <- "eCO2"

decp.avg <- summaryBy(coef~Treatment, data=decp.rt, FUN=mean, keep.names=T)
decp.se <- summaryBy(coef~Treatment, data=decp.rt, FUN=se, keep.names=T)
decp.avg$se <- decp.se$coef

### Litter, CWD and insect
lit.tr <- make_treatment_effect_df(inDF=leaflitter_pool, v=6, cond=1)

lit.pool <- leaflitter_pool
lit.pool[lit.pool$Ring== 2|lit.pool$Ring==3|lit.pool$Ring==6,"Treatment"] <- "aCO2"
lit.pool[lit.pool$Ring== 1|lit.pool$Ring==4|lit.pool$Ring==5,"Treatment"] <- "eCO2"

lit.avg <- summaryBy(leaflitter_pool~Ring+Treatment, data=lit.pool, FUN=mean, keep.names=T)
lit.se <- summaryBy(leaflitter_pool~Ring+Treatment, data=lit.pool, FUN=se, keep.names=T)
lit.avg$se <- lit.se$leaflitter_pool

### Plot decomposition rate
p1 <- ggplot(decp.rt, aes(x=as.character(Ring), y=coef, fill=factor(Treatment)))+
    geom_point(data=decp.rt, mapping=aes(x=as.character(Ring), y=coef,fill=factor(Treatment)), 
               size=4, shape=21)+
    labs(x="Plot", y=expression(paste(k[leaf], " (", d^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

### Plot leaf litter production
p2 <- ggplot(lit.prod.tr, aes(x=Treatment,y=leaf_flux,fill=Treatment))+
    geom_boxplot(position=position_dodge(1))+
    labs(x="Treatment", y=expression(paste("Leaf litterfall (mg C ", m^-2, " ", d^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_discrete("", 
                     labels=c(expression(aCO[2]),
                              expression(eCO[2])))

## Plot leaf litter pool
p3 <- ggplot(lit.tr, aes(x=Date))+
    #geom_point(data=lit.tr, aes(y=avg, color=factor(Treatment)))+
    #geom_segment(data=lit.tr, aes(x=Date, y=avg-sd, xend=Date, yend=avg+sd, color=factor(Treatment)))+
    geom_ribbon(data=lit.tr,aes(ymin=avg-se,ymax=avg+se, fill=factor(Treatment)))+
    #geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Treatment)), 
    #              position = position_dodge(0.9), width=0.2, size=0.4) +
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
          panel.grid.major=element_blank(),
          legend.position="bottom")+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_date(date_breaks = "6 months", 
                 date_labels="%b-%Y",
                 limits = as.Date(c('2013-01-01','2016-12-31')))

p4 <- ggplot(lit.prod, aes(x=as.character(Ring), y=leaf_flux, fill=factor(Treatment)))+
    geom_point(data=lit.prod, mapping=aes(x=Ring, y=leaf_flux,fill=factor(Treatment)), 
               size=4, shape=21)+
    geom_errorbar(aes(ymax=leaf_flux+se, ymin=leaf_flux-se), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="Plot", y=expression(paste("Leaf litterfall (mg C ", m^-2, " ", d^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

p5 <- ggplot(lit.avg, aes(x=as.character(Ring), y=leaflitter_pool, fill=factor(Treatment)))+
    geom_point(data=lit.avg, mapping=aes(x=Ring, y=leaflitter_pool,fill=factor(Treatment)), 
               size=4, shape=21)+
    geom_errorbar(aes(ymax=leaflitter_pool+se, ymin=leaflitter_pool-se), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="Plot", y=expression(paste(C[lit], " (g C ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="bottom")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

grid.labs <- c("(a)", "(b)", "(c)")

## plot 
pdf("output/Figure_S8.pdf", width=6,height=8)
#top_row <- plot_grid(p1, p2, align = 'h')
#plot_grid(top_row, p3, ncol = 1, rel_heights = c(1, 1.2))
#grid.text(grid.labs,x = c(0.14, 0.625, 0.13), y = c(0.97, 0.97, 0.50),
#          gp=gpar(fontsize=16, col="black", fontface="bold"))
plot_grid(p1, p4, p5, labels="", ncol=1, align="v", axis = "l", rel_heights=c(1,1,1.5))
grid.text(grid.labs, x = 0.21, y = c(0.95, 0.66, 0.38),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()


###################---------------------######################
### Insect pool
insDF <- insect_pool
insDF[insDF$Ring== 2|insDF$Ring==3|insDF$Ring==6,"Treatment"] <- "aCO2"
insDF[insDF$Ring== 1|insDF$Ring==4|insDF$Ring==5,"Treatment"] <- "eCO2"

plotDF1 <- summaryBy(insect_pool~Treatment, data=insDF, FUN=mean, na.rm=T, keep.names=T)
plotDF2 <- summaryBy(insect_pool~Treatment, data=insDF, FUN=se, na.rm=T, keep.names=T)
plotDF1$se <- plotDF2$insect_pool
plotDF1$pos <- plotDF1$insect_pool + plotDF1$se
plotDF1$neg <- plotDF1$insect_pool - plotDF1$se

### Plot 
p1 <- ggplot(plotDF1, aes(Treatment, insect_pool))+
    #geom_point(aes(x=Treatment, y=insect_pool, color=Treatment), size=1.5)+
    #geom_errorbar(data=plotDF1, aes(x=Treatment, y=insect_pool-se, 
    #                               xend=Treatment, yend=insect_pool+se, color=factor(Treatment)))+
    geom_bar(stat = "identity", aes(fill=Treatment), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Treatment)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(C[ins], " (g C", " ", m^-2, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_discrete("", 
                     labels=c(expression(aCO[2]),
                              expression(eCO[2])))

pdf("output/Figure_S9.pdf", width=5,height=4)
plot(p1)
dev.off()

###################---------------------######################
### Overstorey GPP and understorey GPP
o.gpp.tr <- overstorey_gpp_flux
u.gpp.tr <- understorey_gpp_flux


p1 <- ggplot(data = o.gpp.tr, aes(x = interaction(year, Ring, lex.order = TRUE), 
                            y = GPP)) +
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
    #annotate(geom = "text", x = seq_len(nrow(o.gpp.tr)), y = -100, label = o.gpp.tr$Ring, size = 4) +
    #annotate(geom = "text", x = 3.5 + 6 * (0:3), y = -200, label = unique(o.gpp.tr$year), size = 6) +
    labs(x="Year", y=expression(paste(GPP[o], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="top")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))


p2 <- ggplot(data = u.gpp.tr, aes(x = interaction(year, Ring, lex.order = TRUE), 
                                  y = GPP)) +
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
    annotate(geom = "text", x = seq_len(nrow(u.gpp.tr)), y = -100, label = u.gpp.tr$Ring, size = 4) +
    annotate(geom = "text", x = 3.5 + 6 * (0:3), y = -200, label = unique(u.gpp.tr$year), size = 6) +
    coord_cartesian(ylim = c(0, 750), expand = FALSE, clip = "off") +
    labs(x="Year", y=expression(paste(GPP[u], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
          panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))
    
pdf("output/Figure_S10.pdf", width=9,height=6)
plot_grid(p1, p2, labels="", ncol=1, align="v", axis = "l",
          rel_heights=c(1, 1.1))
grid.text(grid.labs, x = 0.14, y = c(0.85, 0.45),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
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

### calculate annual rate
lit.prod.tr$year <- year(lit.prod.tr$Date)
#yr.list <- unique(lit.prod.tr$year)
yr.list <- c(2013:2016)

plotDF <- data.frame(rep(yr.list, each=6), rep(c(1:6), by=length(yr.list)), NA, NA, NA, NA)
colnames(plotDF) <- c("Year", "Ring", "leaf_lit", "seed_lit", "bark_lit", "twig_lit")


for (i in yr.list) {
    for (j in 1:6) {
        plotDF$leaf_lit[plotDF$Year==i&plotDF$Ring==j] <- round(with(lit.prod.tr[lit.prod.tr$year==i&lit.prod.tr$Ring==j,],
                                                                     sum(leaf_flux*ndays,na.rm=T)/sum(ndays,na.rm=T)) * conv ,2)
        plotDF$seed_lit[plotDF$Year==i&plotDF$Ring==j] <- round(with(lit.prod.tr[lit.prod.tr$year==i&lit.prod.tr$Ring==j,],
                                                                     sum(seed_flux*ndays,na.rm=T)/sum(ndays,na.rm=T)) * conv ,2)
        plotDF$twig_lit[plotDF$Year==i&plotDF$Ring==j] <- round(with(lit.prod.tr[lit.prod.tr$year==i&lit.prod.tr$Ring==j,],
                                                                     sum(twig_flux*ndays,na.rm=T)/sum(ndays,na.rm=T)) * conv ,2)
        plotDF$bark_lit[plotDF$Year==i&plotDF$Ring==j] <- round(with(lit.prod.tr[lit.prod.tr$year==i&lit.prod.tr$Ring==j,],
                                                                     sum(bark_flux*ndays,na.rm=T)/sum(ndays,na.rm=T)) * conv ,2)
    }
}

plotDF[plotDF$Ring%in%c(2,3,6),"Treatment"] <- "aCO2"
plotDF[plotDF$Ring%in%c(1,4,5),"Treatment"] <- "eCO2"

plotDF2 <- summaryBy(leaf_lit+seed_lit+bark_lit+twig_lit~Year+Treatment, data=plotDF, FUN=mean, keep.names=T)
plotDF3 <- summaryBy(leaf_lit+seed_lit+bark_lit+twig_lit~Year+Treatment, data=plotDF, FUN=se, keep.names=T)
plotDF2$leaf_se <- plotDF3$leaf_lit
plotDF2$seed_se <- plotDF3$seed_lit
plotDF2$twig_se <- plotDF3$twig_lit
plotDF2$bark_se <- plotDF3$bark_lit

### plot
p1 <- ggplot(leaf.lit, aes(x=Date))+
    geom_ribbon(data=leaf.lit,aes(ymin=avg-se,ymax=avg+se, fill=factor(Treatment)))+
    geom_line(aes(x=Date,y=avg,color=as.factor(Treatment)))+
    labs(x="", y=expression(paste(NPP[ol], " (mg C ", m^-2, " ", d^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))

p2 <- ggplot(twig.lit, aes(x=Date))+
    geom_ribbon(data=twig.lit,aes(ymin=avg-se,ymax=avg+se, fill=factor(Treatment)))+
    geom_line(aes(x=Date,y=avg,color=as.factor(Treatment)))+
    labs(x="", y=expression(paste(NPP[twig], " (mg C ", m^-2," ",  d^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p3 <- ggplot(bark.lit, aes(x=Date))+
    geom_ribbon(data=bark.lit,aes(ymin=avg-se,ymax=avg+se, fill=factor(Treatment)))+
    geom_line(aes(x=Date,y=avg,color=as.factor(Treatment)))+
    labs(x="", y=expression(paste(NPP[bark], " (mg C ", m^-2, " ", d^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p4 <- ggplot(seed.lit, aes(x=Date))+
    geom_ribbon(data=seed.lit,aes(ymin=avg-se,ymax=avg+se, fill=factor(Treatment)))+
    geom_line(aes(x=Date,y=avg,color=as.factor(Treatment)))+
    labs(x="Date", y=expression(paste(NPP[seed], " (mg C ", m^-2, " ", d^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="bottom")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))


### plot
p5 <- ggplot(plotDF2, aes(x=Year, y=leaf_lit))+
    geom_bar(stat = "identity", aes(fill=Treatment), position="dodge")+
    geom_errorbar(aes(ymax=leaf_lit+leaf_se, ymin=leaf_lit-leaf_se, color=factor(Treatment)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(NPP[ol], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p6 <- ggplot(plotDF2, aes(x=Year, y=twig_lit))+
    geom_bar(stat = "identity", aes(fill=Treatment), position="dodge")+
    geom_errorbar(aes(ymax=twig_lit+twig_se, ymin=twig_lit-twig_se, color=factor(Treatment)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(NPP[twig], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p7 <- ggplot(plotDF2, aes(x=Year, y=bark_lit))+
    geom_bar(stat = "identity", aes(fill=Treatment), position="dodge")+
    geom_errorbar(aes(ymax=bark_lit+bark_se, ymin=bark_lit-bark_se, color=factor(Treatment)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(NPP[bark], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p8 <- ggplot(plotDF2, aes(x=Year, y=seed_lit))+
    geom_bar(stat = "identity", aes(fill=Treatment), position="dodge")+
    geom_errorbar(aes(ymax=seed_lit+seed_se, ymin=seed_lit-seed_se, color=factor(Treatment)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(NPP[seed], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="bottom")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

grid.labs <- c("(a)", "(b)", "(c)", "(d)")

## plot 
pdf("output/Figure_S12.pdf", width=8,height=12)
plot_grid(p5, p6, 
          p7, p8, 
          labels="", ncol=1, align="v", axis = "l", 
          rel_heights=c(0.7, 0.7, 0.7, 1))
grid.text(grid.labs,x = 0.12, y = c(0.97, 0.74, 0.51, 0.29),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()


###################---------------------######################
### NPP wood
wood.prod <- make_treatment_effect_df(inDF=wood_production_flux, v=5, cond=1)

wood.prod$avg <- wood.prod$avg * 365 / 1000
wood.prod$pos <- wood.prod$pos * 365 / 1000
wood.prod$neg <- wood.prod$neg * 365 / 1000

wood.prod$Yr <- year(wood.prod$Date)

### plot
p1 <- ggplot(wood.prod, aes(x=as.character(Yr), y=avg))+
    geom_bar(stat = "identity", aes(fill=Treatment), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Treatment)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(NPP[stem], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

## plot 
pdf("output/Figure_S13.pdf", width=6,height=4)
plot(p1)
dev.off()

###################---------------------######################
### NPP for fineroot and coarseroot

### Coarseroot
croot.prod <- coarse_root_production_flux_1
croot.prod$Yr <- year(croot.prod$Date)
y.list <- unique(croot.prod$Yr)
croot.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(croot.ann) <- c("Ring", "Yr", "value")
for (i in 1:6) {
    for (j in y.list) {
        croot.ann$value[croot.ann$Ring==i&croot.ann$Yr==j] <- with(croot.prod[croot.prod$Yr==j&croot.prod$Ring==i, ],
                                                                 sum(coarse_root_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
croot.ann$Trt[croot.ann$Ring%in%c(2,3,6)] <- "aCO2"
croot.ann$Trt[croot.ann$Ring%in%c(1,4,5)] <- "eCO2"
croot.avg <- summaryBy(value~Trt, FUN=mean, data=croot.ann, keep.names=T, na.rm=T)
croot.se <- summaryBy(value~Trt, FUN=se, data=croot.ann, keep.names=T, na.rm=T)
croot.avg$se <- croot.se$value
croot.avg$component <- "coarseroot"

### fineroot
froot.prod <- fineroot_production_flux
froot.prod$Yr <- year(froot.prod$Date)
y.list <- unique(froot.prod$Yr)
froot.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(froot.ann) <- c("Ring", "Yr", "value")
for (i in 1:6) {
    for (j in y.list) {
        froot.ann$value[froot.ann$Ring==i&froot.ann$Yr==j] <- with(froot.prod[froot.prod$Yr==j&froot.prod$Ring==i, ],
                                                                   sum(fineroot_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
froot.ann$Trt[froot.ann$Ring%in%c(2,3,6)] <- "aCO2"
froot.ann$Trt[froot.ann$Ring%in%c(1,4,5)] <- "eCO2"
froot.avg <- summaryBy(value~Trt, FUN=mean, data=froot.ann, keep.names=T, na.rm=T)
froot.se <- summaryBy(value~Trt, FUN=se, data=froot.ann, keep.names=T, na.rm=T)
froot.avg$se <- froot.se$value
froot.avg$component <- "fineroot"

root.plot <- rbind(croot.avg, froot.avg)
root.plot$pos <- with(root.plot, value + se)
root.plot$neg <- with(root.plot, value - se)

### Plot
p1 <- ggplot(root.plot, aes(x=component, y=value))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(NPP[root], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="bottom")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

pdf("output/Figure_S14.pdf", width=6,height=4)
plot(p1)
dev.off()


###################---------------------######################
### NPP for understorey production
und.prod <- understorey_aboveground_production_flux
und.prod$Yr <- year(und.prod$Date)
y.list <- unique(und.prod$Yr)
und.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(und.ann) <- c("Ring", "Yr", "value")
for (i in 1:6) {
    for (j in y.list) {
        und.ann$value[und.ann$Ring==i&und.ann$Yr==j] <- with(und.prod[und.prod$Yr==j&und.prod$Ring==i, ],
                                                                   sum(understorey_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
und.ann$Trt[und.ann$Ring%in%c(2,3,6)] <- "aCO2"
und.ann$Trt[und.ann$Ring%in%c(1,4,5)] <- "eCO2"
und.avg <- summaryBy(value~Trt+Yr, FUN=mean, data=und.ann, keep.names=T, na.rm=T)
und.se <- summaryBy(value~Trt+Yr, FUN=se, data=und.ann, keep.names=T, na.rm=T)
und.avg$se <- und.se$value
und.avg$pos <- with(und.avg, value + se)
und.avg$neg <- with(und.avg, value - se)

### Plot
p1 <- ggplot(und.avg, aes(x=as.character(Yr), y=value))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(NPP[ua], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="bottom")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

pdf("output/Figure_S15.pdf", width=6,height=4)
plot(p1)
dev.off()


###################---------------------######################
### NPP leaf consumed by herbivory, frass production, and herbivory respiration flux

## NPP leaf consumed
hb.prod <- herbivory_leaf_consumption_flux
hb.prod$Yr <- year(hb.prod$Date)
y.list <- unique(hb.prod$Yr)
hb.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(hb.ann) <- c("Ring", "Yr", "value")
for (i in 1:6) {
    for (j in y.list) {
        hb.ann$value[hb.ann$Ring==i&hb.ann$Yr==j] <- with(hb.prod[hb.prod$Yr==j&hb.prod$Ring==i, ],
                                                             sum(herbivory_leaf_consumption_flux*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
hb.ann$Trt[hb.ann$Ring%in%c(2,3,6)] <- "aCO2"
hb.ann$Trt[hb.ann$Ring%in%c(1,4,5)] <- "eCO2"
hb.avg <- summaryBy(value~Trt+Yr, FUN=mean, data=hb.ann, keep.names=T, na.rm=T)
hb.se <- summaryBy(value~Trt+Yr, FUN=se, data=hb.ann, keep.names=T, na.rm=T)
hb.avg$se <- hb.se$value
hb.avg$pos <- with(hb.avg, value + se)
hb.avg$neg <- with(hb.avg, value - se)

### Rhb
hb.resp <- herbivory_respiration_flux
hb.resp$Yr <- year(hb.resp$Date)
y.list <- unique(hb.resp$Yr)
hb.resp.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(hb.resp.ann) <- c("Ring", "Yr", "value")
for (i in 1:6) {
    for (j in y.list) {
        hb.resp.ann$value[hb.resp.ann$Ring==i&hb.resp.ann$Yr==j] <- with(hb.resp[hb.resp$Yr==j&hb.resp$Ring==i, ],
                                                          sum(respiration_flux*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
hb.resp.ann$Trt[hb.resp.ann$Ring%in%c(2,3,6)] <- "aCO2"
hb.resp.ann$Trt[hb.resp.ann$Ring%in%c(1,4,5)] <- "eCO2"
hb.resp.avg <- summaryBy(value~Trt+Yr, FUN=mean, data=hb.resp.ann, keep.names=T, na.rm=T)
hb.resp.se <- summaryBy(value~Trt+Yr, FUN=se, data=hb.resp.ann, keep.names=T, na.rm=T)
hb.resp.avg$se <- hb.resp.se$value
hb.resp.avg$pos <- with(hb.resp.avg, value + se)
hb.resp.avg$neg <- with(hb.resp.avg, value - se)


### Frass production
frass.prod <- frass_production_flux
frass.prod$Yr <- year(frass.prod$Date)
y.list <- unique(frass.prod$Yr)
frass.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(frass.ann) <- c("Ring", "Yr", "value")
for (i in 1:6) {
    for (j in y.list) {
        frass.ann$value[frass.ann$Ring==i&frass.ann$Yr==j] <- with(frass.prod[frass.prod$Yr==j&frass.prod$Ring==i, ],
                                                          sum(frass_production_flux*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
frass.ann$Trt[frass.ann$Ring%in%c(2,3,6)] <- "aCO2"
frass.ann$Trt[frass.ann$Ring%in%c(1,4,5)] <- "eCO2"
frass.avg <- summaryBy(value~Trt+Yr, FUN=mean, data=frass.ann, keep.names=T, na.rm=T)
frass.se <- summaryBy(value~Trt+Yr, FUN=se, data=frass.ann, keep.names=T, na.rm=T)
frass.avg$se <- frass.se$value
frass.avg$pos <- with(frass.avg, value + se)
frass.avg$neg <- with(frass.avg, value - se)

### Plot
p1 <- ggplot(hb.avg, aes(x=as.character(Yr), y=value))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(NPP[ins], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    ylim(0,50)+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p2 <- ggplot(hb.resp.avg, aes(x=as.character(Yr), y=value))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(R[ins], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    ylim(0,50)+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p3 <- ggplot(frass.avg, aes(x=as.character(Yr), y=value))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste("Frass (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="bottom")+
    ylim(0,50)+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))


grid.labs <- c("(a)", "(b)", "(c)")


pdf("output/Figure_S16.pdf", width=6,height=8)
plot_grid(p1, p2, p3, labels="", ncol=1, align="v", axis = "l",
          rel_heights=c(0.7, 0.7, 1))
grid.text(grid.labs, x = 0.14, y = c(0.96, 0.675, 0.38),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()

###################---------------------######################
### Autotrophic respiration

## overstorey leaf respiration
o.leaf.resp <- overstorey_leaf_respiration_flux
o.leaf.resp$Trt[o.leaf.resp$Ring%in%c(2,3,6)] <- "aCO2"
o.leaf.resp$Trt[o.leaf.resp$Ring%in%c(1,4,5)] <- "eCO2"
o.leaf.resp.avg <- summaryBy(Rfoliage~Trt+year, FUN=mean, data=o.leaf.resp, keep.names=T, na.rm=T)
o.leaf.resp.se <- summaryBy(Rfoliage~Trt+year, FUN=se, data=o.leaf.resp, keep.names=T, na.rm=T)
o.leaf.resp.avg$se <- o.leaf.resp.se$Rfoliage
o.leaf.resp.avg$pos <- with(o.leaf.resp.avg, Rfoliage + se)
o.leaf.resp.avg$neg <- with(o.leaf.resp.avg, Rfoliage - se)
o.leaf.resp.avg$component <- "overstorey leaf"

## overstorey wood respiration
wood.resp <- wood_respiration_flux
wood.resp$year <- year(wood.resp$Date)
y.list <- unique(wood.resp$year)
wood.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(wood.ann) <- c("Ring", "year", "value")
for (i in 1:6) {
    for (j in y.list) {
        wood.ann$value[wood.ann$Ring==i&wood.ann$year==j] <- with(wood.resp[wood.resp$year==j&wood.resp$Ring==i, ],
                                                          sum(wood_respiration*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
wood.ann$Trt[wood.ann$Ring%in%c(2,3,6)] <- "aCO2"
wood.ann$Trt[wood.ann$Ring%in%c(1,4,5)] <- "eCO2"
wood.avg <- summaryBy(value~Trt+year, FUN=mean, data=wood.ann, keep.names=T, na.rm=T)
wood.se <- summaryBy(value~Trt+year, FUN=se, data=wood.ann, keep.names=T, na.rm=T)
wood.avg$se <- wood.se$value
wood.avg$pos <- with(wood.avg, value + se)
wood.avg$neg <- with(wood.avg, value - se)
wood.resp.avg <- subset(wood.avg, year > 2012)
wood.resp.avg$component <- "wood"

## understorey aboveground respiration
und.resp <- understorey_respiration_flux
und.resp$year <- year(und.resp$Date)
y.list <- unique(und.resp$year)
und.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(und.ann) <- c("Ring", "year", "value")
for (i in 1:6) {
    for (j in y.list) {
        und.ann$value[und.ann$Ring==i&und.ann$year==j] <- with(und.resp[und.resp$year==j&und.resp$Ring==i, ],
                                                                  sum(respiration*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
und.ann$Trt[und.ann$Ring%in%c(2,3,6)] <- "aCO2"
und.ann$Trt[und.ann$Ring%in%c(1,4,5)] <- "eCO2"
und.avg <- summaryBy(value~Trt+year, FUN=mean, data=und.ann, keep.names=T, na.rm=T)
und.se <- summaryBy(value~Trt+year, FUN=se, data=und.ann, keep.names=T, na.rm=T)
und.avg$se <- und.se$value
und.avg$pos <- with(und.avg, value + se)
und.avg$neg <- with(und.avg, value - se)
und.resp.avg <- subset(und.avg, year > 2012)
und.resp.avg$component <- "understorey aboveground"

## root respiration
root.resp <- root_respiration_flux
root.resp$year <- year(root.resp$Date)
y.list <- unique(root.resp$year)
root.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(root.ann) <- c("Ring", "year", "value")
for (i in 1:6) {
    for (j in y.list) {
        root.ann$value[root.ann$Ring==i&root.ann$year==j] <- with(root.resp[root.resp$year==j&root.resp$Ring==i, ],
                                                               sum(root_respiration_flux*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
root.ann$Trt[root.ann$Ring%in%c(2,3,6)] <- "aCO2"
root.ann$Trt[root.ann$Ring%in%c(1,4,5)] <- "eCO2"
root.avg <- summaryBy(value~Trt+year, FUN=mean, data=root.ann, keep.names=T, na.rm=T)
root.se <- summaryBy(value~Trt+year, FUN=se, data=root.ann, keep.names=T, na.rm=T)
root.avg$se <- root.se$value
root.avg$pos <- with(root.avg, value + se)
root.avg$neg <- with(root.avg, value - se)
root.resp.avg <- subset(root.avg, year > 2012)
root.resp.avg$component <- "root"


### Plot
p1 <- ggplot(o.leaf.resp.avg, aes(x=as.character(year), y=Rfoliage))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(R[ol], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    ylim(0, 600)+
    #scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p2 <- ggplot(wood.resp.avg, aes(x=as.character(year), y=value))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(R[stem], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    ylim(0, 600)+
    #scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p3 <- ggplot(und.resp.avg, aes(x=as.character(year), y=value))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(R[ua], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    ylim(0, 600)+
    #scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p4 <- ggplot(root.resp.avg, aes(x=as.character(year), y=value))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(R[root], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="bottom")+
    ylim(0, 600)+
    #scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

grid.labs <- c("(a)", "(b)", "(c)", "(d)")

## plot 
pdf("output/Figure_S17.pdf", width=8,height=12)
plot_grid(p1, p2, 
          p3, p4, 
          labels="", ncol=1, align="v", axis = "l", 
          rel_heights=c(0.7, 0.7, 0.7, 1))
grid.text(grid.labs,x = 0.15, y = c(0.97, 0.74, 0.5, 0.29),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()

###################---------------------######################
### Growth respiration
g.resp <- data.frame(c("aCO2", "eCO2"), NA, NA)
colnames(g.resp) <- c("Trt", "value", "se")

inout <- tables_by_ring$inout
g.resp$value[g.resp$Trt=="aCO2"] <- inout$aCO2[inout$term=="Rgrowth"]
g.resp$value[g.resp$Trt=="eCO2"] <- inout$eCO2[inout$term=="Rgrowth"]
g.resp$se[g.resp$Trt=="aCO2"] <- inout$aCO2_sd[inout$term=="Rgrowth"]/3
g.resp$se[g.resp$Trt=="eCO2"] <- inout$eCO2_sd[inout$term=="Rgrowth"]/3
g.resp$pos <- with(g.resp, value + se)
g.resp$neg <- with(g.resp, value - se)

### Plot
p1 <- ggplot(g.resp, aes(x=as.character(Trt), y=value))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(R[grow], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_discrete("", 
                     labels=c(expression(aCO[2]),
                              expression(eCO[2])))

pdf("output/Figure_S18.pdf", width=4,height=4)
plot(p1)
dev.off()


###################---------------------######################
### Rsoil and Rh

## Rsoil
soil.resp <- soil_respiration_flux
soil.resp$year <- year(soil.resp$Date)
y.list <- unique(soil.resp$year)
soil.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(soil.ann) <- c("Ring", "year", "value")
for (i in 1:6) {
    for (j in y.list) {
        soil.ann$value[soil.ann$Ring==i&soil.ann$year==j] <- with(soil.resp[soil.resp$year==j&soil.resp$Ring==i, ],
                                                                  sum(soil_respiration_flux*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
soil.ann$Trt[soil.ann$Ring%in%c(2,3,6)] <- "aCO2"
soil.ann$Trt[soil.ann$Ring%in%c(1,4,5)] <- "eCO2"
soil.avg <- summaryBy(value~Trt+year, FUN=mean, data=soil.ann, keep.names=T, na.rm=T)
soil.se <- summaryBy(value~Trt+year, FUN=se, data=soil.ann, keep.names=T, na.rm=T)
soil.avg$se <- soil.se$value
soil.avg$pos <- with(soil.avg, value + se)
soil.avg$neg <- with(soil.avg, value - se)
soil.resp.avg <- subset(soil.avg, year > 2012)
soil.resp.avg$component <- "soil"


## heterotrophic respiration
rh.resp <- heterotrophic_respiration_flux
rh.resp$year <- year(rh.resp$Date)
y.list <- unique(rh.resp$year)
rh.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(rh.ann) <- c("Ring", "year", "value")
for (i in 1:6) {
    for (j in y.list) {
        rh.ann$value[rh.ann$Ring==i&rh.ann$year==j] <- with(rh.resp[rh.resp$year==j&rh.resp$Ring==i, ],
                                                                  sum(heterotrophic_respiration_flux*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
rh.ann$Trt[rh.ann$Ring%in%c(2,3,6)] <- "aCO2"
rh.ann$Trt[rh.ann$Ring%in%c(1,4,5)] <- "eCO2"
rh.avg <- summaryBy(value~Trt+year, FUN=mean, data=rh.ann, keep.names=T, na.rm=T)
rh.se <- summaryBy(value~Trt+year, FUN=se, data=rh.ann, keep.names=T, na.rm=T)
rh.avg$se <- rh.se$value
rh.avg$pos <- with(rh.avg, value + se)
rh.avg$neg <- with(rh.avg, value - se)
rh.resp.avg <- subset(rh.avg, year > 2012)
rh.resp.avg$component <- "rh"


### Plot
p1 <- ggplot(soil.resp.avg, aes(x=as.character(year), y=value))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(R[soil], " (g C ", m^-2," ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="none")+
    ylim(0, 1500)+
    #scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

p2 <- ggplot(rh.resp.avg, aes(x=as.character(year), y=value))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(R[hetero], " (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="bottom")+
    ylim(0, 1500)+
    #scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))

grid.labs <- c("(a)", "(b)")

## plot 
pdf("output/Figure_S19.pdf", width=8,height=6)
plot_grid(p1, p2, 
          labels="", ncol=1, align="v", axis = "l", 
          rel_heights=c(0.7, 1))
grid.text(grid.labs,x = 0.15, y = c(0.95, 0.55),
          gp=gpar(fontsize=16, col="black", fontface="bold"))
dev.off()



###################---------------------######################
### CH4 and DOC and VOC

## CH4
ch4.flux <- methane_c_flux
ch4.flux$year <- year(ch4.flux$Date)
y.list <- unique(ch4.flux$year)
ch4.flux.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(ch4.flux.ann) <- c("Ring", "year", "value")
for (i in 1:6) {
    for (j in y.list) {
        ch4.flux.ann$value[ch4.flux.ann$Ring==i&ch4.flux.ann$year==j] <- with(ch4.flux[ch4.flux$year==j&ch4.flux$Ring==i, ],
                                                                  sum(methane_flux*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
ch4.flux.ann$Trt[ch4.flux.ann$Ring%in%c(2,3,6)] <- "aCO2"
ch4.flux.ann$Trt[ch4.flux.ann$Ring%in%c(1,4,5)] <- "eCO2"
ch4.flux.ann$value <- -ch4.flux.ann$value
ch4.flux.avg <- summaryBy(value~Trt, FUN=mean, data=ch4.flux.ann, keep.names=T, na.rm=T)
ch4.flux.se <- summaryBy(value~Trt, FUN=se, data=ch4.flux.ann, keep.names=T, na.rm=T)
ch4.flux.avg$se <- ch4.flux.se$value
ch4.flux.avg$pos <- with(ch4.flux.avg, value + se)
ch4.flux.avg$neg <- with(ch4.flux.avg, value - se)
ch4.flux.avg$component <- "ch4"

## DOC
doc.flux <- doc_leaching_flux
doc.flux$year <- year(doc.flux$Date)
y.list <- unique(doc.flux$year)
doc.flux.ann <- data.frame(rep(c(1:6), length(y.list)), rep(y.list, each=6), NA)
colnames(doc.flux.ann) <- c("Ring", "year", "value")
for (i in 1:6) {
    for (j in y.list) {
        doc.flux.ann$value[doc.flux.ann$Ring==i&doc.flux.ann$year==j] <- with(doc.flux[doc.flux$year==j&doc.flux$Ring==i, ],
                                                            sum(doc_leaching_flux*ndays, na.rm=T)/sum(ndays, na.rm=T))*365/1000 
    }
}
doc.flux.ann$Trt[doc.flux.ann$Ring%in%c(2,3,6)] <- "aCO2"
doc.flux.ann$Trt[doc.flux.ann$Ring%in%c(1,4,5)] <- "eCO2"
doc.flux.avg <- summaryBy(value~Trt, FUN=mean, data=doc.flux.ann, keep.names=T, na.rm=T)
doc.flux.se <- summaryBy(value~Trt, FUN=se, data=doc.flux.ann, keep.names=T, na.rm=T)
doc.flux.avg$se <- doc.flux.se$value
doc.flux.avg$pos <- with(doc.flux.avg, value + se)
doc.flux.avg$neg <- with(doc.flux.avg, value - se)
doc.flux.avg$component <- "doc"

plot.df <- rbind(ch4.flux.avg, doc.flux.avg)

### Plot
p1 <- ggplot(plot.df, aes(x=as.character(component), y=value))+
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
    geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                  position = position_dodge(0.9), width=0.2, size=0.4) +
    labs(x="", y=expression(paste(Flux, " (g C ", m^-2," ", yr^-1, ")")))+
    theme_linedraw() +
    theme(panel.grid.minor=element_blank(),
          axis.title.x = element_text(size=14), 
          axis.text.x = element_text(size=12),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="right")+
    ylim(0, 0.3)+
    #scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                        labels=c(expression(aCO[2]), expression(eCO[2])))+
    scale_x_discrete("",  
                     labels=c(expression(CH[4]),
                              expression(DOC)))

## plot 
pdf("output/Figure_S11.pdf", width=6,height=4)
plot(p1)
dev.off()


###################---------------------######################
### VOC
o.voc.tr <- voc_emission_flux
o.voc.tr$Trt[o.voc.tr$Ring%in%c(2,3,6)] <- "aCO2"
o.voc.tr$Trt[o.voc.tr$Ring%in%c(1,4,5)] <- "eCO2"
o.voc.tr$year <- as.character(year(o.voc.tr$Date))

p1 <- ggplot(data = o.voc.tr, aes(x = interaction(year, Ring, lex.order = TRUE), 
                                  y = voc_flux)) +
    geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
    annotate(geom = "text", x = seq_len(nrow(o.voc.tr)), y = -0.2, label = rep(c(1:6),4), size = 4) +
    annotate(geom = "text", x = 3.5 + 6 * (0:3), y = -0.6, label = unique(o.voc.tr$year), size = 6) +
    coord_cartesian(ylim = c(0, 6), expand = FALSE, clip = "off")+
    labs(x="Year", y=expression(paste("VC (g C ", m^-2, " ", yr^-1, ")")))+
    theme_linedraw() +
    theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
          panel.grid.minor=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y=element_text(size=12),
          axis.title.y=element_text(size=14),
          legend.text=element_text(size=12),
          legend.title=element_text(size=14),
          panel.grid.major=element_blank(),
          legend.position="top")+
    scale_y_continuous(position="left")+
    scale_fill_manual(name="Treatment", values = c("aCO2" = "cyan", "eCO2" = "pink"),
                      labels=c(expression(aCO[2]), expression(eCO[2])))


## plot 
pdf("output/Figure_S20.pdf", width=9,height=6)
plot(p1)
dev.off()
