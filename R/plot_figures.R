####### This script calls run.R and plot figures based on that

#### Call run.R program
source("run.R")

#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}


#### LAI
### Process data by treatment
ac <- data.frame(unique(lai_variable$Date), NA, NA, NA)
colnames(ac) <- c("Date", "a", "b", "c")
ec <- ac
ac$a <- lai_variable[lai_variable$Ring == 2, "lai_variable"]
ac$b <- lai_variable[lai_variable$Ring == 3, "lai_variable"]
ac$c <- lai_variable[lai_variable$Ring == 6, "lai_variable"]
ec$a <- lai_variable[lai_variable$Ring == 1, "lai_variable"]
ec$b <- lai_variable[lai_variable$Ring == 4, "lai_variable"]
ec$c <- lai_variable[lai_variable$Ring == 5, "lai_variable"]
ac$Treatment <- "aCO2"
ec$Treatment <- "eCO2"
lai.tr <- rbind(ac, ec)
lai.tr$avg <- rowSums(lai.tr[,2:4])/3
lai.tr$max <- apply(lai.tr[, 2:4], 1, max)
lai.tr$min <- apply(lai.tr[, 2:4], 1, min)
lai.tr$sd <- apply(lai.tr[, 2:4], 1, sd)
lai.tr$pos <- lai.tr$avg + lai.tr$sd
lai.tr$neg <- lai.tr$avg - lai.tr$sd

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
sla <- sla_variable
sla$ym <- format(as.Date(sla$Date), "%Y-%m")
ac <- data.frame(unique(sla$ym), NA, NA, NA)
colnames(ac) <- c("Date", "a", "b", "c")
ec <- ac

for (i in sla$ym) {
    ac[ac$Date == i, "a"] <- mean(sla[sla$Ring == 2 & sla$ym == i, "sla_variable"], na.rm=T)
    ac[ac$Date == i, "b"] <- mean(sla[sla$Ring == 3 & sla$ym == i, "sla_variable"], na.rm=T)
    ac[ac$Date == i, "c"] <- mean(sla[sla$Ring == 6 & sla$ym == i, "sla_variable"], na.rm=T)
    ec[ec$Date == i, "a"] <- mean(sla[sla$Ring == 1 & sla$ym == i, "sla_variable"], na.rm=T)
    ec[ec$Date == i, "b"] <- mean(sla[sla$Ring == 4 & sla$ym == i, "sla_variable"], na.rm=T)
    ec[ec$Date == i, "c"] <- mean(sla[sla$Ring == 5 & sla$ym == i, "sla_variable"], na.rm=T)
}

ac$Treatment <- "aCO2"
ec$Treatment <- "eCO2"
sla.tr <- rbind(ac, ec)
sla.tr$avg <- rowSums(sla.tr[,2:4], na.rm=T)/3
sla.tr$max <- apply(sla.tr[, 2:4], 1, max, na.rm=T)
sla.tr$min <- apply(sla.tr[, 2:4], 1, min, na.rm=T)
sla.tr$sd <- apply(sla.tr[, 2:4], 1, sd, na.rm=T)
sla.tr$sd[is.na(sla.tr$sd)] <- 0
sla.tr$pos <- sla.tr$avg + sla.tr$sd
sla.tr$neg <- sla.tr$avg - sla.tr$sd
sla.tr$Date <- as.Date(paste0(as.character(sla.tr$Date), "-01"), format="%Y-%m-%d")

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
