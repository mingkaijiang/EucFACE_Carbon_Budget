
### Read initial basal area data
f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
f12$ba <- ((f12$X17.02.2011/2)^2) * pi

baDF <- summaryBy(ba~Ring, data=f12, FUN=sum, na.rm=T, keep.names=T)

### return in unit of cm2/m2, which is m2 ha-1
baDF$ba_ground_area <- baDF$ba / ring_area

### summary LAI per ring per ring
laiDF <- lai_variable
laiDF$Yr <- year(laiDF$Date)
lai.ann <- summaryBy(lai_variable~Yr+Ring, data=laiDF, FUN=mean, keep.names=T, na.rm=T)


### Assign data to gppDF
for (j in 1:6) {
    laiDF$BA[laiDF$Ring== j] <- baDF$ba_ground_area[baDF$Ring==j]
}


a.lai.cv.o <- sd(laiDF$lai_variable[laiDF$Ring%in%c(2,3,6)])/mean(laiDF$lai_variable[laiDF$Ring%in%c(2,3,6)])
e.lai.cv.o <- sd(laiDF$lai_variable[laiDF$Ring%in%c(1,4,5)])/mean(laiDF$lai_variable[laiDF$Ring%in%c(1,4,5)])


### Standardize
tempDF <- data.frame(c(1:6), NA, NA, NA)
colnames(tempDF) <- c("Ring", "ring_mean", "trt_mean", "diff")    
for (i in 1:6) {
    tempDF$ring_mean[tempDF$Ring==i] <- mean(laiDF$lai_variable[laiDF$Ring==i])
}

tempDF$trt_mean[tempDF$Ring%in%c(2,3,6)] <- mean(laiDF$lai_variable[laiDF$Ring%in%c(2,3,6)])
tempDF$trt_mean[tempDF$Ring%in%c(1,4,5)] <- mean(laiDF$lai_variable[laiDF$Ring%in%c(1,4,5)])
tempDF$diff <- tempDF$trt_mean - tempDF$ring_mean

for (i in 1:6) {
    laiDF$lai_new[laiDF$Ring==i] <- tempDF$diff[tempDF$Ring==i] + laiDF$lai_variable[laiDF$Ring==i]
}

a.lai.cv.n <- sd(laiDF$lai_new[laiDF$Ring%in%c(2,3,6)])/mean(laiDF$lai_new[laiDF$Ring%in%c(2,3,6)])
e.lai.cv.n <- sd(laiDF$lai_new[laiDF$Ring%in%c(1,4,5)])/mean(laiDF$lai_new[laiDF$Ring%in%c(1,4,5)])

lai.cv <- data.frame(rbind(a.lai.cv.o, e.lai.cv.o), rbind(a.lai.cv.n, e.lai.cv.n))
colnames(lai.cv) <- c("lai_old", "lai_new")
rownames(lai.cv) <- c("aCO2", "eCO2")

## 2013
lai2013 <- subset(laiDF, Yr==2013)
t2 <- summaryBy(BA+lai_variable+lai_new~Ring, FUN=mean, data=lai2013, keep.names=T, na.rm=T)

p2 <- ggplot(lai2013, aes(x=BA, y=lai_new)) + 
    geom_smooth(method="lm") + ylab("LAI") +
    geom_point(data=t2, aes(BA, lai_new,color=as.factor(t2$Ring)), size=4)+
    scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                        labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
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
    ggtitle("Year 2013")+
    ylim(1, 2.1)

plot(p2)


a.ba.cv.o <- sd(laiDF$BA[laiDF$Ring%in%c(2,3,6)])/mean(laiDF$BA[laiDF$Ring%in%c(2,3,6)])
e.ba.cv.o <- sd(laiDF$BA[laiDF$Ring%in%c(1,4,5)])/mean(laiDF$BA[laiDF$Ring%in%c(1,4,5)])


### Standardize
tempDF <- data.frame(c(1:6), NA, NA, NA)
colnames(tempDF) <- c("Ring", "ring_mean", "trt_mean", "diff")    
for (i in 1:6) {
    tempDF$ring_mean[tempDF$Ring==i] <- mean(laiDF$BA[laiDF$Ring==i])
}

tempDF$trt_mean[tempDF$Ring%in%c(2,3,6)] <- mean(laiDF$BA[laiDF$Ring%in%c(2,3,6)])
tempDF$trt_mean[tempDF$Ring%in%c(1,4,5)] <- mean(laiDF$BA[laiDF$Ring%in%c(1,4,5)])
tempDF$diff <- tempDF$trt_mean - tempDF$ring_mean

for (i in 1:6) {
    laiDF$ba_new[laiDF$Ring==i] <- tempDF$diff[tempDF$Ring==i] + laiDF$BA[laiDF$Ring==i]
}

a.ba.cv.n <- sd(laiDF$ba_new[laiDF$Ring%in%c(2,3,6)])/mean(laiDF$ba_new[laiDF$Ring%in%c(2,3,6)])
e.ba.cv.n <- sd(laiDF$ba_new[laiDF$Ring%in%c(1,4,5)])/mean(laiDF$ba_new[laiDF$Ring%in%c(1,4,5)])

ba.cv <- data.frame(rbind(a.ba.cv.o, e.ba.cv.o), rbind(a.ba.cv.n, e.ba.cv.n))
colnames(ba.cv) <- c("ba_old", "ba_new")
rownames(ba.cv) <- c("aCO2", "eCO2")

## 2013
lai2013 <- subset(laiDF, Yr==2013)
t2 <- summaryBy(BA+ba_new+lai_variable+lai_new~Ring, FUN=mean, data=lai2013, keep.names=T, na.rm=T)

p2 <- ggplot(lai2013, aes(x=ba_new, y=lai_new)) + 
    geom_smooth(method="lm") + ylab("LAI") +
    geom_point(data=t2, aes(ba_new, lai_new,color=as.factor(t2$Ring)), size=4)+
    scale_colour_manual(name="Ring", values = c("1" = "red", "2" = "cyan",  "3" = "blue", 
                                                "4" = "pink",  "5" = "orange", "6" = "darkblue"),
                        labels=c("R1", "R2", "R3", "R4", "R5", "R6")) +
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
    ggtitle("Year 2013")+
    ylim(1, 2.1)

plot(p2)
