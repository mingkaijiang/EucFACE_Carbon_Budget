combine_all_model_output <- function() {
    
    
    ### read in model output
    cablDF <- read_in_cabl()
    clm4DF <- read_in_clm4()
    clmpDF <- read_in_clmp()
    gdayDF <- read_in_gday()
    lpjwDF <- read_in_lpjw()
    lpjxDF <- read_in_lpjx()
    ocnxDF <- read_in_ocnx()
    sdvmDF <- read_in_sdvm()
    
    eucDF <- read_in_EucFACE_output()
    
    ### assign names
    cablDF$model <- "CABL"
    clm4DF$model <- "CLM4"
    clmpDF$model <- "CLMP"
    gdayDF$model <- "GDAY"
    lpjwDF$model <- "LPJW"
    lpjxDF$model <- "LPJX"
    ocnxDF$model <- "OCNX"
    sdvmDF$model <- "SDVM"
    
    ### we can effectively ignore lpjw because it's not included in the Medlyn 2016 paper
    outDF <- rbind(cablDF, clm4DF, clmpDF, gdayDF, lpjxDF, ocnxDF, sdvmDF)
    
    
    ### grouping allocation
    outDF$AWOOD_2 <- outDF$AWOOD + outDF$ACROOT  # CABLE has a large Froot pool, so Croot is part of it!
    outDF$tau_MYCO <- NA
    outDF$tau_MICR <- NA
    
    outDF[mapply(is.infinite, outDF)] <- NA

    ### summary of problems:
    ### 1. we need to group coarseroot and stem together,
    ###    but actually we can't, because for models that don't differ croot and froot, froot biomass is huge, 
    ###    so a fraction of that must be croot.
    ### 2. we haven't add delta into the calculations.
    ### 3. LPJW has a very large allocation fraction to other, which includes NTC and reproductive organs! Do we just ignore it?
    ### 4. Csoil and Cwood are large in all model output, and therefore tau soil and tau wood is problematic. 
    ### 5. The simulated GPP is overstorey only (in theory). 
    
    ### calculate multi-model mean
    subDF1 <- subset(outDF, CO2=="aCO2")
    subDF2 <- subset(outDF, CO2=="eCO2")
    
    subDF1 <- subDF1[,c("ALEAF", "AWOOD_2", "AFROOT", "AOTHER", 
                        "tau_LEAF", "tau_FROOT", "tau_MYCO",
                        "tau_CFLITA", "tau_CFLITB", "tau_MICR",
                        "tau_SOIL", "model")]
    
    subDF2 <- subDF2[,c("ALEAF", "AWOOD_2", "AFROOT", "AOTHER", 
                        "tau_LEAF", "tau_FROOT", "tau_MYCO",
                        "tau_CFLITA", "tau_CFLITB", "tau_MICR",
                        "tau_SOIL", "model")]
    
    mmDF1 <- melt(subDF1, id.vars = "model")
    mmDF1$CO2 <- "aCO2"
    
    mmDF2 <- melt(subDF2, id.vars = "model")
    mmDF2$CO2 <- "eCO2"
    
    mmDF <- rbind(mmDF1, mmDF2)
    pctDF <- mmDF1
    
    for(i in unique(pctDF$model)) {
        for (j in unique(pctDF$variable)) {
            pctDF$value[pctDF$model==i&pctDF$variable==j] <-mmDF2$value[mmDF2$model==i&mmDF2$variable==j]/mmDF1$value[mmDF1$model==i&mmDF1$variable==j]
        }
    }
    
    pctDF$CO2 <- "pct"
    
    mmDF <- rbind(mmDF, pctDF)
    mmDF$variable <- gsub("AWOOD_2", "AWOOD", mmDF$variable)
    colnames(mmDF) <- c("Model", "variable", "value", "CO2")
    mmDF$sd <- "NA"
    mmDF$Source <- "Model"
    
    ### eucDF
    eucDF$Source <- "Data"
    eucDF$Model <- "Data"
    eucDF <- eucDF[,c("Model", "variable", "value", "CO2", "sd", "Source")]
    
    plotDF <- rbind(mmDF, eucDF)
    plotDF$sd <- as.numeric(plotDF$sd)
    colnames(plotDF) <- c("Model", "variable", "value", "Trt", "sd", "Source")
    
    ### get the dataframes
    plotDF1 <- plotDF[plotDF$Trt%in%c("aCO2", "eCO2") & plotDF$variable %in%c("ALEAF", "AWOOD", "AFROOT", "AOTHER"), ]
    plotDF2 <- plotDF[plotDF$Trt%in%c("aCO2", "eCO2") & plotDF$variable %in%c("tau_LEAF", "tau_FROOT", #"tau_MYCO", 
                                                                   "tau_CFLITA", "tau_CFLITB"), ]
    
    plotDF3 <- plotDF[plotDF$Trt%in%c("aCO2", "eCO2") & plotDF$variable=="tau_SOIL", ]
    
    #plotDF5 <- plotDF[plotDF$CO2 == "pct" & plotDF$variable %in%c("ALEAF", "AWOOD", "AFROOT", "AOTHER"), ]
    #plotDF6 <- plotDF[plotDF$CO2 == "pct" & plotDF$variable %in%c("tau_LEAF", "tau_FROOT", #"tau_MYCO", 
    #                                                               "tau_CFLITA", "tau_CFLITB", #"tau_MICR", 
    #                                                               "tau_SOIL"), ]
    
    ### color blind friendly
    library(RColorBrewer)
    display.brewer.all(colorblindFriendly = TRUE)
    f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
    cols <- f("Set2")

    ### make the bar plot
    p1 <- ggplot(plotDF1,
                 aes(x=variable, y=value, group=Trt)) + 
        geom_errorbar(aes(x=variable, ymin=value-sd, ymax=value+sd), 
                      position = position_dodge(0.6), width=0.2)+
        geom_point(aes(x=variable, y=value, shape=Trt, fill=Model), 
                   size=4,position = position_dodge(0.6))+
        xlab("") + ylab("Allocation coefficients")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete("",  
                         limits=c("ALEAF",
                                  "AWOOD",
                                  "AFROOT",
                                  "AOTHER"),
                         labels=c("Leaf",
                                  "Wood",
                                  "Froot",
                                  "Other"))+
        scale_y_continuous(limits=c(-0.02, 0.8), 
                           breaks=c(0.0, 0.2, 0.4, 0.6, 0.8),
                           labels=c(0.0, 0.2, 0.4, 0.6, 0.8))+
        scale_fill_manual(values=c("CABL"=cols[1], "CLM4"=cols[2],"CLMP"=cols[3],
                                   "GDAY"=cols[4], "LPJX"=cols[5],"OCNX"=cols[6],
                                   "SDVM"=cols[7], "Data"=cols[8]))+
        scale_shape_manual(values=c(21,23))+
        guides(fill = guide_legend(override.aes = list(shape=21)))

    
    p2 <- ggplot(plotDF2,
                 aes(x=variable, y=value, group=Trt)) + 
        geom_errorbar(aes(x=variable, ymin=value-sd, ymax=value+sd), 
                      position = position_dodge(0.6), width=0.2)+
        geom_point(aes(x=variable, y=value, shape=Trt, fill=Model), 
                   size=4,position = position_dodge(0.6))+
        xlab("") + ylab(expression("Turnover rates ( " * yr^-1 * " )"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=16), 
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_discrete("",  
                         limits=c("tau_LEAF",
                                  "tau_FROOT",
                                  "tau_CFLITA",
                                  "tau_CFLITB"),
                         labels=c("Leaf",
                                  "Froot",
                                  "Aglit",
                                  "Bglit"))+
        scale_y_continuous(limits=c(0, 4), 
                           breaks=c(0, 1, 2, 3, 4),
                           labels=c(0, 1, 2, 3, 4))+
        scale_fill_manual(values=c("CABL"=cols[1], "CLM4"=cols[2],"CLMP"=cols[3],
                                   "GDAY"=cols[4], "LPJX"=cols[5],"OCNX"=cols[6],
                                   "SDVM"=cols[7], "Data"=cols[8]))+
        scale_shape_manual(values=c(21,23))


    p3 <- ggplot(plotDF3,
                 aes(x=variable, y=value, group=Trt)) + 
        geom_errorbar(aes(x=variable, ymin=value-sd, ymax=value+sd), 
                      position = position_dodge(0.6), width=0.2)+
        geom_point(aes(x=variable, y=value, shape=Trt, fill=Model), 
                   size=4,position = position_dodge(0.6))+
        xlab("") + ylab(expression("Turnover rates ( " * yr^-1 * " )"))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_blank(), 
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              plot.title = element_text(hjust = 0.5),
              axis.title = element_text(size = 20, face="bold"))+
        scale_x_discrete("",  
                         limits=c("tau_SOIL"),
                         labels=c("Soil"))+
        scale_y_continuous(limits=c(0, 0.2), 
                           breaks=c(0, 0.1, 0.2),
                           labels=c(0, 0.1, 0.2))+
        scale_fill_manual(values=c("CABL"=cols[1], "CLM4"=cols[2],"CLMP"=cols[3],
                                   "GDAY"=cols[4], "LPJX"=cols[5],"OCNX"=cols[6],
                                   "SDVM"=cols[7], "Data"=cols[8]))+
        scale_shape_manual(values=c(21,23))
    

    ### combined plots + shared legend
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(p2, p3, rel_widths=c(0.8, 0.3),
                                labels=c("B","C"), ncol=2, align="v", axis = "l")
    
    
    ### output
    pdf("output/model_data_comparison.pdf", width=8, height=10)
    plot_grid(p1, combined_plots, legend_shared, 
              labels=c("A","",""),
              ncol=1, rel_heights=c(1,1,0.3))
    dev.off()    
    
    

}