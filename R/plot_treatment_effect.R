plot_treatment_effect <- function(inDF, y.lab) {
    
    ### This function plot treatment effect
    ### inDF: calls the input
    ### y.lab: the y axis label
    
    ac <- subset(inDF, Treatment == "aCO2")
    ec <- subset(inDF, Treatment == "eCO2")
    
    p <- ggplot(ac, aes(Date))+
        geom_line(data=ac, aes(y=avg, col="aCO2"))+
        geom_ribbon(data=ac,aes(ymin=neg,ymax=pos),fill="cyan", alpha=0.3)+
        geom_line(data=ec, aes(y=avg, col="eCO2"))+
        geom_ribbon(data=ec,aes(ymin=neg,ymax=pos),fill="red", alpha=0.3)+
        labs(x="Date", y=y.lab)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text=element_text(size=12),
              axis.title=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_line(color="grey"))+
        scale_colour_manual(name="Treatment", values = c("aCO2" = "blue", "eCO2" = "red"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    return(p)
}