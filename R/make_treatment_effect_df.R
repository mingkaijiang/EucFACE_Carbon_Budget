make_treatment_effect_df <- function(inDF, v, cond) {
    ### This function processes input variable by treatment
    ### so that they are in format for plotting
    ### inDF: the input variable
    ### v: the column number of the variable flux/pool
    ### cond: condition of processing the input variable, with
    ###       1 == simply one value for each date and each ring
    ###       2 == not all rings share the same dates
    ###       3 == 
    
    ### Checking for conditions 
    if (cond == 1) {
        ### Process data by treatment
        ac <- data.frame(unique(inDF$Date), NA, NA, NA)
        colnames(ac) <- c("Date", "a", "b", "c")
        ec <- ac
        
        ac$a <- inDF[inDF$Ring == 2, v]
        ac$b <- inDF[inDF$Ring == 3, v]
        ac$c <- inDF[inDF$Ring == 6, v]
        ec$a <- inDF[inDF$Ring == 1, v]
        ec$b <- inDF[inDF$Ring == 4, v]
        ec$c <- inDF[inDF$Ring == 5, v]
        ac$Treatment <- "aCO2"
        ec$Treatment <- "eCO2"
        
        ### Combine ec and ac 
        tr <- rbind(ac, ec)
        tr$avg <- rowSums(tr[,2:4], na.rm=T)/3
        tr$max <- apply(tr[, 2:4], 1, max, na.rm=T)
        tr$min <- apply(tr[, 2:4], 1, min, na.rm=T)
        tr$sd <- apply(tr[, 2:4], 1, sd, na.rm=T)
        tr$pos <- tr$avg + tr$sd
        tr$neg <- tr$avg - tr$sd
        
    } else if (cond == 2) {
        ### Process data by treatment
        inDF$ym <- format(as.Date(inDF$Date), "%Y-%m")
        ac <- data.frame(unique(inDF$ym), NA, NA, NA)
        colnames(ac) <- c("Date", "a", "b", "c")
        ec <- ac
        
        for (i in inDF$ym) {
            ac[ac$Date == i, "a"] <- mean(inDF[inDF$Ring == 2 & inDF$ym == i, v], na.rm=T)
            ac[ac$Date == i, "b"] <- mean(inDF[inDF$Ring == 3 & inDF$ym == i, v], na.rm=T)
            ac[ac$Date == i, "c"] <- mean(inDF[inDF$Ring == 6 & inDF$ym == i, v], na.rm=T)
            ec[ec$Date == i, "a"] <- mean(inDF[inDF$Ring == 1 & inDF$ym == i, v], na.rm=T)
            ec[ec$Date == i, "b"] <- mean(inDF[inDF$Ring == 4 & inDF$ym == i, v], na.rm=T)
            ec[ec$Date == i, "c"] <- mean(inDF[inDF$Ring == 5 & inDF$ym == i, v], na.rm=T)
        }
        
        ac$Treatment <- "aCO2"
        ec$Treatment <- "eCO2"
        tr <- rbind(ac, ec)
        tr$avg <- rowSums(tr[,2:4], na.rm=T)/3
        tr$max <- apply(tr[, 2:4], 1, max, na.rm=T)
        tr$min <- apply(tr[, 2:4], 1, min, na.rm=T)
        tr$sd <- apply(tr[, 2:4], 1, sd, na.rm=T)
        tr$sd[is.na(tr$sd)] <- 0
        tr$pos <- tr$avg + tr$sd
        tr$neg <- tr$avg - tr$sd
        tr$Date <- as.Date(paste0(as.character(tr$Date), "-01"), format="%Y-%m-%d")
    }
    
    ### return
    return(tr)
}