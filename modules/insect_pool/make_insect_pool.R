make_insect_pool <- function(c_frac){

    
    #### read in data
    f16 <- read.csv("data/EucFACE_data/FACE_P0017_RA_Litter_20160101-20161212-L1-V2.csv")
    f15 <- read.csv("data/EucFACE_data/FACE_P0017_RA_Litter_20150101-20151217-L1-V2.csv")
    f14 <- read.csv("data/EucFACE_data/FACE_P0017_RA_Litter_20140101-20141216-L1-V2.csv")
    f13 <- read.csv("data/EucFACE_data/FACE_P0017_RA_Litter_20121001-20131231-L1-V2.csv")
    
    #### set up col names
    colnames(f13) <- c("Ring", "Date", "Trap", "Twig", "Bark", "Seed", "Leaf", "Other", "Insect", "Comments", "days.past")
    colnames(f14) <- c("Ring", "Date", "Trap", "Twig", "Bark", "Seed", "Leaf", "Other", "Insect", "Comments", "days.past")
    colnames(f15) <- c("Ring", "Date", "Trap", "Twig", "Bark", "Seed", "Leaf", "Other", "Insect", "Comments", "days.past")
    colnames(f16) <- c("Ring", "Date", "Trap", "Twig", "Bark", "Seed", "Leaf", "Other", "Insect", "Comments", "days.past")
    
    #### Merge the files
    litter_raw <- rbind(f13, f14, f15, f16) 
    
    # glitch fix
    litter_raw$Ring <- as.character(litter_raw$Ring)
    litter_raw$Trap <- as.character(litter_raw$Trap)
    litter_raw$Ring[is.na(litter_raw$Ring)] <- litter_raw$RING[is.na(litter_raw$Ring)]
    litter_raw$TRAP[is.na(litter_raw$Ring)] <- litter_raw$RING[is.na(litter_raw$Ring)]
    litter_raw$Twig <- as.numeric(litter_raw$Twig)
    litter_raw$Bark <- as.numeric(litter_raw$Bark)
    litter_raw$Seed <- as.numeric(litter_raw$Seed)
    litter_raw$Leaf <- as.numeric(litter_raw$Leaf)
    litter_raw$Other <- as.numeric(litter_raw$Other)
    litter_raw$Insect <- as.numeric(litter_raw$Insect)
    
    # remove two data points where big branches fall into litter bascket
    line.num <- which.max(litter_raw$Twig)
    litter_raw <- litter_raw[-line.num,]
    
    line.num <- which.max(litter_raw$Twig)
    litter_raw <- litter_raw[-line.num,]
    
    # Conversion factor from g basket-1 to g m-2
    conv <- c_frac / frass_basket_area
    
    litter <- dplyr::mutate(litter_raw, 
                            Date = as.Date(litter_raw$Date, format = "%d/%m/%Y"),
                            Start_date = Date - days.past,
                            End_date = Date,
                            ndays = days.past,
                            Insect = as.numeric(Insect) * conv,
                            Other = as.numeric(Other) * conv,
                            Leaf = as.numeric(Leaf) * conv)
    
    # Averages by Ring
    litter_a <- summaryBy(Insect ~ Date + Ring, FUN=mean, na.rm=TRUE,
                          data=litter, id = ~Start_date + End_date, keep.names=TRUE)
    
    litter_a$ndays <- as.numeric(litter_a$End_date - litter_a$Start_date) + 1
    
    # Only use data period 2012-2016
    litter_a <- litter_a[litter_a$Date<="2016-12-31",]
    
    ### Remove NAs
    out <- litter_a[complete.cases(litter_a$Insect),]
    
    out <- out[,c("Date", "Ring", "Insect")]
    colnames(out) <- c("Date", "Ring", "insect_pool")
    
    ### First date has missing rings
    min.date <- min(out$Date)
    out <- out[out$Date != min.date, ]
    
    ### return
    return(out)
}

