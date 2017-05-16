#- Make the fine root C pool
make_fineroot_pool <- function(c_fraction){
  
  #- download the data. Note that the data consist of an out-of date, poorly named excel file. (Sigh).
  #  Fix to use Gerry's csv files.
  download_fineroot_data()
  
  #- read in the first sheet, entitled "Fine root biomass". Do some tidying.
  sheets <- excel_sheets("download/EucFACE Fine root HIEv.xlsx")
  focalsheet <- which(sheets=="Fine root biomass")
  frb1 <- data.frame(read_excel("download/EucFACE Fine root HIEv.xlsx",sheet=focalsheet))
  frb1$Date <- as.Date(frb1$Date)
  names(frb1)[2] <- "Ring"
  names(frb1)[8] <- "frb_tot"
  
  #- average across rings and dates
  frb.m <- summaryBy(frb_tot~Date+Ring,data=frb1,FUN=mean,keep.names=T)
  
  #- convert to g C m-2. Use fine-root specific c_fraction from Juan.
  frb.m$fineroot_pool <- frb.m$frb_tot*c_fraction
  
  #- format dataframe to return
  frb.out <- frb.m[,c("Date","Ring","fineroot_pool")]
  return(frb.out)
}