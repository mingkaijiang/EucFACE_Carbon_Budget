#- Make the fine production flux
make_fineroot_production_flux <- function(c_fraction){
  
  #- download the data. Note that the data consist of an out-of date, poorly named excel file. (Sigh).
  download_fineroot_data()
  
  #- read in the second sheet, entitled "Fine root production". Do some tidying.
  sheets <- excel_sheets("download/EucFACE Fine root HIEv.xlsx")
  focalsheet <- which(sheets=="Fine root production")
  frp1 <- data.frame(read_excel("download/EucFACE Fine root HIEv.xlsx",sheet=focalsheet))
  frp1$Date <- as.Date(frp1$Date)
  names(frp1)[2] <- "Ring"
  names(frp1)[8] <- "frp_tot"
  
  #- average across rings and dates
  frp.m <- summaryBy(frp_tot~Date+Ring,data=frp1,FUN=mean,keep.names=T,na.rm=T)
  
  #- convert to mg C m-2 day-1
  frp.m$fineroot_production_flux <- frp.m$frp_tot*c_fraction*1000
  
  #- format dataframe to return
  frp.out <- frp.m[,c("Date","Ring","fineroot_production_flux")]
  return(frp.out)
}