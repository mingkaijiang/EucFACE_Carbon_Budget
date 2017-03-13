make_lai_variable <- function(){
  
  res <- expand.grid(Date=seq(as.Date("2013-1-1"), by="2 weeks", length=110),
                     Ring=1:6)
  res$lai_variable <- runif(nrow(res),1,2)
  
  return(res)
}
