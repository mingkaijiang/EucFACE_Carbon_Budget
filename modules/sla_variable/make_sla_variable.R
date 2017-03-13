make_sla_variable <- function(){
  
  
  expand.grid(Date=seq(as.Date("2013-1-1"), as.Date("2017-1-1"), by="1 day"),
              Ring=1:6,
              sla_variable=55)
  
}