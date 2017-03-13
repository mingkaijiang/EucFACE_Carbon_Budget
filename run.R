
source("R/prepare.R")

lai_variable <- make_lai_variable()

sla_variable <- make_sla_variable()


leaf_pool <- make_leaf_pool(lai_variable, sla_variable)


