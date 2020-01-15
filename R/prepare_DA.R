#### Create output folder
if(!dir.exists("DA_output")) {
    dir.create("DA_output", showWarnings = FALSE)
}

#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(doBy, 
               ggplot2,
               grid,
               cowplot,
               reshape2,
               lme4,
               mcmc,
               mvtnorm,
               RColorBrewer,
               raster,
               ncdf4)  

#### Sourcing all R files in the modules subdirectory
source_basic_scripts <- dir("DA_scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z1 in source_basic_scripts)source(z1)


