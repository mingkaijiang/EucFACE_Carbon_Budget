
if(!dir.exists("download"))dir.create("download")

if(!require(HIEv)){
  stop("Install the HIEv package first from bitbucket.org/remkoduursma/HIEv")
}

setToken(tokenfile="tokenfile.txt", quiet=TRUE)
setToPath("download")

if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, doBy, readxl) # add other packages needed to this list


# Loading constants
source("definitions/constants.R")

# Sourcing all R files in the modules subdirectory
sourcefiles <- dir("modules", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)




