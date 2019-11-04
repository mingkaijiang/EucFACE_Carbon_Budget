# EucFACE C balance

*this is the working repository to synthetize EucFACE carbon budget*



## General instruction to access the repo

1. Place your HIEv token in a file called 'tokenfile.txt', and place it in the directory for this project. Note that only internal user from HIE who has access to HIEv has a token file. 
2. Code for a pool or flux is organized in a 'module', as a subdirectory in the 'modules' directory. See below on naming the module.
3. Each of those folders will have a function definition in it, which will be named make_<<module>>.R, for example 'make_leaf_pool.R'. Many folders also contain a separate script to download the data, and some additional scripts to process the data and generate statistics. 
4. Data downloaded from HIEv are stored in the folder "download". 
5. Data not available from HIEv are either stored in folder "temp_files" or "data". Some cleaning is needed to merge the two folders. 
6. The `run.R` script is the master script, where the entire repo is processed. 
8. Constants / hardwired parameters are placed in `definitions/constants.R`.
9. All packages and essential pre-setting work are pre-loaded in 'R/prepare.R".
10. The folder 'R" synthesized all the individual scripts and compute the budget related variables and statistics. 
11. The 'DA_scripts' folder contains necessary codes to construct the data assimilation framework. 


## Downloading data

As mentioned above, downloading occurs in a separate function. **Do not set the 'to path' in the download statement**, using `cache=` or `setToPath`. Reading downloaded data should also not assume the download location,

```
mydata <- read.csv(file.path(getToPath(), "FACE_somefile.csv"))
```

This allows us to change the download location so that it is actually respected by all subfunctions.

## Code conventions

- `snake_case_always`, not `camelCaseNever`



## Naming the module

Probably useful to be very consistent, and have a suffix to indicate whether the thing is a flux, a pool, or 'something else' (for example, specific leaf area) (for this case maybe a 'variable' suffix?)

Examples:
- leaf_pool
- stem_branch_pool
- stem_wood_pool
- soil_microbe_pool
- understorey_gpp_flux
- sla_variable
- lai_variable

### Output

All output are stored in folder 'output'. The data assimilation results are stored in 'DA_output'.


