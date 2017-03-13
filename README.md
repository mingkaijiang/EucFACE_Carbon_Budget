# EucFACE C balance

*this is a work in progress - anything below is subject to change! And please suggest changes.*



## Instructions for contributors

1. Place your HIEv token in a file called 'tokenfile.txt', and place it in the directory for this project.
2. Code for a pool or flux should be organized in a 'module', as a subdirectory in the 'modules' directory. Name these wisely (instructions tbd), for example leaf_pool, leaf_flux, stem_bark_pool, etc. See below on naming the module.
3. Each of those folders will have a function definition in it, which will be named make_<<module>>.R, for example 'make_leaf_pool.R'. Place this function inside an R script called 'make_leaf_pool.R', though it does not actually matter what you call it.
4. The `make_` function may take inputs (see `run.R` for examples), and will produce one output (and one only), a dataframe (see details below).
4. In each module subfolder, it is **not allowed** to place scripts other than ones that define functions. Write functions only (no exceptions).
5. In each of the module subfolders, place other stuff you might need there, like CSV files with information that is not on HIEv (not data, but maybe settings or whatever).
6. Constants / hardwired parameters that you think will be used more than once should be placed in `definitions/constants.R`
7. **Do not** load packages inside any code in the modules, instead add needed packages to the `R/prepare.R` script. `library` or `require` statements are not allowed.
8. The file 'run.R' will compute each module in order needed (since some depend on others before it), and produce lots of dataframes.
9. `snake_case_always`, not `camelCaseNever`
10. If you just want to calculate all, do `source('run.R')`

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



## Module output specs

Each module function should produce a dataframe with the following **mandatory** columns:

- Date (YYYY-MM-DD) (Date or character, **not** POSIXct)
- Ring (1 - 6) (numeric)
- <<module>> (e.g. leaf_pool). Units: mgC m^-2^ day^-1^ for fluxes, mgC m^-2^ for pools

And the following **optional** columns:

- Method (1, 2, ..., n) (numeric)
- *any others?*


