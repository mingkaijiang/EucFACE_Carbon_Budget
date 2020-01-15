# EucFACE C balance

*this is the publishable version of the EucFACE carbon budget code repository*



## General instruction to access the repo

1. If source data from HIEv, the internal data server at the Hawkesbury Institute for the Environment, you need you place your HIEv token in a file called 'tokenfile.txt', and place it in the directory for this project. Note that only internal user from HIE who has access to HIEv has a token file. 
2. If source data from the data repository, you need to create a folder named "data" in the main directory, and place all the downloaded data, in their existing structure, into the folder. 
3. The `run.R` script is the master script, where the entire repo is processed. Detailed instructions and comments are available within the script. 
4. Codes for the budget are organized in subdirectories of the folder "module". 
5. Each of those folders has a function definition in it, which is named make_<<module>>.R, for example 'make_leaf_pool.R'. Many folders also contain a separate script to download the data, and some additional scripts to process the data and generate statistics. 
6. Constants / hardwired parameters are placed in folder "definitions".
7. All packages and essential pre-setting work are pre-loaded in `R/prepare.R`.
8. Data assimilation codes are stored in folder "DA_scripts".
9. Data assimilation results are stored in folder "DA_output".
10. The folder 'R" synthesized all the individual scripts and compute the budget related variables and statistics. 
11. All key output of the budget are stored in folder "output".

For any questions, please contact: m.jiang@westernsydney.edu.au



