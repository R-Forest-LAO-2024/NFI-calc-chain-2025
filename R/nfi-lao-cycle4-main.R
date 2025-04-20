
##
## Lao PDR NFI cycle 4 data analysis #####
##

## Description ####

## See README.md


## Get 'here' package first to avoid issues with relative paths #### 
## Useful when sourcing script from Quarto doc.
if (!require(here)) install.packages("here")
library(here)

## Get user inputs ####

source(here("R/user/00-user-inputs.R"))


## Run Setup ####

source(here("R/setup/init.R"))

source(here("R/setup/paths.R"))

source(here("R/setup/sampling.R"))

source(here("R/setup/load-anci.R"))


## Load NFI data, prepare if needed ####

source(here("R/setup/get-data.R"))


## Clean data  ####
## Mainly entity code issues

source(here("R/user/01a-clean-subplot.R"))

source(here("R/user/01b-clean-lcs.R"))

source(here("R/user/01c-clean-tree.R"))


## Run analysis #### 



