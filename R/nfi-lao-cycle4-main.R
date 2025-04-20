
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

source(here("R/user/00-user-inputs.R"), local = T)


## Run Setup ####

source(here("R/setup/init.R"), local = T)

source(here("R/setup/paths.R"), local = T)

source(here("R/setup/sampling.R"), local = T)

source(here("R/setup/load-anci.R"), local = T)


## Load NFI data, prepare if needed ####

source(here("R/setup/get-data.R"), local = T)


## Clean data  ####
## Mainly entity code issues

source(here("R/user/01a-clean-subplot.R"), local = T)

source(here("R/user/01b-clean-tree.R"), local = T)


## Run analysis #### 



