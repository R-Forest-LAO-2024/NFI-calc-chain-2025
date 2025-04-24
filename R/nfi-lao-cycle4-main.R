
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

source(here("R/user/00a-clean-subplot.R"))

source(here("R/user/00b-clean-lcs.R"))

source(here("R/user/00c-clean-tree.R"))

source(here("R/user/00z-clean-assign.R"))

## Run analysis #### 

source(here("R/user/00-common.R"))

source(here("R/user/01-tree-dbh-class.R"))

source(here("R/user/02-tree-lcs.R"))

source(here("R/user/03-tree-lcs-join.R"))

source(here("R/user/04-tree-ba.R"))

source(here("R/user/05-tree-agb.R"))

