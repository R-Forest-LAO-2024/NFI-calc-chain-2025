
##
## Lao PDR NFI cycle 4 data analysis #####
##


##
## Description ####
##

## See README.md


##
## Calculation chain initiation ####
##

## + Get 'here' package first to avoid issues with relative paths #### 
## Useful when sourcing script from Quarto doc.
if (!require(here)) install.packages("here"); library(here)


## + Get user inputs ####

source(here("R/user/00-user-inputs.R"))

## + Run Setup ####

source(here("R/setup/init.R"))

source(here("R/setup/paths.R"))

source(here("R/setup/sampling.R"))

source(here("R/setup/load-anci.R"))


## + Load NFI data, prepare from raw table(s) if needed ####

source(here("R/setup/get-data.R"))



##
## Data preparation ####
##

## + Add custom functions for analysis ####

source(here("R/user/00-common.R"))

## + Clean data ####
## GS: Mainly entity code issues

source(here("R/user/00a-clean-subplot.R"))

source(here("R/user/00b-clean-lcs.R"))

source(here("R/user/00c-clean-tree.R"))

source(here("R/user/00d-clean-dw.R"))

source(here("R/user/00e-clean-ldw.R"))

source(here("R/user/00f-clean-stump.R"))


## + Get all tables from 'data_clean' list into the Global Environment ####

source(here("R/user/00i-clean-assign.R"))


## + Add Chave et al. 2014 environmental factor E to ancillary data ####

source(here("R/user/00m-extract-chaveE.R"))


## + Add wood density averages at species and genus level to ancillary data ####

source(here("R/user/00n-average-wood-density.R"))


## + Join tables ####

## Land cover section assigned to tree and dw in the cleaning phase

source(here("R/user/00z-tree-join.R"))

source(here("R/user/00z-dw-join.R"))

source(here("R/user/00z-ldw-join.R"))

source(here("R/user/00z-stump-join.R"))


##
## Entity level calculation #### 
##


## + Tree level calculation ####

source(here("R/user/01-tree-dbh-class.R"))

source(here("R/user/02-tree-weight.R"))

source(here("R/user/05-tree-ba.R"))

source(here("R/user/06-tree-agb.R"))

source(here("R/user/07-tree-bgb.R"))

source(here("R/user/08-tree-carbon.R"))


## + Deadwood calculation ###

source(here("R/user/11-dw-agb.R"))

source(here("R/user/12-ldw-agb.R"))

source(here("R/user/13-stump-agb.R"))



## + Save tables ####

# source(here("R/user/99-save-tables.R"))



##
## Aggregate data ####
##

# source(here("R/analytics/strata-weights.R"))


## 
## Other analytics ####
##

## Carbon stock based on ratio estimator
source(here("R/user/99-cstock.R"))

## + Nb tree per DBH class
# source(here("R/user/99-nb-tree-dbh-class.R"))



