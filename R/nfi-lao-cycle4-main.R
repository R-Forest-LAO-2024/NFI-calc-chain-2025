
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

## + Clean data ####

## GS: Mainly entity code issues

source(here("R/user/00a-clean-subplot.R"))

source(here("R/user/00b-clean-lcs.R"))

source(here("R/user/00c-clean-tree.R"))

source(here("R/user/00d-clean-dw.R"))

source(here("R/user/00e-clean-assign.R"))

## + Add Chave et al. 2014 environmental factor E to ancillary data ####

source(here("R/user/00m-extract-chaveE.R"))

## + Add wood density averages at species and genus level to ancillary data ####

source(here("R/user/00n-average-wood-density.R"))


## + Join tables ####

source(here("R/user/00y-tree-prepa-join.R"))

source(here("R/user/00z-tree-join.R"))

## > Now the tree table with all info is the object 'tree_'




##
## Entity level calculation #### 
##

## + Add custom functions for analysis ####

source(here("R/user/00-common.R"))



## + Tree level calculation ####

source(here("R/user/01-tree-dbh-class.R"))

source(here("R/user/05-tree-ba.R"))

source(here("R/user/06-tree-agb.R"))

## + Manual saving of the tree data with AGB
# tree |>
#   select(
#     plot_no = subplot_plot_no, subplot_no, tree_lcs_no_new, lcs_location,
#     lcs_location, lcs_class, lcs_class_num, lcs_code_new, lcs_name, lcs_name_lao,
#     lcs_type, lcs_strata_no, lcs_strata_name,
#     subplot_lc_class_center,
#     tree_no, tree_stem_no, tree_distance, tree_azimuth, tree_x, tree_y,
#     tree_dbh, tree_pom, tree_species_code, tree_species_newlocal,
#     tree_species_scientific_name, tree_species_local_name,
#     tree_dbh_class, tree_dbh_class_reduced, tree_dbh_nest,
#     tree_ba, tree_agb
#     ) |>
#   write_csv(file.path(path$res$data, paste0("tree_live_agb-", Sys.Date(), ".csv")))

source(here("R/user/11-sdw-agb.R"))



##
## Aggregate data ####
##

source(here("R/analytics/strata-weights.R"))
