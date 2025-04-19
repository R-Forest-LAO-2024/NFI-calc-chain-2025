
## User inputs for user 793500 ####

if (!exists("usr")) {
  usr <- list()
  ## Erase cleaned and harmonized data and recompile from source
  usr$recompile_source <- FALSE
  ## Download source data from ONA
  usr$download_new     <- FALSE
  ## Delete all source data (useful if new data gets downloaded)
  usr$clean_all        <- FALSE
  ## File name of the NFI source data if not downloaded
  usr$get_filename     <- "4th_NFI_up to2025_02_24_withpath.csv" ## "4th_NFI_2025_02_10_csv.zip" ## if method is manual, specify file name 
  ## File name of the CEO file put in 'data/data-anci'
  usr$get_ceofile      <- "ceo-25-03-07.csv"
  ## Specify timezone to get correct timestamps even outside country
  usr$time_zone        <- "Asia/Bangkok"
}

## Get 'here' package first to avoid issues with relative paths #### 

if (!require(here)) install.packages("here")
library(here)


## Run Setup ####

source(here("R/setup/init.R"), local = T)

source(here("R/setup/paths.R"), local = T)

source(here("R/setup/sampling.R"), local = T)

source(here("R/setup/load-anci.R"), local = T)


## Load NFI data, prepare if needed ####

source(here("R/setup/get-data.R"), local = T)


## Clean data  ####
## Mainly entity code issues
source(here("R/user/00-common.R"), local = T)

source(here("R/user/01a-clean-subplot.R"), local = T)

source(here("R/user/01b-clean-tree.R"), local = T)


## Run analysis #### 



