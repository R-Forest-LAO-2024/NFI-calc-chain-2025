
## User inputs for user 793500 ####

if (!exists("usr")) {
  usr <- list()
  usr$get_new      <- TRUE
  usr$get_auto     <- FALSE
  usr$get_filename <- "4th_NFI_up to2025_02_24_withpath.csv" ## "4th_NFI_2025_02_10_csv.zip" ## if method is manual, specify file name 
  usr$clean_all    <- FALSE
  usr$time_zone    <- "Asia/Bangkok"
}

## Get here package first to avoid issues with relative paths

if (!require("here")) install.packages("here")
library(here)


## Run analysis ####

source(here("R/setup/init.R"), local = T)

source(here("R/setup/paths.R"), local = T)

source(here("R/setup/sampling.R"), local = T)

source(here("R/setup/load-anci.R"), local = T)

source(here("R/setup/get-data.R"), local = T)




