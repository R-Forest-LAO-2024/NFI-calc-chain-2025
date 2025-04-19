
## Create function to check if package is installed and if not install it ####

## Simple function
use_package <- function(.pkg_name) {
  pkg_name <- as.character(substitute(.pkg_name))
  if (!require(pkg_name, character.only = T,  quietly = TRUE)) install.packages(pkg_name, dep =TRUE)
  library(pkg_name, character.only = T, quietly = TRUE)
}

## Advanced function for Github packages FYI
# use_package <- function(.pkg_name, .github = FALSE, .gh_repo = NULL, .load = T) {
#   pkg_name <- as.character(substitute(.pkg_name))
#   if (!require(pkg_name, character.only = T,  quietly = TRUE)) {
#     if (!.github) {
#       install.packages(pkg_name, dep =TRUE)
#     } else if (.github) {
#       if (!require(remotes, quietly = TRUE)) install.packages("remotes", dep = TRUE)
#       remotes::install_github(paste0(.gh_repo))
#     }
#   }
#   if (.load) library(pkg_name, character.only = T, quietly = TRUE)
# }

use_package(usethis)
use_package(here)
use_package(readr)
use_package(stringr)
use_package(tidyr)
use_package(lubridate)
use_package(ggplot2)
use_package(purrr)
use_package(dplyr)
use_package(sf)
use_package(terra)

## Default theme for ggplot
theme_set(theme_bw())

## Initiate lists to store data and results
data_init  <- list()
data_prep  <- list()
data_clean <- list()
res_gg     <- list()
res        <- list()


## Get time in UTC
local_time <- function(tz, .spe_chr = T, .show_tz = T) {
  
  tt <- Sys.time()
  attr(tt,"tzone") <- tz
  tt <- str_remove(as.character(tt), "\\..*")
  if (.show_tz) tt <- paste0(tt, " ", tz)
  if (!.spe_chr) tt <- tt |> str_replace_all(":|/", "-") |> str_replace_all(" ", "_")
  tt
  
}

## Initiate log
write_lines(
  paste0(local_time("UTC"), ": Initiate log \n\n"), 
  file = here("log.txt")
)


