
## Initiate list to store user inputs 
usr <- list()

## Erase cleaned and harmonized data and recompile from source
usr$recompile_source <- TRUE

## Download source data from ONA
usr$download_new <- FALSE

## Delete all source data (useful if new data gets downloaded)
usr$clean_all <- FALSE

## File name of the NFI source data if not downloaded
usr$get_filename <-  "4th_NFI_2025_06_04_09_11_28_457776.csv" ## "4th_NFI_up to2025_02_24_withpath.csv" ## "4th_NFI_2025_02_10_csv.zip" ## if method is manual, specify file name 

## File name of the CEO file put in 'data/data-anci'
usr$get_ceofile <- "ceo-2025-06-06-complete.csv"

## Specify timezone to get correct timestamps even outside country
usr$time_zone <- "Asia/Bangkok"

## Save intermediate tables
usr$save_csv <- FALSE
