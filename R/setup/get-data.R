
## RUN ONLY IF USER REQUEST NEW DATA
if (usr$recompile_source) {
  
  ## Load helpers
  source(here("R/setup/get-data/helpers.R"), local = T)
  
  ## REMOVE EXISTING FILES ####
  
  ## Remove all harmo and clean files
  unlink(list.files(path$dat$prep, full.names = T, pattern = "\\.csv"))
  unlink(list.files(path$dat$clean, full.names = T, pattern = "\\.csv"))
  
  ## Clean source files if requested
  if (usr$clean_all) {
    unlink(list.files(path$dat$src, full.names = T), recursive = T)
  }
  
  ## GET INITIAL DATA ####
  
  ## Get new file, download or unzip if necessary
  if (usr$download_new) {
    source(here("R/setup/get-data/download-ona.R"), local = T)
  } else {
    source(here("R/setup/get-data/read-manual.R"), local = T)
  }
  
  ## If file is CSV, need to make entities
  if ("master_csv" %in% names(data_init)) {
    source(here("R/setup/get-data/split-master-csv.R"), local = T)
  }
  
  
  ## PREPARE ENTITY BASED TABLES WITH SIMPLIFIED NAMING ####
  
  source(here("R/setup/get-data/prepa-subplot.R"), local = T)
  
  source(here("R/setup/get-data/prepa-nest.R"), local = T)
  
  source(here("R/setup/get-data/prepa-sapling.R"), local = T)
  
  source(here("R/setup/get-data/prepa-ntfp.R"), local = T)
  
  source(here("R/setup/get-data/prepa-ldw.R"), local = T)
  
  source(here("R/setup/get-data/prepa-write.R"), local = T)
  
  #rm(data_init)
  
} else {
  
  tmp <- list()
  
  tmp$prep_files <- list.files(path$dat$prep, pattern = "\\.csv", full.names = T)
  tmp$sort_date <- tmp$prep_files |> 
    str_remove(".*202[0-9]-") |>
    str_remove("\\.csv") |>
    unique() |> 
    sort(decreasing = T)
  
  tmp$latest_date <- tmp$sort_date[1]
  tmp$prep_select <- tmp$prep_files |> str_subset(pattern = tmp$latest_date)
  tmp$prep_names  <- tmp$prep_select |>
    str_remove(".*/") |>
    str_remove("_202[0-9].*")
  
  data_prep <- map(tmp$prep_select, read_csv, show_col_type = F)
  names(data_prep) <- tmp$prep_names
}







