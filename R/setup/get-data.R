
## RUN ONLY IF USER REQUEST NEW DATA
if (usr$get_new) {
  
  ## Initiate list to store source data
  data_init <- list()
  data_prep <- list()
  data_clean <- list()
  
  ## Initiate list for results
  res <- list()
  
  ## Load helpers
  source(here("R/setup/get-data/helpers.R"), local = T)
  
  ## REMOVE EXISTING FILES ####
  
  ## Remove all harmo and clean files
  unlink(list.files(path$dat$harmo, full.names = T, pattern = "\\.csv"))
  unlink(list.files(path$dat$clean, full.names = T, pattern = "\\.csv"))
  
  ## Clean source files if requested
  if (usr$clean_all) {
    unlink(list.files(path$dat$src, full.names = T), recursive = T)
  }
  
  ## GET INITIAL DATA ####
  
  ## Get new file, download or unzip if necessary
  if (usr$get_auto) {
    source(here("R/setup/get-data/download-ona.R"), local = T)
  } else if (!usr$get_auto) {
    source(here("R/setup/get-data/read-manual.R"), local = T)
  }
  
  ## If file is CSV, need to make entities
  if ("master_csv" %in% names(data_init)) {
    source(here("R/setup/get-data/split-master-csv.R"), local = T)
  }
  
  
  ## PREPARE ENTITY BASED TABLES WITH SIMPLIFIED NAMING ####
  
  source(here("R/setup/get-data/prepa-subplot.R"), local = T)
  
  
  ## CORRECT DATA ENTRY TYPOS (MAINLY CODE ISSUES) ####
  
  source(here("R/setup/get-data/clean-subplot.R"), local = T)
  
  
  
  
  source(here("R/setup/get-data/clean-tree.R"), local = T)
  
  source(here("R/setup/get-data/clean-subplot.R"), local = T)
  
  source(here("R/setup/get-data/clean-subplot.R"), local = T)
  
  source(here("R/setup/get-data/clean-subplot.R"), local = T)
  
  
  ## HARMONIZE TO ENTITY BASED TABLES ####
  
  source(here("R/setup/get-data/harmo-treeplot_corr.R"), local = T)
  
  source(here("R/setup/get-data/harmo-plot.R"), local = T)
  
  source(here("R/setup/get-data/harmo-nest.R"), local = T)
  
  source(here("R/setup/get-data/harmo-ntfp.R"), local = T)
  
  source(here("R/setup/get-data/harmo-write.R"), local = T)
  
  #rm(data_init)
  
}







