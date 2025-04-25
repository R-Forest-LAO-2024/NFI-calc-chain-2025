
## Initiate list for temporary objects
tmp_read <- list()

## Get ext
tmp_read$get_ext <- str_remove(usr$get_filename, ".*\\.")

if (tmp_read$get_ext == "zip") {
  
  ## !!! NOT TESTED WITH CYCLE 4 DATA
  stop("ZIP input not supported, to be consistent with ONA API, only CSV with names supported at the moment")
  
  # ## Create directory for the data
  # tmp_read$dir_name <- str_remove(usr$get_filename, "\\..*")
  # tmp_read$dir_path <- file.path(path$dat$src, tmp_read$dir_name)
  # dir.create(tmp_read$dir_path, showWarnings = F)
  # 
  # ## Unzip file
  # unzip(
  #   zipfile = file.path(path$dat$src, usr$get_filename),
  #   exdir = tmp_read$dir_path
  # )
  # 
  # ## List data and create names
  # tmp_read$list_csv <- list.files(tmp_read$dir_path, pattern = "\\.csv", full.names = T)
  # tmp_read$names <- tmp_read$list_csv |> 
  #   str_replace(str_subset(tmp_read$list_csv, "data\\.csv"), "treeplot_init") |>
  #   str_replace(str_subset(tmp_read$list_csv, "tree_data_nest1"), "tree_nest1_init") |>
  #   str_replace(str_subset(tmp_read$list_csv, "tree_data_nest2"), "tree_nest2_init") |>
  #   str_replace(str_subset(tmp_read$list_csv, "sapling"), "sapling_init") |>
  #   str_replace(str_subset(tmp_read$list_csv, "ldw"), "ldw_init") |>
  #   str_replace(str_subset(tmp_read$list_csv, "ntfp"), "ntfp_init")
  # 
  # ## Read data
  # tmp_read$data_init <- map(tmp_read$list_csv, function(x) {
  #   tt <- read_csv(x, col_types = cols(.default = "c")) |>
  #     rename_with(.cols = everything(), str_replace_all, "/", "__") |>
  #     rename_with(.cols = starts_with("_"), str_replace, "_", "ONA_")
  #   })
  # 
  # ## Rename data
  # names(tmp_read$data_init) <- tmp_read$names
  # 
  # ## Transfer to data_init
  # data_init <- append(data_init, tmp_read$data_init)
  
} else if (tmp_read$get_ext == "csv") {
  
  ## Read data directly into data_init
  data_init$master_csv <- read_csv(
    file.path(path$dat$src, usr$get_filename), col_types = cols(.default = "c"), na = "n/a"
    ) |>
    rename_with(.cols = everything(), str_replace_all, "/", "__") |>
    rename_with(.cols = everything(), str_replace_all, "\\[|\\]", "___") |>
    rename_with(.cols = everything(), str_replace_all, "_____", "___") |>
    rename_with(.cols = starts_with("_"), str_replace, "_", "ONA_") |>
    mutate(ONA_index = row_number())
  
}

## Update log
write_lines(
  paste0(
    local_time("UTC"), ": Read data from manual download \n",
    "data type: ", tmp_read$get_ext, "\n",
    "data_init now contains: ", paste(names(data_init), collapse = ", "), "\n\n"
  ),
  here("log.txt") , append = T
)

## Clean tmp elements
rm(tmp_read)

