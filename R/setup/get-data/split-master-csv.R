
## Initiate list to store temporary objects
tmp <- list()

## !!! The data is spread over a thd columns as one row is one subplot, 
##     tree info is in new column for each tree.

## Get table names for main entities
tmp$tab_names <- names(data_init$master_csv) |>
  str_subset("ldw_data_rep|ntfp_rep|sapling_data_rep|tree_data_nest1_rep|tree_data_nest2_rep") |>
  str_remove("___.*") |>
  unique()

tmp$tab_names_new <- tmp$tab_names |>
  str_replace(str_subset(tmp$tab_names, "tree_data_nest1"), "tree_init1") |>
  str_replace(str_subset(tmp$tab_names, "tree_data_nest2"), "tree_init2") |>
  str_replace(str_subset(tmp$tab_names, "sapling"), "sapling_init") |>
  str_replace(str_subset(tmp$tab_names, "ldw"), "ldw_init") |>
  str_replace(str_subset(tmp$tab_names, "ntfp"), "ntfp_init")

## Extract subplot level, equivalent to data in ONA
data_init$subplot_init <- data_init$master_csv |> 
  select(-starts_with(tmp$tab_names))

## Extract other tables that require pivot_longer()
tmp$data_init <- map(tmp$tab_names, fct_extract_from_csv, .httr_csv_content = data_init$master_cs)

names(tmp$data_init) <- tmp$tab_names_new

data_init <- append(data_init, tmp$data_init)

data_init$master_csv <- NULL

## Update log
write_lines(
  paste0(
    local_time("UTC"), ": Convert master CSV to main elements \n",
    "main elements: ", paste(names(data_init), collapse = ", "), "\n",
    "respective number of rows: ", paste(map(data_init, nrow), collapse = ", ") , "\n\n"
  ),
  here("log.txt") , append = T
)

## Clean tmp elements
rm(tmp)
