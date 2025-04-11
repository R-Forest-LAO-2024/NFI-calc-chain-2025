
fct_extract_from_csv <- function(.httr_csv_content, .pattern) {
  
  ## !!! FOR TESTING ONLY
  # .httr_csv_content = data_init$master_csv
  # .pattern = "survey__reference_trees__measure__tree_data_nest1__tree_data_nest1_rep"
  ## !!!
  
  if (!is.data.frame(.httr_csv_content)) stop(".httr_csv_content should be a data frame")
  if (!is.character(.pattern)) stop(".pattern should be a character vector")
  if (length(.pattern) != 1)   stop(".pattern should be of size 1")
  
  out_df <- .httr_csv_content |>
    select(ONA_parent_index = ONA_index, starts_with(.pattern)) |>
    rename_with(.cols = starts_with(.pattern), str_extract, "___.*") |>
    pivot_longer(
      cols = starts_with("___"), 
      cols_vary = "slowest",
      names_to = c("ONA_no", ".value"), 
      names_pattern = "___(.*[0-9])___(.*)",
      values_drop_na = TRUE
    ) |>
    mutate(ONA_no = as.numeric(ONA_no)) |>
    arrange(ONA_parent_index, ONA_no) |>
    mutate(ONA_index = row_number()) |>
    select(ONA_parent_index, ONA_index, ONA_no, everything())
  
  out_df
  
}
