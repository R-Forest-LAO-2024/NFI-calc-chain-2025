

## !!! FOR TESTING ONLY
# tree_bkp <- tree
# tree <- tree_bkp
# rm(tree_bkp)
## !!!

tmp <- list()

## Extract codes from subplot table
tmp$subplot_codes <- subplot |> select(ONA_index, subplot_plot_no, subplot_no, subplot_lc_class_center)

tree <- tree |>
  left_join(tmp$subplot_codes, by = join_by(ONA_parent_index == ONA_index)) |>
  left_join(data_clean$lcs, by = join_by(subplot_plot_no == lcs_plot_no, subplot_no == lcs_subplot_no, tree_lcs_no == lcs_no)) |>
  mutate(
    lcs_class_num = case_when(
      lcs_class == "16AC" ~ "161",
      lcs_class == "16EC" ~ "162",
      lcs_class == "16PN" ~ "163",
      lcs_class == "16RB" ~ "164",
      lcs_class == "16TK" ~ "165",
      lcs_class == "16OTH" ~ "169",
      TRUE ~ lcs_class
    ),
    lcs_class_num = as.numeric(lcs_class_num)
  ) |>
  left_join(anci$lc, by = join_by(lcs_class_num == lc_no)) |>
  rename_with(.cols = starts_with("lc_"), str_replace, pattern = "lc_", replacement = "lcs_")


# ## Check for NAs
# table(tree$lcs_class, useNA = "ifany")
# table(tree$lcs_class_num, useNA = "ifany")
# table(tree$lcs_code_new, useNA = "ifany")
# table(tree$lcs_type, useNA = "ifany")
# 
# ## Check NAs
# tmp$check_na <- tree |>
#   filter(is.na(lcs_class)) |>
#   select(ONA_parent_index, subplot_plot_no, subplot_no, tree_lcs_no)
# 
# unique(tmp$check_na$ONA_parent_index)


## save table in 'results'
write_csv(tree, file.path(path$res$data, "tree_with_lcs.csv"))  

## Remove tmp object
rm(tmp)