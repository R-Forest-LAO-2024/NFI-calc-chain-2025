
tmp <- list()

## Extract codes from subplot table
tmp$subplot_codes <- subplot |> select(ONA_index, subplot_plot_no, subplot_no, subplot_lc_class_center)

## plot/subplot
sapling <- sapling |>
  mutate(
    subplot_plot_no = NA, 
    subplot_no = NA, 
    subplot_lc_class_center = NA 
  ) |>
  left_join(tmp$subplot_codes, by = join_by(ONA_parent_index == ONA_index), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))




## LU class ####
sapling <- sapling |>
  mutate(
    sapling_lcs_no = 1,
    lcs_name = NA,
    lcs_lu_class_txt = NA,
  ) |>
  left_join(
    y = lcs, 
    by = join_by(subplot_plot_no == lcs_plot_no, subplot_no == lcs_subplot_no, sapling_lcs_no == lcs_no),
    suffix = c("_rm", "")
  ) |>
  select(-ends_with("_rm")) |>
  mutate(
    lcs_lu_class_no = as.numeric(case_when(
      lcs_lu_class_txt == "16AC" ~ "161",
      lcs_lu_class_txt == "16EC" ~ "162",
      lcs_lu_class_txt == "16PN" ~ "163",
      lcs_lu_class_txt == "16RB" ~ "164",
      lcs_lu_class_txt == "16TK" ~ "165",
      lcs_lu_class_txt == "16OTH" ~ "169",
      TRUE ~ lcs_lu_class_txt
    ))
  )
