

tmp <- list()


## NEW METHOD: INITIATE JOINED COL AND CONTRIOL SUFFIX TO AVOID ERROR WHEN JOINING TWICE THE SAME COLS

## Extract codes from subplot table
tmp$subplot_codes <- subplot |> select(ONA_index, subplot_plot_no, subplot_no, subplot_lc_class_center)

## plot, subplot and LCS codes ####
dw <- dw |>
  mutate(
    subplot_plot_no = NA, 
    subplot_no = NA, 
    subplot_lc_class_center = NA 
  ) |>
  left_join(tmp$subplot_codes, by = join_by(ONA_parent_index == ONA_index), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))


## LU class ####
dw <- dw |>
  mutate(
    lcs_name = NA,
    lcs_lu_class_txt = NA,
  ) |>
  left_join(
    y = lcs, 
    by = join_by(subplot_plot_no == lcs_plot_no, subplot_no == lcs_subplot_no, dw_lcs_no == lcs_no),
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

## LC names ####
dw <- dw |>
  mutate(
    lu_code_new = NA,
    lu_name = NA,
    lu_name_lao = NA,
    lu_type = NA,
    lu_strata_no = NA,
    lu_strata_name = NA
  ) |>
  left_join(anci$lc, by = join_by(lcs_lu_class_no == lu_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))



## save table in 'results'
write_csv(dw, file.path(path$res$data, paste0("dw_with_joins-", Sys.Date(), ".csv")))  

## Remove tmp object
rm(tmp)
