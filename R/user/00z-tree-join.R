

tmp <- list()


## NEW METHOD: INITIATE JOINED COL AND CONTRIOL SUFFIX TO AVOID ERROR WHEN JOINING TWICE THE SAME COLS

## Extract codes from subplot table
tmp$subplot_codes <- subplot |> select(ONA_index, subplot_plot_no, subplot_no, subplot_lc_class_center)

## plot, subplot and LCS codes ####
tree <- tree |>
  mutate(
    subplot_plot_no = NA, 
    subplot_no = NA, 
    subplot_lc_class_center = NA 
  ) |>
  left_join(tmp$subplot_codes, by = join_by(ONA_parent_index == ONA_index), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))


## LU class ####
tree <- tree |>
  mutate(
    lcs_name = NA,
    lcs_lu_class_txt = NA,
  ) |>
  left_join(
    y = lcs, 
    by = join_by(subplot_plot_no == lcs_plot_no, subplot_no == lcs_subplot_no, tree_lcs_no == lcs_no),
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
tree <- tree |>
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

## Chave E ####
tree <- tree |>
  mutate(
    plot_E = NA
  ) |>
  left_join(anci$plot_chaveE, by = join_by(subplot_plot_no == plot_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))

## Species names ####
tree <- tree |>
  mutate(
    tree_species_scientific_name = NA,
    tree_species_local_name = NA
  ) |>
  left_join(anci$species_list, by = join_by(tree_species_code), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>  
  mutate(
    tree_genus_name = word(tree_species_scientific_name),
    tree_species_binomial = if_else(
      is.na(word(tree_species_scientific_name, 2)),
      tree_genus_name, 
      paste(word(tree_species_scientific_name), word(tree_species_scientific_name, 2))
    )
  )

## Wood density ####
tree <- tree |>
  mutate(
    wd_species_mean = NA,
    wd_genus_mean = NA
  ) |>
  left_join(anci$wd_species, by = join_by(tree_species_binomial == wd_species), suffix = c("_rm", "")) |>
  left_join(anci$wd_genus, by = join_by(tree_genus_name == wd_genus), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    wd_all = case_when(
      !is.na(wd_species_mean) ~ wd_species_mean,
      !is.na(wd_genus_mean) ~ wd_genus_mean,
      TRUE ~ anci$wd_default
    ),
    wd_level = case_when(
      !is.na(wd_species_mean) ~ "species",
      !is.na(wd_genus_mean) ~ "genus",
      TRUE ~ "default"
    )
  )
  

## Add codes to lcs 
lcs_ <- lcs |>
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
  ) |>
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



# OLD METHOD: CHANGING RESULT OBJ NAME TO AVOID SUFFIX ISSUE
# tree_ <- tree |>
#   ## Replace ONA with plot/subplot codes
#   left_join(tmp$subplot_codes, by = join_by(ONA_parent_index == ONA_index)) |>
#   ## Join LC code based on section 
#   left_join(lcs, by = join_by(subplot_plot_no == lcs_plot_no, subplot_no == lcs_subplot_no, tree_lcs_no_new == lcs_no)) |>
#   mutate(
#     lcs_class_num = case_when(
#       lcs_class == "16AC" ~ "161",
#       lcs_class == "16EC" ~ "162",
#       lcs_class == "16PN" ~ "163",
#       lcs_class == "16RB" ~ "164",
#       lcs_class == "16TK" ~ "165",
#       lcs_class == "16OTH" ~ "169",
#       TRUE ~ lcs_class
#     ),
#     lcs_class_num = as.numeric(lcs_class_num)
#   ) |>
#   ## Join land cover full names based on code
#   left_join(anci$lc, by = join_by(lcs_class_num == lc_no)) |>
#   rename_with(.cols = starts_with("lc_"), str_replace, pattern = "lc_", replacement = "lcs_") |>
#   ## Join Chave E for 2014 model
#   left_join(anci$plot_chaveE, by = join_by(subplot_plot_no == plot_no)) |>
#   ## Join species names based on species code
#   left_join(anci$species_list, by = "tree_species_code") |>
#   mutate(
#     tree_genus_name = word(tree_species_scientific_name),
#     tree_species_binomial = if_else(
#       is.na(word(tree_species_scientific_name, 2)),
#       tree_genus_name, 
#       paste(word(tree_species_scientific_name), word(tree_species_scientific_name, 2))
#     )
#   ) |>
#   ## Join wood density
#   left_join(anci$wd_species, by = join_by(tree_species_binomial == wd_species)) |>
#   left_join(anci$wd_genus, by = join_by(tree_genus_name == wd_genus)) |>
#   mutate(
#     wd_all = case_when(
#       !is.na(wd_species_mean) ~ wd_species_mean,
#       !is.na(wd_genus_mean) ~ wd_genus_mean,
#       TRUE ~ anci$wd_default
#     ),
#     wd_level = case_when(
#       !is.na(wd_species_mean) ~ "species",
#       !is.na(wd_genus_mean) ~ "genus",
#       TRUE ~ "default"
#     )
#   )


# ## Check for NAs
# table(tree_$lcs_class, useNA = "ifany")
# table(tree_$lcs_class_num, useNA = "ifany")
# table(tree_$lcs_code_new, useNA = "ifany")
# table(tree_$lcs_type, useNA = "ifany")
# table(tree_$wd_level, useNA = "ifany")


# ## Check NAs
# tmp$check_na <- tree |>
#   filter(is.na(lcs_class)) |>
#   select(ONA_parent_index, subplot_plot_no, subplot_no, tree_lcs_no)
# 
# unique(tmp$check_na$ONA_parent_index)


## save table in 'results'
write_csv(tree, file.path(path$res$data, paste0("tree_with_joins-", Sys.Date(), ".csv")))  

## Remove tmp object
rm(tmp)
