
##
## Steps ####
##

## Make combinations of all plots and lcs levels based on input plot list in Phase 2.
## Add strata from CEO + shifted plot correction
## Add land cover class based on field observation + FTM values for non-visited.
## Add access status based on field + all non-visited in strata 1:3 are considered non-accessible
## Aggregate from each LC and for total

## Input: 
## ceo_nfi_id
## ceo_nfi_id_all
## ceo
## lc_ceo (to get lc name and stratum from LC no)
## shifted_spA
## tree
## dw
## stump
## sapling
## ldw
## lcs
## subplot

##
## Data preparation ####
##

## Need same land cover, sub-population and stratum name across tables

tmp <- list()

## + Phase 1 data from CEO ####
## Add Phase 2 plot number to Phase 1 table
## Correct Phase 1 land cover and strata for shifted plots

tmp$ceo_fid <- anci$ceo_nfi_id_all |> select(ph2_plot_no = plotid_all, ph1_plot_no = pl_orig_fid)
tmp$shifted_lc <- anci$shifted_spA |> select(ph2_plot_no = plot_code_, ph1_lc_no_shifted = CEO_Phase_I)
tmp$ph1_lc_class <- anci$lc_ceo |> select(ph1_lc_no = lc_no, ph1_lc_code = lc_code, ph1_lc_name = lc_name, ph1_stratum_no = lc_strata_no, ph1_stratum_name = lc_strata_name)

ph1_data <- anci$ceo |>
  select(ph1_plot_no = pl_orig_fid, ph1_prov_no = pl_pcode, ph1_prov_name = pl_prov,  ph1_lc_no = LC_ID) |>
  left_join(tmp$ceo_fid, by = join_by(ph1_plot_no)) |>
  left_join(tmp$shifted_lc, by = join_by(ph2_plot_no)) |>
  mutate(
    ph1_subpop_no = if_else(ph1_prov_no %in% 3:8, 1, 2),
    ph1_lc_no_ceo = ph1_lc_no,
    ph1_lc_no = if_else(!is.na(ph1_lc_no_shifted), ph1_lc_no_shifted, ph1_lc_no_ceo)
  ) |>
  left_join(tmp$ph1_lc_class, by = join_by(ph1_lc_no)) |>
  mutate(ph1_lc_code = if_else(ph1_lc_code == "MDF", "MD", ph1_lc_code)) |>
  select(ph1_plot_no, ph2_plot_no, ph1_subpop_no, ph1_stratum_no, ph1_stratum_name, ph1_lc_no, ph1_lc_code, ph1_lc_name, ph1_prov_no, ph1_prov_name) |>
  arrange(ph1_plot_no)


## + Phase 2 lowest measurement area ####
## Get subpopulation and stratum to Phase 2 table
## Add land use correction (based on FTM) and accessibility for non-visited plots
## Phase 2 data is at the minimum area level
## For ratio estimator subplot is in practice land cover section

## Prepare data to link
tmp$ph1_info <- ph1_data |>
  filter(!is.na(ph2_plot_no)) |>
  select(plot_no = ph2_plot_no, ph1_subpop_no, ph1_lc_no, ph1_stratum_no, ph1_stratum_name)
tmp$sp_access <- subplot |> select(plot_no = subplot_plot_no, subplot_no, subplot_access_txt = subplot_access) 
tmp$lcs_lc <- lcs_ |> select(plot_no = lcs_plot_no, subplot_no = lcs_subplot_no, lcs_no, lc_no_field = lcs_lu_class_no)
tmp$ftm_lc <- anci$ceo_nfi_id |>
  left_join(anci$ceo_nfi_id_all, b = join_by(ORIG_FID == pl_orig_fid, ID)) |>
  select(plot_no = plotid_all, subplot_no = Plot, lc_no_ftm = FTM2022) |>
  mutate(
    lc_no_ftm = case_when(
      lc_no_ftm == 16 ~ 169,
      TRUE ~ lc_no_ftm
    )
  )
tmp$vec_ph2 <- ph1_data |> filter(!is.na(ph2_plot_no)) |> pull(ph2_plot_no)

## Check
length(tmp$vec_ph2)

## Make table 
ph2_subplot <- expand_grid(
  plot_no = tmp$vec_ph2,
  subplot_no = c("A", "B", "C", "D"),
  lcs_no = 1:5
) |>
  mutate(subplot_id = paste0(subplot_no, lcs_no)) |>
  left_join(tmp$ph1_info, by = join_by(plot_no)) |>
  left_join(tmp$sp_access, by = join_by(plot_no, subplot_no)) |>
  left_join(tmp$lcs_lc, by = join_by(plot_no, subplot_no, lcs_no)) |>
  left_join(tmp$ftm_lc, by = join_by(plot_no, subplot_no)) |>
  mutate(
    lc_no = if_else(!is.na(lc_no_field), lc_no_field, lc_no_ftm),
    lc_no_origin = if_else(!is.na(lc_no_field), "field", "FTM2022"),
    subplot_access = case_when(
      subplot_access_txt == "accessible" ~ TRUE,
      ph1_stratum_no %in% 1:3 ~ FALSE,
      ph1_stratum_no == 4 ~ TRUE,
      TRUE ~ NA
    ),
    max_meas_area = round(if_else(lcs_no == 1, 12^2, (pi*16^2 - 12^2)/4) / 10000, 4)
  ) |>
  arrange(plot_no)

## Checks
table(ph2_subplot$lc_no, useNA = "ifany")
table(ph2_subplot$subplot_access, useNA = "ifany")


## + Phase 2 entities ####

tree2 <- tree |>
  select(plot_no = subplot_plot_no, subplot_no, lcs_no = tree_lcs_no, tree_no, tree_stem_no, tree_dbh, agb = tree_agb_final, bgb = tree_bgb) |>
  mutate(
    subplot_id = paste0(subplot_no, lcs_no),
    subplot_area = case_when(
      lcs_no == 1 ~ round(12^2 / 10000, 4), ## Square of 12x12 m for all trees
      lcs_no != 1 & tree_dbh <= 30 ~ round((pi*8^2 - 12^2)/40000, 4), ## Quarter of 8 m radius circle minus the 12x12 m square 
      lcs_no != 1 & tree_dbh > 30 ~ round((pi*16^2 - 12^2)/40000, 4), ## Quarter of 16 m radius circle minus the 12x12 m square
      TRUE ~ NA_real_
    )
  )

dw2 <- dw |>
  select(plot_no = subplot_plot_no, subplot_no, lcs_no = dw_lcs_no, dw_no, dw_dbh, agb = dw_agb) |>
  mutate(
    subplot_id = paste0(subplot_no, lcs_no),
    subplot_area = case_when(
      lcs_no == 1 ~ round(12^2 / 10000, 4), ## Square of 12x12 m for all trees
      lcs_no != 1 & dw_dbh <= 30 ~ round((pi*8^2 - 12^2)/40000, 4), ## Quarter of 8 m radius circle minus the 12x12 m square 
      lcs_no != 1 & dw_dbh > 30 ~ round((pi*16^2 - 12^2)/40000, 4), ## Quarter of 16 m radius circle minus the 12x12 m square
      TRUE ~ NA_real_
    )
  )

stump2 <- stump |>
  select(plot_no = subplot_plot_no, subplot_no, lcs_no = stump_lcs_no, stump_no, stump_diameter_mean, agb = stump_agb) |>
  mutate(
    subplot_id = paste0(subplot_no, lcs_no),
    subplot_area = case_when(
      lcs_no == 1 ~ round(12^2 / 10000, 4), ## Square of 12x12 m for all trees
      lcs_no != 1 & stump_diameter_mean <= 30 ~ round((pi*8^2 - 12^2)/40000, 4), ## Quarter of 8 m radius circle minus the 12x12 m square 
      lcs_no != 1 & stump_diameter_mean > 30 ~ round((pi*16^2 - 12^2)/40000, 4), ## Quarter of 16 m radius circle minus the 12x12 m square
      TRUE ~ NA_real_
    )
  )

## This function is part of a series of aggregation functions for NFI analysis
## function 3: double sampling for stratification with ratio estimator
nfi_aggregate3 <- function(
    .ph1_df, .subpop, .stratum, .ph2_subplot_df, .plot_id, .subplot_id, .access, .max_area,
    .ph2_entity_df, .measurement_area, .out_var, .out_class
    ){
  
  # ## !!! FOR TESTING ONLY
  # ph1_df = ph1_data
  # ph2_spdf = ph2_subplot
  # ph2_endf = tree2
  # subpop = quo(ph1_subpop_no)
  # stratum = quo(ph1_stratum_no)
  # plot_id = quo(plot_no)
  # subp_id = quo(subplot_id)
  # access = quo(subplot_access)
  # max_area = quo(max_meas_area)
  # meas_area = quo(subplot_area)
  # out_var = quo(agb)
  # out_class = quo(lc_no)
  # ## !!!
  
  ## Shorten inputs
  ph1_df   <- .ph1_df
  ph2_spdf <- .ph2_subplot
  ph2_endf <- .ph2_entity
  
  ## Defuse inputs column names 
  subpop    <- enquo(.subpop)
  stratum   <- enquo(.stratum)
  plot_id   <- enquo(.plot_id)
  subp_id   <- enquo(.subplot_id)
  access    <- enquo(.access)
  max_area  <- enquo(.max_area)
  meas_area <- enquo(.measurement_area)
  out_var   <- enquo(.out_var)
  out_class <- enquo(.out_class)
  
  ## Aggregate entity to subplot
  ph2_spdf_var <- ph2_endf |>
    group_by(!!plot_id, !!subp_id) |>
    summarise(
      entity_count_ha = sum(1 / !!meas_area),
      entity_value_ha = sum(!!out_var / !!meas_area),
      .groups= "drop"
    ) |>
    right_join(ph2_spdf, by = join_by(!!plot_id, !!subp_id)) |>
    mutate(
      entity_count = if_else(!is.na(entity_count_ha), entity_count_ha * max_meas_area, 0),
      entity_value = if_else(!is.na(entity_value_ha), entity_value_ha * max_meas_area, 0)
    ) |>
    select(!!plot_id, !!subp_id, entity_count_ha, entity_value_ha, entity_count, entity_value, !!subpop, !!stratum, !!max_area) |>
    arrange(!!plot_id, !!subp_id)
    
}




