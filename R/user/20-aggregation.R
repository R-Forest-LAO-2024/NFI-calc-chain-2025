
##
## Steps ####
##

## Make combinations of all plots and lcs levels based on input plot list in Phase 2.
## Add strata from CEO + shifted plot correction
## Add land cover class based on field observation + FTM values for non-visited.
## Add access status based on field + all non-visited in strata 1:3 are considered non-accessible
## Aggregate from each LC and for total

## Input: 
## ceo_nfi_id_add
## ceo
## shifted_spA
## tree
## lcs
## subplot

##
## Data preparation ####
##

tmp <- list()

## Need same land cover, sub-population and stratum name across tables ####

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


## + Phase 2 data ####
## Get subpopulation and stratum to Phase 2 table
## Add land use correction (based on FTM) and accessibility for non-visited plots
## Phase 2 data is at the minimum area level
## For ratio estimator subplot is in practice land cover section
tmp$ph1_info <- ph1_data |>
  filter(!is.na(ph2_plot_no)) |>
  select(plot_no = ph2_plot_no, ph1_subpop_no, ph1_lc_no, ph1_stratum_no, ph1_stratum_name)
tmp$sp_access <- subplot |> select(plot_no = subplot_plot_no, subplot_no, subplot_access) 
tmp$lcs_lc <- lcs_ |> select(plot_no = lcs_plot_no, subplot_no = lcs_subplot_no, lcs_no, lc_no = lcs_lu_class_no)
tmp$ftm_lc <- anci$ceo_nfi_id |>
  left_join(anci$ceo_nfi_id_all, b = join_by(ORIG_FID == pl_orig_fid, ID)) |>
  filter(!plot_visited) |>
  select(plot_no = plotid_all, subplot_no = Plot, ph2_lc_no_ftm = FTM2022) |>
  mutate(
    ph2_lc_no_ftm = case_when(
      ph2_lc_no_ftm == 16 ~ 169,
      TRUE ~ ph2_lc_no_ftm
    )
  )
tmp$vec_ph2 <- ph1_data |> filter(!is.na(ph2_plot_no)) |> pull(ph2_plot_no)

length(tmp$vec_ph2)

ph2_subplot <- expand_grid(
  plot_no = tmp$vec_ph2,
  subplot_no = c("A", "B", "C", "D"),
  lcs_no = 1:5
) |>
  left_join(tmp$ph1_info, by = join_by(plot_no)) |>
  left_join(tmp$sp_access, by = join_by(plot_no, subplot_no)) |>
  left_join(tmp$lcs_lc, by = join_by(plot_no, subplot_no, lcs_no)) |>
  left_join(tmp$ftm_lc, by = join_by(plot_no, subplot_no)) |>
  arrange(plot_no)



## lcs_all <- lcs_all |> filter(!plot_no %in% c(106, 139, 154, 439, 534, 578, 1163))
## Count as inaccessible


## Need to assign accessibility for non-visited plots ####


## This function is part of a series of aggregation functions for NFI analysis
## function 3: double sampling for stratification with ratio estimator
nfi_aggregate3 <- function(.out_var, .out_class, .subpop, .strata, .df_ph1, .df_ph2, .tree, .subplot){
  
  ## !!! FOR TESTING ONLY
  .out_var = tree_agb_final
  .out_class = lu_txt
  .subpop = ceo_subpop_no
  .strata = ceo_stratum_no
  .ph_info = 
  
}




