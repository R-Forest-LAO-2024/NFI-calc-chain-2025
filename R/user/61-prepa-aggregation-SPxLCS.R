

## Compared to previous version (20-prepa-aggregation.R), this version builds subplot level tables for the aggregation function
## The idea is that all entities can be aggregated to the smallest measurement area.
## In case of Lao that area, coded as subplot, is in fact the land cover section or subplot x land cover section.


## Need same land cover, sub-population and stratum name across tables

tmp <- list()

##
## Phase 1 data from CEO ####
##

## Add Phase 2 plot number to Phase 1 table
## Add stratum number based on land cover

tmp$ceo_fid <- anci$ceo_nfi_id_all |> select(ph2_plot_no = plotid_all, ph1_plot_no = pl_orig_fid)
tmp$ph1_lc_class <- anci$lc_ceo |> select(ph1_lc_no = lc_no, ph1_lc_code = lc_code, ph1_lc_name = lc_name, ph1_stratum_no = lc_strata_no, ph1_stratum_name = lc_strata_name)

ph1_data <- anci$ceo |>
  select(ph1_plot_no = pl_orig_fid, ph1_prov_no = pl_pcode, ph1_prov_name = pl_prov,  ph1_lc_no = LC_ID) |>
  left_join(tmp$ceo_fid, by = join_by(ph1_plot_no)) |>
  mutate(ph1_subpop_no = if_else(ph1_prov_no %in% 3:8, 1, 2)) |>
  left_join(tmp$ph1_lc_class, by = join_by(ph1_lc_no)) |>
  select(ph1_plot_no, plot_id = ph2_plot_no, subpop = ph1_subpop_no, stratum = ph1_stratum_no, ph1_lc_no) |>
  arrange(ph1_plot_no)



##
##  Phase 2 lowest measurement area ####
##

## Get subpopulation and stratum to Phase 2 table
## Add land use correction (based on FTM) and accessibility for non-visited plots
## Phase 2 data is at the minimum area level
## For ratio estimator subplot is in practice subplot x land cover section (5 lcs per subplot, 20 per plot)

## Prepare data to link
tmp$shifted_lc <- anci$shifted_spA |> select(plot_id = plot_code_, ph1_lc_no_shifted = CEO_Phase_I)

tmp$ph1_info <- ph1_data |>
  filter(!is.na(plot_id)) |>
  select(plot_id, subpop, ph1_lc_no) |>
  left_join(tmp$shifted_lc, by = join_by(plot_id)) |>
  mutate(
    ph1_lc_no_ceo = ph1_lc_no,
    ph1_lc_no = if_else(!is.na(ph1_lc_no_shifted), ph1_lc_no_shifted, ph1_lc_no_ceo)
  ) |>
  left_join(tmp$ph1_lc_class, by = join_by(ph1_lc_no)) |>
  select(plot_id, subpop, stratum = ph1_stratum_no)

tmp$sp_access <- subplot |> select(plot_id = subplot_plot_no, subplot_no, subplot_access_txt = subplot_access) 

tmp$lcs_lc <- lcs_ |> select(plot_id = lcs_plot_no, subplot_no = lcs_subplot_no, lcs_no, lc_no_field = lcs_lu_class_no)

tmp$ftm_lc <- anci$ceo_nfi_id |>
  left_join(anci$ceo_nfi_id_all, b = join_by(ORIG_FID == pl_orig_fid, ID)) |>
  select(plot_id = plotid_all, subplot_no = Plot, lc_no_ftm = FTM2022) |>
  mutate(
    lc_no_ftm = case_when(
      lc_no_ftm == 16 ~ 169,
      TRUE ~ lc_no_ftm
    )
  )

tmp$vec_ph2 <- ph1_data |> filter(!is.na(plot_id)) |> pull(plot_id)

## Check
length(tmp$vec_ph2)

## Make subplot table 
ph2_subplot <- expand_grid(
  plot_id = tmp$vec_ph2,
  subplot_no = c("A", "B", "C", "D"),
  lcs_no = 1:5
) |>
  mutate(subplot_id = paste0(subplot_no, lcs_no)) |>
  left_join(tmp$ph1_info, by = join_by(plot_id)) |>
  left_join(tmp$sp_access, by = join_by(plot_id, subplot_no)) |>
  left_join(tmp$lcs_lc, by = join_by(plot_id, subplot_no, lcs_no)) |>
  left_join(tmp$ftm_lc, by = join_by(plot_id, subplot_no)) |>
  mutate(
    lc_no = if_else(!is.na(lc_no_field), lc_no_field, lc_no_ftm),
    lc_no_origin = if_else(!is.na(lc_no_field), "field", "FTM2022"),
    access = case_when(
      plot_id <= 636 & subplot_access_txt == "accessible" ~ TRUE,
      plot_id <= 636 & subplot_access_txt != "accessible" ~ FALSE,
      plot_id <= 636 & is.na(subplot_access_txt) ~ FALSE,
      stratum %in% 1:3 ~ FALSE,
      stratum == 4 ~ TRUE,
      TRUE ~ NA
    ),
    # sp_area = (pi*16^2 / 5) / 10000
    sp_area = if_else(lcs_no == 1, 12^2, (pi*16^2 - 12^2)/4) / 10000
  ) |>
  select(plot_id, subplot_id, subpop, stratum, lc_no, subplot_access_txt, access, sp_area, subplot_no, lcs_no) |>
  arrange(plot_id)

## Checks
table(ph2_subplot$lc_no, useNA = "ifany")
table(ph2_subplot$access, useNA = "ifany")



## 
## Phase 2 entity level ####
##

## Aggregate entities to subplot level
## ldw and sapling follow a different aggregation path than tree, dw and stumps

## + tree AGB & BGB ####

ph2_sp_tree <- tree |>
  select(plot_id = subplot_plot_no, subplot_no, lcs_no = tree_lcs_no, tree_no, tree_stem_no, tree_dbh, tree_weight, tree_ba, tree_agb_final, tree_bgb) |>
  mutate(
    subplot_id = paste0(subplot_no, lcs_no)
  ) |>
  group_by(plot_id, subplot_id) |>
  summarise(
    n_tree = sum(tree_weight),
    ba     = sum(tree_ba * tree_weight),
    agb    = sum(tree_agb_final * tree_weight) / 1000, ## Convert kg to tons
    bgb    = sum(tree_bgb * tree_weight) / 1000,
    .groups= "drop"
  ) |>
  right_join(select(ph2_subplot, plot_id, subplot_id, sp_area), by = join_by(plot_id, subplot_id)) |>
  mutate(
    n_tree = if_else(!is.na(n_tree), n_tree, 0),
    ba     = if_else(!is.na(ba), ba, 0),
    agb    = if_else(!is.na(agb), agb, 0),
    bgb    = if_else(!is.na(bgb), bgb, 0),
  ) |>
  select(plot_id, subplot_id, n_tree, ba, agb, bgb) |>
  arrange(plot_id, subplot_id)
ph2_sp_tree


## + DW ####
ph2_sp_dw <- dw |>
  select(plot_id = subplot_plot_no, subplot_no, lcs_no = dw_lcs_no, dw_no, dw_dbh, dw_agb) |>
  mutate(
    subplot_id = paste0(subplot_no, lcs_no),
    dw_weight = if_else(dw_dbh < 30, 4, 1) ## here inclusion proba for small trees is 16^2.pi / 8^2.pi = 4
  ) |>
  group_by(plot_id, subplot_id) |>
  summarise(
    n_dw = sum(dw_weight),
    dw   = sum(dw_agb * dw_weight) / 1000,
    .groups= "drop"
  ) |>
  right_join(select(ph2_subplot, plot_id, subplot_id, sp_area), by = join_by(plot_id, subplot_id)) |>
  mutate(
    n_dw = if_else(!is.na(n_dw), n_dw, 0),
    dw   = if_else(!is.na(dw), dw, 0)
  ) |>
  select(plot_id, subplot_id, n_dw, dw) |>
  arrange(plot_id, subplot_id)
ph2_sp_dw

## + STUMP ####
ph2_sp_stump <- stump |>
  select(plot_id = subplot_plot_no, subplot_no, lcs_no = stump_lcs_no, stump_no, stump_d = stump_diameter_mean, stump_agb) |>
  mutate(
    subplot_id = paste0(subplot_no, lcs_no),
    stump_weight = if_else(stump_d < 30, 4, 1) ## here inclusion proba for small trees is 16^2.pi / 8^2.pi = 4
  ) |>
  group_by(plot_id, subplot_id) |>
  summarise(
    n_stump = sum(stump_weight),
    stump   = sum(stump_agb * stump_weight) / 1000,
    .groups= "drop"
  ) |>
  right_join(select(ph2_subplot, plot_id, subplot_id, sp_area), by = join_by(plot_id, subplot_id)) |>
  mutate(
    n_stump = if_else(!is.na(n_stump), n_stump, 0),
    stump   = if_else(!is.na(stump)  , stump    , 0)
  ) |>
  select(plot_id, subplot_id, n_stump, stump) |>
  arrange(plot_id, subplot_id)
ph2_sp_stump

## + Attributes not measured on all land cover sections ####

## + + combined area of LCS matching the center land cover ####
center_sp <- ph2_subplot |>
  filter(lcs_no == 1) |>
  select(plot_id, subplot_no, lc_center = lc_no)

ph2_sp_main <- ph2_subplot |>
  left_join(center_sp, by = join_by(plot_id, subplot_no)) |>
  filter(lc_no == lc_center) |>
  group_by(plot_id, subplot_no, lc_center) |>
  summarise(sp_area_centerlc = sum(sp_area), .groups = "drop")

## + + Sapling ####
sapling_aggregate <- sapling |>
  mutate(sapling_area = pi*2^2/10000) |>
  group_by(subplot_plot_no, subplot_no) |>
  summarise(
    sap_agb_ha = sum(sapling_agb / sapling_area) / 1000,
    .groups = "drop"
  )

ph2_sp_sap <- ph2_subplot |>
  left_join(ph2_sp_main, by = join_by(plot_id, subplot_no)) |>
  left_join(sapling_aggregate, by = join_by(plot_id == subplot_plot_no, subplot_no)) |>
  mutate(
    sap_agb_ha_corr = if_else(is.na(sap_agb_ha) | lc_no != lc_center , 0, sap_agb_ha * (pi*16^2/10000) / sp_area_centerlc),
    sap_agb = sap_agb_ha_corr * sp_area
  ) |>
  select(plot_id, subplot_id, sap_agb) |>
  arrange(plot_id, subplot_id)


## + + lying deadwood ####
ph2_sp_ldw <- ph2_subplot |>
  left_join(ph2_sp_main, by = join_by(plot_id, subplot_no)) |>
  left_join(ldw_aggregate, by = join_by(plot_id == subplot_plot_no, subplot_no)) |>
  mutate(
    ldw_ha_corr = if_else(is.na(ldw_agb_ha) | lc_no != lc_center , 0, ldw_agb_ha / 1000 * (pi*16^2/10000) / sp_area_centerlc),
    ldw = ldw_ha_corr * sp_area
  ) |>
  select(plot_id, subplot_id, ldw) |>
  arrange(plot_id, subplot_id)


##
## Combined entities at subplot level ####
##

ph2_subplot <- ph2_subplot |>
  mutate(
    n_tree  = NA,
    ba      = NA,
    agb     = NA,
    bgb     = NA,
    sap_agb = NA,
    n_dw    = NA,
    dw      = NA,
    n_stump = NA,
    ldw     = NA,
  ) |>
  left_join(ph2_sp_tree, by = join_by(plot_id, subplot_id), suffix = c("_rm", "")) |>
  left_join(ph2_sp_sap, by = join_by(plot_id, subplot_id), suffix = c("_rm", "")) |>
  left_join(ph2_sp_dw, by = join_by(plot_id, subplot_id), suffix = c("_rm", "")) |>
  left_join(ph2_sp_stump, by = join_by(plot_id, subplot_id), suffix = c("_rm", "")) |>
  left_join(ph2_sp_ldw, by = join_by(plot_id, subplot_id), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    Btot = agb + bgb + sap_agb + dw + stump + ldw,
    Ctot = Btot * CF
  )
ph2_subplot


## Remove all tmp objects
rm(list = str_subset(ls(), pattern = "ph2_sp_"))
rm(center_sp)
rm(tmp)
