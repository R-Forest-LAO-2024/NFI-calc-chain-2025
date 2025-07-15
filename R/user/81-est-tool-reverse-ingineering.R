
## Reverse ingeneering Est tool for total live AGB / ha
## Requires Windows environment to run activeX for calculations macros.

## Run 'Current solution' in "Options & Results" with 0 in 'Land Class - EN' 
## (change this value according to position in "Codes" to get land use specific )

## Backtrace
## Live AGB/ha in "Options & Results" trace back to "Combined" tab. 

## "Combined" 
##  Aggregates multiple cycles and subpopulations. 
## -> Multi-cycles not needed here, we can find the value directly in Cycle 1 part of "Combined".
## -> subpopulation needed. We can now trace one specific subpopulation ('current') to "Cycle 1".

## "Cycle 1" 
## -> Estimates in "Cycle 1" come from stratum mean. Requires:
##    + strata_weights = Nh/N (phase 1 points) (aggregation of strata means)
##    + strata_means = sum_i (agb_plot_i) / sum_i (area_plot_i)
##      - agb_plot_i (yi): aggregate AGB from plot ("Plot_Summary"): 
##      - area_plot_i (ai): aggregate area from plot ("Plot_Summary")

## "Plot_Summary"
## agb_plot_i (yi): sum AGB of LCS in strata i, subpopulation (see in "Cycle 1" F1) and combined filter (not QAQC), inaccessible???
## area_plot_i (ai): sum area of  LCS in strata i, subpopulation of interest and combined filter. 

## "Subplot"
## Actually LCS level

## "Measured areas"
## used to weight AGB and area.

##
## Preparation ####
##

tmp <- list()


## + Initiate plots and LCS ####
## All plots and LCS in phase 2, theoretically to be measured

plot_all <- tibble(
  plot_no = sort(unique(anci$ceo_nfi_id_all$plotid_all))
)

lcs_all <- expand_grid(
  plot_no = sort(unique(anci$ceo_nfi_id_all$plotid_all)), 
  subplot_no = c("A", "B", "C", "D"), 
  lcs_no = 1:5
  )

## Check
nrow(lcs_all) == 1067 * 20
nrow(plot_all) + length(unique(data_prep$subplot_qc$subplot_plot_no))


## + !!! Est. tool matching adjustments ####
## 2025-07-09
## In 'Subplot' table:
## Current version of Est. tool missed plots: 106, 139, 154, 439, 534, 578, 1163
## Current version of Est. tool has duplicate of plot 553
tmp$dup_553 <- lcs_all |> filter(plot_no == 553)
if (nrow(filter(lcs_all, plot_no == 553)) == 20) {
  lcs_all <- lcs_all |> bind_rows(tmp$dup_553)
}

lcs_all <- lcs_all |> filter(!plot_no %in% c(106, 139, 154, 439, 534, 578, 1163))

## Check matching Est. tool without QAQC: 21220 
message("LCS adjusted for Est. tool: ", nrow(lcs_all) == 21220)

## In 'Plot' table: 
## Current version of Est. tool missed plots: 106, 139, 154, 439, 534, 578 but not 1163
## Current version of Est. tool has duplicate of plot 553
if (nrow(filter(plot_all, plot_no == 553)) == 1) {
  plot_all <- plot_all |> bind_rows(list(plot_no = 553))
}

plot_all <- plot_all |> filter(!plot_no %in% c(106, 139, 154, 439, 534, 578))

## Check matching Est. tool without QAQC: 1116
message("Plot adjusted for Est. tool: ", nrow(plot_all) + length(unique(data_prep$subplot_qc$subplot_plot_no)) == 1116)
## !!!


## + Plot level preparation ####
## + + Assign sub-population ####
tmp$ceo_fid <- anci$ceo_nfi_id_all |> select(plot_no = plotid_all, ceo_fid = pl_orig_fid)
tmp$ceo_prov <- anci$ceo |> select(ceo_fid = pl_orig_fid, ceo_prov_no = pl_pcode)

plot_all <- plot_all |>
  mutate(
    ceo_fid = NA,
    ceo_prov_no = NA
  ) |>
  left_join(tmp$ceo_fid, by = join_by(plot_no), suffix = c("_rm", "")) |>
  left_join(tmp$ceo_prov, by = join_by(ceo_fid), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    ceo_subpop_no = if_else(ceo_prov_no %in% 3:8, 1, 2),
    ceo_subpop_txt = if_else(ceo_prov_no %in% 3:8, "FCPF ERPA", "not FCPF ERPA")
  )

# lcs_all <- lcs_all |>
#   mutate(
#     ceo_fid = NA,
#     province_no = NA
#     ) |>
#   left_join(tmp$ceo_fid, by = join_by(plot_no), suffix = c("_rm", "")) |>
#   left_join(tmp$ceo_prov, by = join_by(ceo_fid), suffix = c("_rm", "")) |>
#   select(-ends_with("_rm")) |>
#   mutate(
#     subpopulation_no = if_else(province_no %in% 3:8, 1, 2),
#     subpopulation_txt = if_else(province_no %in% 3:8, "FCPF ERPA", "not FCPF ERPA")
#   )

## Check
table(plot_all$ceo_subpop_no, useNA = "ifany")


## + + Assign strata ####
tmp$ceo_strata <- anci$ceo |>
  select(ceo_fid = pl_orig_fid, ceo_lc_no = LC_ID, ceo_lc_code = LC, ceo_stratum_name = `LC Level1`, ceo_ftm = pl_ftm2022)

plot_all <- plot_all |>
  mutate(
    ceo_lc_no = NA, 
    ceo_lc_code = NA,
    ceo_stratum_name = NA, 
    ceo_ftm = NA
  ) |>
  left_join(tmp$ceo_strata, by = join_by(ceo_fid), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(ceo_stratum_no = case_when(
    ceo_stratum_name == "Natural Forest" ~ 1,
    ceo_stratum_name == "Forest plantation" ~ 2,
    ceo_stratum_name == "Potential Forest" ~ 3,
    ceo_stratum_name == "Non Forest" ~ 4,
    TRUE ~ NA_integer_
  ))

# lcs_all <- lcs_all |>
#   mutate(
#     ceo_plot_lc_no = NA, 
#     ceo_plot_lc_code = NA,
#     ceo_plot_stratum_name = NA, 
#     ceo_plot_ftm = NA
#   ) |>
#   left_join(tmp$ceo_strata, by = join_by(ceo_fid), suffix = c("_rm", "")) |>
#   select(-ends_with("_rm")) |>
#   mutate(ceo_plot_stratum_no = case_when(
#     ceo_plot_stratum_name == "Natural Forest" ~ 1,
#     ceo_plot_stratum_name == "Forest plantation" ~ 2,
#     ceo_plot_stratum_name == "Potential Forest" ~ 3,
#     ceo_plot_stratum_name == "Non Forest" ~ 4,
#     TRUE ~ NA_integer_
#   ))

## Check 
table(plot_all$ceo_stratum_no, useNA = "ifany")


## + LCS level ####
## + + Add plot attributes ####
lcs_all <- lcs_all |>
  mutate(
    ceo_fid = NA,
    ceo_prov_no = NA,
    ceo_subpop_no = NA,
    ceo_subpop_txt = NA,
    ceo_lc_no = NA,
    ceo_lc_code = NA,
    ceo_stratum_name = NA,
    ceo_ftm = NA
  ) |>
  left_join(distinct(plot_all), by = join_by(plot_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))

## + + Add access  ####
table(subplot$subplot_access, useNA = "ifany")

tmp$subplot_access <- subplot |> filter(subplot_access == "accessible") |> pull(subplot_id)
tmp$subplot_notvisited <- anci$ceo_nfi_id_all |> filter(!plot_visited) |> pull(plotid_all)

lcs_all <- lcs_all |>
  mutate(
    subplot_id = case_when(
      plot_no < 10 ~ paste0("000", plot_no, subplot_no),
      plot_no < 100 ~ paste0("00", plot_no, subplot_no),
      plot_no < 1000 ~ paste0("0", plot_no, subplot_no),
      TRUE ~ paste0(plot_no, subplot_no)
    ),
    subplot_access1 = if_else(subplot_id %in% tmp$subplot_access, TRUE, FALSE),
    subplot_access2 = case_when(
      plot_no %in% tmp$subplot_notvisited & ceo_stratum_no == 1 ~ FALSE,
      plot_no %in% tmp$subplot_notvisited & ceo_stratum_no == 2 ~ FALSE,
      plot_no %in% tmp$subplot_notvisited & ceo_stratum_no == 3 ~ TRUE,
      plot_no %in% tmp$subplot_notvisited & ceo_stratum_no == 4 ~ TRUE,
      TRUE ~ subplot_access1
      )
  )

## NEXT SECTION NOT NEEDED ANYMORE
## >> discrepancies between R and Est. tool mainly due shifted plot new LC 
##    and rules for non-visited 

# ## Checks 
# table(lcs_all$subplot_access1, useNA = "ifany")
# table(lcs_all$ceo_stratum_no, lcs_all$subplot_access1, useNA = "ifany")
# ## >> Big difference with est. tool :'(
# 
# ## Adding not visited plots, see subplot_access2
# table(lcs_all$subplot_access2, useNA = "ifany")
# table(lcs_all$ceo_stratum_no, useNA = "ifany")
# table(lcs_all$ceo_stratum_no, lcs_all$subplot_access2, useNA = "ifany")
# ## >> Not fully aligned but almost there 
# 
# ## Comparing stratum, access at lcs level with pivottable in the Est. Tool
# write_csv(lcs_all, "results/tests/check_lcs_access_strata.csv")
# 
# ## Solving inconsistencies, probably due to shifted plots
# ## Ex. plot 38: RV/stratum 3 in Est. tool, MDF/stratum 1 in lcs_all
# tt <- lcs_all |> filter(plot_no == 38)
# tt2 <- anci$ceo |> filter(pl_orig_fid == unique(tt$ceo_fid))
# tt3 <- anci$shifted_spA |> filter(plot_code_ == 38)
# ## >> CEO: LC = 12, ftm = 11. shifted plot table: LC_class = 12, FTM = 11, LC_CEO = 22 (RV/strata 3)
# 
# ## Cross checking at spatial location with google earth
# tt2 |>
#   mutate(x = str_remove(pl_xy, ".*,"), y = str_remove(pl_xy, ",.*")) |>
#   st_as_sf(coords = c("x", "y"), crs = 4326) |>
#   st_write("results/tests/ceo_38.kml", delete_dsn = T)
# 
# tt3 |> 
#   mutate(x = LON, y = LAT) |>
#   st_as_sf(coords = c("x", "y"), crs = 4326) |>
#   st_write("results/tests/ceo_38_shifted.kml", delete_dsn = T)
# ## >> None seem to be RV/stratum 3, need more info on CEO_Phase_I
# 
# ## Test adding shifted plot LC correction
# tmp$shifted_lc <- anci$shifted_spA |> select(plot_no = plot_code_, shifted_lc = CEO_Phase_I)
# tmp$lc_corr <- anci$lc_ceo |> 
#   select(
#     ceo_plot_lc_no_corr = lc_no, 
#     ceo_plot_lc_code_corr = lc_code, 
#     ceo_plot_stratum_no_corr = lc_strata_no, 
#     ceo_plot_stratum_name_corr = lc_strata_name
#     )
# 
# lcs_all <- lcs_all |> 
#   mutate(
#     shifted_lc = NA,
#     ceo_plot_lc_no_corr = NA, 
#     ceo_plot_lc_code_corr = NA, 
#     ceo_plot_stratum_name_corr = NA, 
#     ceo_plot_stratum_no_corr = NA
#     ) |>
#   left_join(tmp$shifted_lc, by = join_by(plot_no), suffix = c("_rm", "")) |>
#   mutate(
#     ceo_plot_lc_no_corr = if_else(!is.na(shifted_lc), shifted_lc, ceo_plot_lc_no),
#   ) |>
#   left_join(tmp$lc_corr, by = join_by(ceo_plot_lc_no_corr), suffix = c("_rm", "")) |>
#   select(-ends_with("_rm")) |>
#   mutate(
#     subplot_access3 = case_when(
#       plot_no %in% tmp$subplot_notvisited & ceo_plot_stratum_no_corr == 1 ~ FALSE,
#       plot_no %in% tmp$subplot_notvisited & ceo_plot_stratum_no_corr == 2 ~ FALSE,
#       plot_no %in% tmp$subplot_notvisited & ceo_plot_stratum_no_corr == 3 ~ TRUE,
#       plot_no %in% tmp$subplot_notvisited & ceo_plot_stratum_no_corr == 4 ~ TRUE,
#       TRUE ~ subplot_access1
#     )
#   )
# 
# ## Check again
# table(lcs_all$subplot_access3, useNA = "ifany")
# table(lcs_all$subplot_access2, lcs_all$subplot_access3, useNA = "ifany")
# 
# table(lcs_all$ceo_plot_stratum_no, useNA = "ifany")
# table(lcs_all$ceo_plot_stratum_no_corr, useNA = "ifany")
# table(lcs_all$ceo_plot_stratum_no, lcs_all$ceo_plot_stratum_no_corr, useNA = "ifany")
# ## >> Now stratum consistent with Est. tool
# 
# table(lcs_all$ceo_plot_stratum_no_corr, lcs_all$subplot_access3, useNA = "ifany")
# ## >> Much better but still not perfect
# ## >> 25 points more not accessible in Est. tool. for stratum 1
# 
# ## Investigate 
# tt <- lcs_all |> 
#   filter(ceo_plot_stratum_no_corr == 1) |>
#   mutate(subplot_access_txt = if_else(subplot_access3, "access", "no_access")) |>
#   group_by(plot_no, subplot_access_txt) |>
#   summarise(count_lcs = n(), .groups = "drop") |>
#   pivot_wider(names_from = subplot_access_txt, values_from = count_lcs, values_fill = 0)
# 
# write_csv(tt, file.path(path$res$test, "check_access_plot_stratum1.csv"))
# ## >> plots 232B, 620D, 627C, 631B, 632B fully access in R but 1 subplot not access in Est. tool
# ## >> Should be accessed, plenty of trees in tree table, noted accessed in subplot table

## Adding shifted plot corrected LC
tmp$shifted_lc <- anci$shifted_spA |> select(plot_no = plot_code_, shifted_lc = CEO_Phase_I)
tmp$lc_corr <- anci$lc_ceo |> 
  select(
    ceo_lc_no_corr = lc_no, 
    ceo_lc_code_corr = lc_code, 
    ceo_stratum_no_corr = lc_strata_no, 
    ceo_stratum_name_corr = lc_strata_name
  )

lcs_all <- lcs_all |> 
  mutate(
    shifted_lc = NA,
    ceo_lc_no_corr = NA, 
    ceo_lc_code_corr = NA, 
    ceo_stratum_name_corr = NA, 
    ceo_stratum_no_corr = NA
  ) |>
  left_join(tmp$shifted_lc, by = join_by(plot_no), suffix = c("_rm", "")) |>
  mutate(
    ceo_lc_no_corr = if_else(!is.na(shifted_lc), shifted_lc, ceo_lc_no),
  ) |>
  left_join(tmp$lc_corr, by = join_by(ceo_lc_no_corr), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    subplot_access3 = case_when(
      plot_no %in% tmp$subplot_notvisited & ceo_stratum_no_corr == 1 ~ FALSE,
      plot_no %in% tmp$subplot_notvisited & ceo_stratum_no_corr == 2 ~ FALSE,
      plot_no %in% tmp$subplot_notvisited & ceo_stratum_no_corr == 3 ~ TRUE,
      plot_no %in% tmp$subplot_notvisited & ceo_stratum_no_corr == 4 ~ TRUE,
      TRUE ~ subplot_access1
    )
  )


## + + !!! Matching Est. tool access issues ####
lcs_all <- lcs_all |>
  mutate(
    subplot_access_final = case_when(
      plot_no == 232 & subplot_no == "B" ~ FALSE,
      plot_no == 620 & subplot_no == "D" ~ FALSE,
      plot_no == 627 & subplot_no == "C" ~ FALSE,
      plot_no == 631 & subplot_no == "B" ~ FALSE,
      plot_no == 632 & subplot_no == "B" ~ FALSE,
      TRUE ~ subplot_access3
    )
  )
## !!!

## Check
table(lcs_all$ceo_stratum_no_corr, lcs_all$subplot_access_final, useNA = "ifany")
## >> Yes! full match



## 
## LCS level estimation ####
##

lcs_agb <- tree |>
  rename(plot_no = subplot_plot_no, lcs_no = tree_lcs_no) |>
  mutate(subplot_nested_level = if_else(tree_dbh < 30, "nested_small", "nested_large")) |>
  group_by(plot_no, subplot_no, lcs_no, subplot_nested_level) |>
  summarise(
    lcs_sum_agb = sum(tree_agb_final) / 1000,
    .groups = "drop"
  ) |>
  pivot_wider(names_from = subplot_nested_level, values_from = lcs_sum_agb, values_fill = 0)
lcs_agb

## !!! In Est. tool subplot AGB is actually grouped at subplot level, not LCS level
tmp$sp_agb <- lcs_agb |>
  group_by(plot_no, subplot_no) |>
  summarise(
    lcs_agb_nested_small = sum(nested_small),
    lcs_agb_nested_large = sum(nested_large), 
    .groups = "drop"
  ) |>
  mutate(lcs_no = 1)

lcs_agb <- lcs_agb |>
  mutate(
    lcs_agb_nested_small = NA,
    lcs_agb_nested_large = NA
  ) |>
  left_join(tmp$sp_agb, by = join_by(plot_no, subplot_no, lcs_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"), -nested_large, - nested_small)
# lcs_agb
## !!!

lcs_all <- lcs_all |>
  mutate(
    lcs_agb_nested_small = NA,
    lcs_agb_nested_large = NA
  ) |>
  left_join(lcs_agb, by = join_by(plot_no, subplot_no, lcs_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    lcs_agb_nested_small = replace_na(lcs_agb_nested_small, 0),
    lcs_agb_nested_large = replace_na(lcs_agb_nested_large, 0),
    lcs_area_nested_small = pi * 8^2 / 10000 / 5,
    lcs_area_nested_large = pi * 16^2 / 10000/ 5 
  )
  
## >> Cross checked few lines and matches well with Subplot table in Est. tool



##
## Plot level estimation ####
##  

## Requires measured area which combines number of plot per stratum and sub-population
length(unique(lcs_all$plot_no))

## + Get stratum and subpop to plot ####
plot_all <- plot_all |>
  lcs_all <- lcs_all |> 
  mutate(
    shifted_lc = NA,
    ceo_plot_lc_no_corr = NA, 
    ceo_plot_lc_code_corr = NA, 
    ceo_plot_stratum_name_corr = NA, 
    ceo_plot_stratum_no_corr = NA
  ) |>
  left_join(tmp$shifted_lc, by = join_by(plot_no), suffix = c("_rm", "")) |>
  mutate(
    ceo_plot_lc_no_corr = if_else(!is.na(shifted_lc), shifted_lc, ceo_plot_lc_no),
  ) |>
  left_join(tmp$lc_corr, by = join_by(ceo_plot_lc_no_corr), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    subplot_access3 = case_when(
      plot_no %in% tmp$subplot_notvisited & ceo_plot_stratum_no_corr == 1 ~ FALSE,
      plot_no %in% tmp$subplot_notvisited & ceo_plot_stratum_no_corr == 2 ~ FALSE,
      plot_no %in% tmp$subplot_notvisited & ceo_plot_stratum_no_corr == 3 ~ TRUE,
      plot_no %in% tmp$subplot_notvisited & ceo_plot_stratum_no_corr == 4 ~ TRUE,
      TRUE ~ subplot_access1
    )
  )


tt <- lcs_all |> 
  filter(subplot_access_final) |>
  select(plot_no, subpopulation_no, ceo_plot_lc_no, ceo_plot_stratum_no_corr) |>
  distinct()

table(tt$subpopulation_no, tt$ceo_plot_stratum_no_corr)

## In Est. tool, 1 plot more in subpopn 1, stratum 1 and 1 more in subpop 2, stratum 3
tt2 <- tt |> filter(plot_no == 553)
## >> first one is duplicate of 553

## isolating subpopn 2 stratum 3 
tt3 <- tt |> filter(subpopulation_no == 2, ceo_plot_stratum_no_corr == 3)
## >> 1163 is back in the data




