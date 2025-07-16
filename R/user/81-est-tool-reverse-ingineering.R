
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

## Check 
table(plot_all$ceo_stratum_no, useNA = "ifany")


## + + Add shifted plot correction ####
tmp$shifted_lc <- anci$shifted_spA |> select(plot_no = plot_code_, shifted_lc = CEO_Phase_I)
tmp$lc_corr <- anci$lc_ceo |> 
  select(
    ceo_lc_no_corr = lc_no, 
    ceo_lc_code_corr = lc_code, 
    ceo_stratum_no_corr = lc_strata_no, 
    ceo_stratum_name_corr = lc_strata_name
  )

plot_all <- plot_all |>
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
  select(-ends_with("_rm")) 


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

## + + shifted plot correction ####
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
  select(-ends_with("_rm"))


## + + correct access based on corrected stratum ####
lcs_all <- lcs_all |>
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
## LCS level AGB / AREA estimation ####
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

## !!! In Est. tool subplot AGB is actually grouped at subplot level, not LCS level
tmp$sp_agb <- lcs_agb |>
  group_by(plot_no, subplot_no) |>
  summarise(
    lcs_agb_nested_small = sum(nested_small),
    lcs_agb_nested_large = sum(nested_large), 
    .groups = "drop"
  ) |>
  mutate(lcs_no = 1)

# lcs_agb <- lcs_all |>
#   mutate(
#     lcs_agb_nested_small = NA,
#     lcs_agb_nested_large = NA
#   ) |>
#   left_join(tmp$sp_agb, by = join_by(plot_no, subplot_no, lcs_no), suffix = c("_rm", "")) |>
#   select(-ends_with("_rm"))
# lcs_agb
## !!!

lcs_all <- lcs_all |>
  mutate(
    lcs_agb_nested_small = NA,
    lcs_agb_nested_large = NA
  ) |>
  left_join(tmp$sp_agb, by = join_by(plot_no, subplot_no, lcs_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    lcs_agb_nested_small = replace_na(lcs_agb_nested_small, 0),
    lcs_agb_nested_large = replace_na(lcs_agb_nested_large, 0),
    lcs_area_nested_small = pi * 8^2 / 10000 / 5,
    lcs_area_nested_large = pi * 16^2 / 10000/ 5
  ) |>
  rowwise() |>
  mutate(lcs_area_largest = max(lcs_area_nested_small, lcs_area_nested_large)) |>
  ungroup()
  
## >> Cross checked few lines and matches well with Subplot table in Est. tool

## Check again
table(lcs_all$ceo_stratum_no_corr, lcs_all$subplot_access_final, useNA = "ifany")
table(lcs_all$ceo_stratum_no_corr, lcs_all$ceo_subpop_no, useNA = "ifany")

tmp$lcs_access <- lcs_all |> filter(subplot_access_final)
table(tmp$lcs_access$ceo_stratum_no_corr, tmp$lcs_access$subplot_access_final, useNA = "ifany")
table(tmp$lcs_access$ceo_stratum_no_corr, tmp$lcs_access$ceo_subpop_no, useNA = "ifany")



##
## Measured Area ####
##

## + + Count nb of plots that are not accessible: Est. tool 187
tmp$plot_access_partial <- lcs_all |>
  filter(subplot_access_final) |>
  pull(plot_no) |>
  unique() |>
  sort()

plot_access <- plot_all |> filter(plot_no %in% c(tmp$plot_access_partial, 1163))

table(plot_access$ceo_subpop_no, useNA = "ifany")
table(plot_access$ceo_stratum_no_corr, useNA = "ifany")
table(plot_access$ceo_subpop_no, plot_access$ceo_stratum_no_corr, useNA = "ifany")
## >> Full match, yes!


sample_size <- plot_access |>
  summarise(count_plot = n(), .by = c(ceo_subpop_no, ceo_stratum_no_corr)) |>
  arrange(ceo_subpop_no, ceo_stratum_no_corr)
sample_size
## >> Full match too!

tmp$sum_area <- lcs_all |>
  filter(subplot_access_final) |>
  summarise(
    count_lcs = n(),
    sum_area_nested_small = sum(lcs_area_nested_small), 
    sum_area_nested_large = sum(lcs_area_nested_large),
    .by = c(ceo_subpop_no, ceo_stratum_no_corr)
  ) |>
  arrange(ceo_subpop_no, ceo_stratum_no_corr)
tmp$sum_area

measured_area <- sample_size |>
  mutate(
    count_lcs = NA,
    sum_area_nested_small = NA,
    sum_area_nested_large = NA
  ) |>
  left_join(tmp$sum_area, by = join_by(ceo_subpop_no, ceo_stratum_no_corr), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  rowwise() |>
  mutate(
    mean_area_nested_small = sum_area_nested_small / count_plot,
    mean_area_nested_large = sum_area_nested_large / count_plot,
    mean_area_nested_largest = max(mean_area_nested_small, mean_area_nested_large),
  ) |>
  ungroup()


conv <- list()
conv$ratio_small_to_large <- pi * 16^2 / (pi * 8^2)


##
## Plot level estimation ####
##  


## + Plot AGB ####
plot_agb <- lcs_all |>
  filter(subplot_access_final) |>
  group_by(plot_no, ceo_subpop_no, ceo_stratum_no_corr) |>
  summarise(
    plot_count_lcs = n(),
    plot_agb_nested_small = sum(lcs_agb_nested_small),
    plot_agb_nested_large = sum(lcs_agb_nested_large),
    plot_area_largest     = sum(lcs_area_largest),
    .groups = "drop"
  ) 

plot_access <- plot_access |>
  mutate(
    plot_count_lcs = NA, 
    plot_agb_nested_small = NA,
    plot_agb_nested_large = NA,
    plot_area_largest = NA,
  ) |>
  left_join(plot_agb, by = join_by(plot_no, ceo_subpop_no, ceo_stratum_no_corr), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(across(starts_with("plot_"), \(x) replace_na(x, 0))) |>
  mutate(
    plot_yid = plot_agb_nested_small * conv$ratio_small_to_large + plot_agb_nested_large,
    plot_yid_sq = plot_yid^2,
    plot_ai = plot_area_largest,
    plot_ai_sq = plot_ai^2,
    plot_yid_times_ai = plot_yid * plot_ai,
    plot_xid =  plot_area_largest,
    plot_xid_sq = plot_xid^2,
    plot_xid_times_ai = plot_xid * plot_ai,
    plot_yid_times_xid = plot_yid * plot_xid
  ) |>
  filter(plot_no != 1163)
  

## 
## AGGREGATES ####
##

## + Make tables of strata weights ####
tmp$ceo <- anci$ceo |> 
  left_join(anci$lc_ceo, by = join_by(LC_ID == lc_no)) |>
  mutate(ceo_subpop_no = if_else(pl_pcode %in% 3:8, 1, 2))

table(tmp$ceo$ceo_subpop_no, tmp$ceo$lc_strata_no)

tmp$strata_weights <- tmp$ceo |>
  rename(ceo_stratum_no = lc_strata_no) |>
  group_by(ceo_subpop_no, ceo_stratum_no) |>
  summarise(count_ceo = n(), .groups = "drop")

tmp$subpop_ceo_totals <- tmp$strata_weights |>
  summarise(subpop_count_ceo = sum(count_ceo), .by = ceo_subpop_no)

tmp$subpop_plot_totals <- sample_size |>
  summarise(subpop_count_plot = sum(count_plot), .by = ceo_subpop_no)
  
## >> Mismatches in biomass at tree level, reverted plantation AGB model to chave 2005
## >> Still few mismatches, seems to affect mosty RV and B at the LC level in the tree table
## >> No clear pattern though.


## + Stats from plot level ####
subpop_stratum <- plot_access |>
  filter(plot_no %in% tmp$plot_access_partial) |>
  group_by(ceo_subpop_no, ceo_stratum_no_corr) |>
  summarise(
    count_plot = n(),
    sum_yi    = sum(plot_yid),
    sum_yi_sq = sum(plot_yid^2),
    sum_yiai  = sum(plot_yid * plot_ai),
    sum_ai    = sum(plot_ai),
    sum_ai_sq = sum(plot_ai^2),
    sum_xi    = sum(plot_xid),
    sum_xi_sq = sum(plot_xid^2),
    sum_xiai  = sum(plot_xid * plot_ai),
    sum_yixi  = sum(plot_yid * plot_xid),
    .groups = "drop"
    ) |>
  mutate(
    mean_yi = sum_yi / sum_ai,
    mean_xi = sum_xi / sum_ai
  )

## + Add strata weights ####
subpop_stratum <- subpop_stratum |>
  mutate(
    count_plot = NA,
    subpop_count_plot = NA,
    count_ceo = NA, 
    subpop_count_ceo = NA
    ) |>
  left_join(sample_size, by = join_by(ceo_subpop_no, ceo_stratum_no_corr), suffix = c("_rm", "")) |>
  left_join(tmp$subpop_plot_totals, by = join_by(ceo_subpop_no), suffix = c("_rm", "")) |>
  left_join(tmp$strata_weights, by = join_by(ceo_subpop_no, ceo_stratum_no_corr == ceo_stratum_no), suffix = c("_rm", "")) |>
  left_join(tmp$subpop_ceo_totals, by = join_by(ceo_subpop_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    stratum_Wh = count_ceo / subpop_count_ceo
    )


## + Add subpopulation totals ####
subpop <- subpop_stratum |>
  group_by(ceo_subpop_no) |>
  summarise(
    subpop_mean_y = sum(mean_yi * stratum_Wh),
    subpop_mean_x = sum(mean_xi * stratum_Wh)
  )

subpop_stratum <- subpop_stratum |>
  mutate(
    subpop_mean_y = NA,
    subpop_mean_x = NA
    ) |>
  left_join(subpop, by = join_by(ceo_subpop_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))

## + Calc variance ####
subpop_stratum <- subpop_stratum |>
  mutate(
    var_yi = count_plot^2 / (count_plot - 1) * (sum_yi_sq - 2 * mean_yi * sum_yiai + mean_yi^2 * sum_ai_sq) / (sum_ai^2),
    var_mean_yi = stratum_Wh * (count_ceo - 1) / (subpop_count_ceo - 1) * var_yi / count_plot,
    var_xi = count_plot^2 / (count_plot - 1) * (sum_xi_sq - 2 * mean_xi * sum_xiai + mean_xi^2 * sum_ai_sq) / (sum_ai^2),
    var_mean_xi = stratum_Wh * (count_ceo - 1) / (subpop_count_ceo - 1) * var_xi / count_plot,
  )







