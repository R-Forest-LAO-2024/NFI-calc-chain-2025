
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

## Assign land use ####
## + Field observations ####
tmp$lcs_lu <- lcs_ |> select(plot_no = lcs_plot_no, subplot_no = lcs_subplot_no, lcs_no, lu_no = lcs_lu_class_no, lu_txt = lu_code_new)

table(tmp$lcs_lu$lu_txt, useNA = "ifany")

## + FTM value for not accessed / not visited ####
tmp$ftm_lu <- anci$ceo_nfi_id |>
  left_join(anci$ceo_nfi_id_all, b = join_by(ORIG_FID == pl_orig_fid, ID)) |>
  filter(!plot_visited) |>
  left_join(anci$lc_ceo, by = join_by(FTM2022 == lc_no)) |>
  select(plot_no = plotid_all, subplot_no = Plot, ftm_lu_no = FTM2022, ftm_lu_txt = lc_code) |>
  mutate(
    ftm_lu_no = case_when(
      ftm_lu_no == 16 ~ 169,
      TRUE ~ ftm_lu_no
    ),
    ftm_lu_txt = case_when(
      ftm_lu_txt == "MDF" ~ "MD",
      ftm_lu_txt == "P" ~ "P_OTH",
      ftm_lu_txt == "MDF" ~ "MD",
      TRUE ~ ftm_lu_txt
    )
  )
  
## + combine all ####
lcs_all <- lcs_all |> 
  mutate(lu_no = NA, lu_txt = NA, ftm_lu_no = NA, ftm_lu_txt = NA) |>
  left_join(tmp$lcs_lu, by = join_by(plot_no, subplot_no, lcs_no), suffix = c("_rm", "")) |>
  left_join(tmp$ftm_lu, by = join_by(plot_no, subplot_no), suffix = c("_rm", "")) |>
  mutate(
    lu_no_corr = if_else(!is.na(lu_no), lu_no, ftm_lu_no),
    lu_txt_corr = if_else(!is.na(lu_txt), lu_txt, ftm_lu_txt)
  ) |>
  select(-ends_with("_rm"))

table(lcs_all$lu_txt_corr, useNA = "ifany")


lcs_access <- lcs_all |> filter(subplot_access_final)

table(lcs_access$lu_txt_corr, useNA = "ifany")



##
## Plot level estimation ####
##  

## !!! INSERT LAND CLASS FILTERING ####

lcs_lu <- lcs_all |> filter(lu_txt_corr == "EG")
#lcs_lu <- lcs_all

## !!!


## + Plot AGB ####
plot_area <- lcs_all |>
  filter(subplot_access_final) |>
  group_by(plot_no, ceo_subpop_no, ceo_stratum_no_corr) |>
  summarise(plot_area_largest_all = sum(lcs_area_largest), .groups = "drop")

plot_agb <- lcs_lu |>
  filter(subplot_access_final) |>
  group_by(plot_no, ceo_subpop_no, ceo_stratum_no_corr) |>
  summarise(
    plot_count_lcs = n(),
    plot_agb_nested_small = sum(lcs_agb_nested_small),
    plot_agb_nested_large = sum(lcs_agb_nested_large),
    plot_area_largest_lu = sum(lcs_area_largest),
    .groups = "drop"
  )

plot_access <- plot_access |>
  mutate(
    plot_area_largest_all = NA,
    plot_count_lcs = NA, 
    plot_agb_nested_small = NA,
    plot_agb_nested_large = NA,
    plot_area_largest_lu = NA,
  ) |>
  left_join(plot_area, by = join_by(plot_no, ceo_subpop_no, ceo_stratum_no_corr), suffix = c("_rm", "")) |>
  left_join(plot_agb, by = join_by(plot_no, ceo_subpop_no, ceo_stratum_no_corr), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(across(starts_with("plot_"), \(x) replace_na(x, 0))) |>
  mutate(
    plot_yid = plot_agb_nested_small * conv$ratio_small_to_large + plot_agb_nested_large,
    plot_yid_sq = plot_yid^2,
    plot_ai = plot_area_largest_all,
    plot_ai_sq = plot_ai^2,
    plot_yid_times_ai = plot_yid * plot_ai,
    plot_xid =  plot_area_largest_lu,
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

tmp$subpop_count_ceo <- tmp$strata_weights |>
  summarise(subpop_count_ceo = sum(count_ceo), .by = ceo_subpop_no)

tmp$subpop_count_plot <- sample_size |>
  summarise(subpop_count_plot = sum(count_plot), .by = ceo_subpop_no)
  
## >> Mismatches in biomass at tree level, reverted plantation AGB model to chave 2005
## >> Still few mismatches, seems to affect mosty RV and B at the LC level in the tree table
## >> No clear pattern though.

## + Sub-population x strata ####

## + + Stats from plot level ####
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
    mean_yi = if_else(sum_ai > 0, sum_yi / sum_ai, 0),
    mean_xi = if_else(sum_ai > 0, sum_xi / sum_ai, 0)
  )

## + + Add strata weights ####
subpop_stratum <- subpop_stratum |>
  mutate(
    count_plot = NA,
    subpop_count_plot = NA,
    count_ceo = NA, 
    subpop_count_ceo = NA
    ) |>
  left_join(sample_size, by = join_by(ceo_subpop_no, ceo_stratum_no_corr), suffix = c("_rm", "")) |>
  left_join(tmp$subpop_count_plot, by = join_by(ceo_subpop_no), suffix = c("_rm", "")) |>
  left_join(tmp$strata_weights, by = join_by(ceo_subpop_no, ceo_stratum_no_corr == ceo_stratum_no), suffix = c("_rm", "")) |>
  left_join(tmp$subpop_count_ceo, by = join_by(ceo_subpop_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    stratum_Wh = count_ceo / subpop_count_ceo
    )


## + + Add sub-population mean ####
tmp$subpop_mean <- subpop_stratum |>
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
  left_join(tmp$subpop_mean, by = join_by(ceo_subpop_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))

## + + Calc variance ####
subpop_stratum <- subpop_stratum |>
  mutate(
    var_yi      = if_else(count_plot > 1 & sum_ai > 0, count_plot^2 / (count_plot - 1) * (sum_yi_sq - 2*mean_yi*sum_yiai + mean_yi^2*sum_ai_sq) / (sum_ai^2), 0),
    var_xi      = if_else(count_plot > 1 & sum_ai > 0, count_plot^2 / (count_plot - 1) * (sum_xi_sq - 2*mean_xi*sum_xiai + mean_xi^2*sum_ai_sq) / (sum_ai^2), 0),
    covar_xiyi  = if_else(count_plot > 1 & sum_ai > 0, count_plot^2 / (count_plot - 1) * (sum_yixi - mean_yi*sum_xiai - mean_xi*sum_yiai + mean_yi*mean_xi*sum_ai_sq) / (sum_ai^2), 0),
    var_mean_yi = if_else(subpop_count_ceo > 1 & count_plot > 0, stratum_Wh * (count_ceo - 1) / (subpop_count_ceo - 1) * var_yi / count_plot, 0),
    var_mean_xi = if_else(subpop_count_ceo > 1 & count_plot > 0, stratum_Wh * (count_ceo - 1) / (subpop_count_ceo - 1) * var_xi / count_plot, 0),
    covar_mean  = if_else(subpop_count_ceo > 1 & count_plot > 0, stratum_Wh * (count_ceo - 1) / (subpop_count_ceo - 1) * covar_xiyi / count_plot, 0)
  )

print(subpop_stratum)

## + Sub-population level ####

tmp$N_strata <- length(unique(subpop_stratum$ceo_stratum_no_corr))

## + + Calc variance ####
tmp$subpop_var <- subpop_stratum |>
  group_by(ceo_subpop_no) |>
  summarise(
    subpop_var_y = sum(var_mean_yi),
    subpop_var_x = sum(var_mean_xi),
    subpop_covar = sum(covar_mean),
    .groups = "drop"
  )

## + + calc strata areas (ha) ####
tmp$country_area <- 23680000
tmp$ceo_total <- nrow(anci$ceo)


## + + combine sub-population aggregates ####
subpop <- tibble(ceo_subpop_no = sort(unique(subpop_stratum$ceo_subpop_no))) |>
  mutate(
    subpop_count_ceo = NA,
    subpop_count_plot = NA
  ) |>
  left_join(tmp$subpop_count_ceo, by = join_by(ceo_subpop_no), suffix = c("_rm", "")) |>
  left_join(tmp$subpop_count_plot, by = join_by(ceo_subpop_no), suffix = c("_rm", "")) |>
  mutate(
    subpop_weight = subpop_count_ceo / tmp$ceo_total,
    subpop_area   = subpop_weight * tmp$country_area,
    subpop_mean_y = NA,
    subpop_mean_x = NA
  ) |>
  left_join(tmp$subpop_mean, by = join_by(ceo_subpop_no), suffix = c("_rm", "")) |>
  mutate(
    subpop_var_y = NA,
    subpop_var_x = NA,
    subpop_covar = NA,
  ) |>
  left_join(tmp$subpop_var, by = join_by(ceo_subpop_no ), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    subpop_R        = if_else(subpop_mean_x > 0, subpop_mean_y / subpop_mean_x, 0),
    subpop_R_var    = if_else(subpop_mean_x > 0, (subpop_var_y + subpop_R^2*subpop_var_x - 2*subpop_R*subpop_covar) / (subpop_mean_x^2), 0),
    subpop_R_se     = qt(1-0.1/2, df = subpop_count_plot - tmp$N_strata) * sqrt(subpop_R_var),
    subpop_R_seperc = if_else(subpop_R > 0, round(subpop_R_se / subpop_R * 100, 1), 0)
    )
subpop

totals <- subpop |>
  summarise(
    Y     = sum(subpop_mean_y * subpop_weight),
    Y_var = sum(subpop_var_y * subpop_weight * subpop_weight),
    X     = sum(subpop_mean_x * subpop_weight),
    X_var = sum(subpop_var_x * subpop_weight * subpop_weight),
    covar = sum(subpop_covar * subpop_weight * subpop_weight),
    Y_tot = sum(subpop_mean_y * subpop_area),
    X_tot = sum(subpop_mean_x * subpop_area),
    A_tot = sum(subpop_area)
  ) |>
    mutate(
    R     = Y / X,
    R_var = (Y_var + R^2*X_var - 2*R*covar)/X^2,
    ## Margin of Error = half width of confidence interval
    R_me  = round(qt(1-0.1/2, df = Inf) * sqrt(R_var) / R * 100, 1)
  )
totals 

## !!! Works well !!! 


