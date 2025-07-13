
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

## + table with all LCS theoretically measured ####
lcs_theory <- expand_grid(plot_no = sort(unique(anci$ceo_nfi_id_all$plotid_all)), subplot_no = c("A", "B", "C", "D"), lcs_no = 1:5)

## Check
nrow(lcs_theory) == 1067 * 20

## + Assign sub-population ####

tmp$ceo_fid <- anci$ceo_nfi_id_all |> select(plot_no = plotid_all, ceo_fid = pl_orig_fid)
tmp$ceo_prov <- anci$ceo |> select(ceo_fid = pl_orig_fid, province_no = pl_pcode)

lcs_theory <- lcs_theory |>
  mutate(
    ceo_fid = NA,
    province_no = NA
    ) |>
  left_join(tmp$ceo_fid, by = join_by(plot_no), suffix = c("_rm", "")) |>
  left_join(tmp$ceo_prov, by = join_by(ceo_fid), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    subpopulation_no = if_else(province_no %in% 3:8, 1, 2),
    subpopulation_txt = if_else(province_no %in% 3:8, "FCPF ERPA", "not FCPF ERPA"),
  )


## + Assign strata ####
tmp$ceo_strata <- anci$ceo |>
  select(ceo_fid = pl_orig_fid, ceo_plot_lc_no = LC_ID, ceo_plot_lc_code = LC, ceo_plot_stratum_name = `LC Level1`, ceo_plot_ftm = pl_ftm2022)

lcs_theory <- lcs_theory |>
  mutate(
    ceo_plot_lc_no = NA, 
    ceo_plot_lc_code = NA,
    ceo_plot_stratum_name = NA, 
    ceo_plot_ftm = NA
  ) |>
  left_join(tmp$ceo_strata, by = join_by(ceo_fid), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(ceo_plot_stratum_no = case_when(
    ceo_plot_stratum_name == "Natural Forest" ~ 1,
    ceo_plot_stratum_name == "Forest plantation" ~ 2,
    ceo_plot_stratum_name == "Potential Forest" ~ 3,
    ceo_plot_stratum_name == "Non Forest" ~ 4,
    TRUE ~ NA_integer_
  ))

table(lcs_theory$ceo_plot_stratum_no)

## + Add access ####
table(subplot$subplot_access, useNA = "ifany")

tmp$subplot_access <- subplot |> filter(subplot_access == "accessible") |> pull(subplot_id)
tmp$subplot_notvisited <- anci$ceo_nfi_id_all |> filter(plot_visited == F) |> pull(plotid_all)
tmp$plot_access <- lcs_theory |> filter(ceo_plot_stratum_no %in% 1:3, !plot_no %in% tmp$subplot_notvisited)


lcs_theory <- lcs_theory |>
  mutate(
    subplot_id = case_when(
      plot_no < 10 ~ paste0("000", plot_no, subplot_no),
      plot_no < 100 ~ paste0("00", plot_no, subplot_no),
      plot_no < 1000 ~ paste0("0", plot_no, subplot_no),
      TRUE ~ paste0(plot_no, subplot_no)
    ),
    subplot_access = case_when(
      subplot_id %in% tmp$subplot_access ~ TRUE,
      plot_no %in% tmp$subplot_notvisited & ceo_plot_stratum_no == 4 ~ TRUE,
      TRUE ~ FALSE
      )
  )

## Check
table(lcs_theory$subplot_access, useNA = "ifany")
tmp$check <- lcs_theory |> filter(subplot_access)
table(tmp$check$ceo_plot_stratum_no, useNA = "ifany")

## >> Big difference with est. tool :'(


## >> now each subplot has subpopn, strata, access. QAQC plots were removed in the preparation phase.


## 
## LCS estimation ####
##


