

## CHECKS

## Trying to reproduce Chip Scott estimation tool for Lao PDR

## The tool isolate land cover types and calculate results for each type
## The population is split between carbon fund area (subpopn 1) and the rest of the country (subpopn 2)
## The subplot table is in fact the land cover section level (5 LCS per subplot, 4 subplots per plot).

## target LU for demo: EG

##
## CHECKS EST. TOOL NO ROWS ####
##

## nrow in estimation tool: 
## tree: 18136 
## subplot: 22300
## plot_summary: 1115


## + CROSS CHECK TREE TABLE ####


## The data includes the full NFI phase 2 minus some of the plots and the QAQC duplicates
nrow(data_prep$tree_qc) + nrow(data_prep$tree)
## >> not same

## Check LCS 1A1
tt <- tree |> filter(subplot_plot_no == 1, subplot_no == "A", tree_lcs_no == 1)
## >> same

## Check subplot 1A, 18 rows in Est. tool
tt <- tree |> filter(subplot_plot_no == 1, subplot_no == "A")
## >> NOT same, seems dead trees are mixed in the data

tt2 <- dw |> filter(subplot_plot_no == 1, subplot_no == "A")
nrow(tt) + nrow(tt2)
## >> NOT same, still missing 2 rows with DBH = 0 in Est. tool

tt3 <- stump |> filter(subplot_plot_no == 1, subplot_no == "A")
nrow(tt) + nrow(tt2) + nrow(tt3)
## >> same 

## Add all inputs from nested levels 8 and 16 m
N_EST_TREE <- sum(
  nrow(data_prep$tree), nrow(data_prep$tree_qc), 
  nrow(data_prep$dw), nrow(data_prep$dw_qc), 
  nrow(data_prep$stump), nrow(data_prep$stump_qc)
) 
N_EST_TREE == 18136
## >> Works!


## + CROSS CHECK SUBPLOT TABLE ####


## Effectively, the subplot table in Est. tool is at LCS level (5 points per subplot)
nrow(lcs) 
## >> not same. lcs only record accessible field observations

## In theory: nb lcs = 1067 * 4 * 5
1067 * 4 * 5
1067 * 4 * 5 == nrow(anci$ceo_nfi_id) * 5

## ADD QAQC plots 
nrow(anci$ceo_nfi_id) * 5 + nrow(data_prep$subplot_qc) * 5
## >> 100 LCS too many compared to Est. tool

## Identify missing subplots in Est. tool subplot data
max(anci$ceo_nfi_id_all$plotid_all)

## Check plot 106, not in Est. tool
tt <- subplot |> filter(subplot_plot_no == 106)
tt <- tree |> filter(subplot_plot_no == 106)
tt <- anci$ceo_nfi_id |> filter(plotid == 106)
tt <- anci$ceo_nfi_id_all |> filter(plotid_all == 106)
tt <- anci$ceo |> filter(pl_orig_fid == 20357)

## >> Looks like 5 plots that should have been visited were not visited and no data was recorded, they are not in the Est. tool.

## 106, 139, 154, 439, 534, 578, 1163 (Done in Excel, pivot table of subplot vs anci$ceo_nfi_id)
## >> actually 7, there is still mismatch

## List QAQC plots
tt <- data_prep$subplot_qc |> pull(subplot_plot_no) |> unique() |> sort()
length(tt)
tt

nrow(data_prep$subplot_qc) / 4 

## incomplete QC plots ?
tt2 <- data_prep$subplot_qc |>
  summarise(count_sp = n(), .by = subplot_plot_no) |>
  filter(count_sp != 4)
## >> yes, need to use QAQC plot ID * 20 , not actually visited QAQC subplots

## In Est. tool plot 553 is duplicated (both go to not QAQC)

## full match with: 
## 20 LCS for all PH2 plots + 20 LCS for all QAQC plot - 7 * 20 LCS for 7 plots not in Est. tool + 20 LCS for plot 552 duplicated in the Est. tool
N_EST_SUBPLOT <- nrow(anci$ceo_nfi_id_all) * 20 + length(tt) * 20 - 7 * 20 + 20
N_EST_SUBPLOT == 22300
## >> works!


## + CROSS CHECK PLOT_SUMMARY ####

nrow(anci$ceo_nfi_id_all)
length(unique(anci$ceo_nfi_id_all$plotid_all))

nrow(anci$ceo_nfi_id_all) + length(tt)
## Too many: 1067 + 54 = 1121
## missing 7 plots with no records carried to plot level, 553 still duplicate

N_PLOT <- nrow(anci$ceo_nfi_id_all) + length(tt) - 7 + 1
N_PLOT == 1115


##
## CHECK TREE LEVEL AGB ####
##

## + Cross-check AGB of LCS 1A1 ####
tt <- tree |> 
  filter(subplot_plot_no == 1, subplot_no == "A", tree_lcs_no == 1) |>
  select(tree_distance, tree_azimuth, tree_dbh, tree_species_code, lu_code_new, tree_agb_final)
tt

## >> same, but est. tool has stump and deadwood in the tree table.
## >> Careful Subpoint not correct, rely on subplot and point separately

## + Check whole Subplot 1A ####
tt <- tree |> 
  filter(subplot_plot_no == 1, subplot_no == "A") |>
  select(tree_lcs_no, tree_distance, tree_azimuth, tree_dbh, tree_species_code, lu_code_new, tree_agb_final) |>
  arrange(tree_lcs_no)
tt
## >> same as above

## + Check deadwood AGB is always 0 ####
tt2 <- dw |> filter(subplot_plot_no == 1, subplot_no == "A")
## >> OK, dead wood decay 1, has DBH but AGB = 0




