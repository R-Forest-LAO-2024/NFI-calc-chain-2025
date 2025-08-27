

## This script generates a guiding example to help understand the statistics behind the aggregation to domain totals

## Plot 1 is a good starting point since it's split between DD and RV

## 
## Setup ####
##

tmp <- list()

guidex <- list()

## + Select 5 plots for ph2 ####
## 1 (DD, RV), 50 (DD), 126 (DD), 128 (DD), 157 (RV)
vec_ph2 <- c(1, 50, 126, 128, 157)

guidex$ph2_subplot <- ph2_subplot |> 
  filter(plot_id %in% vec_ph2) |>
  select(plot_id, subplot_id, subpop, stratum, lc_no, access, sp_area, agb)


## + Select 20 plots for ph1 ####
## RV area ~ 4.5 Mha, DD area ~ 1.5 Mha, ratio 1/4 DD 3/4 RV
vec_ph1 <- c(
  280, 445, 602, 767, 925, 1080, 1087, 1230, 1403, 1550, 1572, 1710, 1730, 1731, ## 14 ph1 in RV + 1 ph2 in RV see vec_ph2
  2959 ## 1 ph1 in DD + 4 ph2 in DD see vec_ph2
  )

guidex$ph1_data <- ph1_data |> filter(ph1_plot_no %in% vec_ph1 | plot_id %in% vec_ph2)

table(guidex$ph2_subplot$lc_no)
table(guidex$ph1_data$ph1_lc_no)

##
## Make initial data ####
##

guidex$tree_init <- tree |> 
  filter(subplot_plot_no %in% vec_ph2) |>
  mutate(subplot_id = paste0(subplot_no, tree_lcs_no)) |>
  select(plot_id = subplot_plot_no, subplot_id, lc_no = lcs_lu_class_no, tree_no, tree_stem_no, tree_distance, tree_azimuth, tree_dbh)

#table(guidex$tree$plot_id)

guidex$ph2_init <- guidex$ph2_subplot |> select(plot_id, subplot_id, lc_no, access)


##
## Aggregation ####
##

## Plot summary ####
tmp$plota <- guidex$ph2_subplot |>
  group_by(subpop, stratum) |>
  summarise(
    ai = sum(sp_area),
    .groups = "drop"
  )

guidex$plotsum <- guidex$ph2_subplot |>
  group_by(lc_no, subpop, stratum) |>
  summarise(
    yid = sum(agb),
    xid = sum(sp_area),
    .groups = "drop"
  ) |>
  mutate(ai = NA) |>
  left_join(tmp$plota, by = join_by(subpop, stratum), suffix = c("_rm", "")) |>
  select(-ends_with("_rm"))

