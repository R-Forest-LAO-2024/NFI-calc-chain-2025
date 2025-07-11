
## FOR TREE / DW / STUMP: ratio estimator at the LCS level into PLOT level
## FOR LDW: ratio estimator at the subplot level into PLOT level

## list of accessible subplot:
subplot_access <-  subplot |>
  filter(subplot_access == "accessible") |>
  mutate(suplot_id = paste0(subplot_plot_id, subplot_no)) |>
  pull(subplot_id) |>
  sort()

lcs_access <- lcs |>
  rename(subplot_plot_no = lcs_plot_no, subplot_no = lcs_subplot_no) |>
  mutate(
    subplot_plot_id = case_when(
      subplot_plot_no < 10 ~ paste0("000", subplot_plot_no),
      subplot_plot_no < 100 ~ paste0("00", subplot_plot_no),
      subplot_plot_no < 1000 ~ paste0("0", subplot_plot_no),
      TRUE ~ as.character(subplot_plot_no)
    ),
    subplot_id = paste0(subplot_plot_id, subplot_no)
  ) |>
  filter(subplot_id %in% subplot_access) |>
  mutate(
    lu_no = as.numeric(case_when(
      lcs_lu_class_txt == "16AC" ~ "161",
      lcs_lu_class_txt == "16EC" ~ "162",
      lcs_lu_class_txt == "16PN" ~ "163",
      lcs_lu_class_txt == "16RB" ~ "164",
      lcs_lu_class_txt == "16TK" ~ "165",
      lcs_lu_class_txt == "16OTH" ~ "169",
      TRUE ~ lcs_lu_class_txt
    ))
  ) |>
  left_join(anci$lc, by = join_by(lu_no)) 

##
## TREE ####
##

## + sums at lcs level

tree_aggregate <- tree |>
  summarise(
    lcs_nb_tree_ha = sum(tree_weight), 
    lcs_ba_ha      = sum(tree_weight * tree_ba),
    lcs_agb_ha     = sum(tree_weight * tree_agb_final / 1000), 
    lcs_carbon_ha  = sum(tree_weight * tree_carbon / 1000),
    .by = c(subplot_plot_no, subplot_no, tree_lcs_no)
  ) |>
  arrange(subplot_plot_no, subplot_no, tree_lcs_no)


lcs_tree <- lcs_access |>
  left_join(tree_aggregate, by = join_by(subplot_plot_no, subplot_no, lcs_no == tree_lcs_no)) |>
  mutate(
    lcs_area_ha    = if_else(lcs_no == 1, 12^2 / 10000, round((pi*16^2-12^2)/4, 0)/ 10000),
    lcs_sum_tree   = if_else(is.na(lcs_nb_tree_ha), 0, round(lcs_nb_tree_ha * lcs_area_ha, 3)),
    lcs_sum_ba     = if_else(is.na(lcs_ba_ha), 0, round(lcs_ba_ha * lcs_area_ha, 3)),
    lcs_sum_agb    = if_else(is.na(lcs_agb_ha), 0, round(lcs_agb_ha * lcs_area_ha, 3)),
    lcs_sum_carbon = if_else(is.na(lcs_carbon_ha), 0, round(lcs_carbon_ha * lcs_area_ha, 3))
  )


plot_tree <- lcs_tree |>
  summarise(
    count_lcs = n(),
    plot_area_ha = sum(lcs_area_ha),
    plot_sum_tree = sum(lcs_sum_tree),
    plot_sum_ba = sum(lcs_sum_ba),
    plot_sum_agb = sum(lcs_sum_agb),
    plot_sum_carbon = sum(lcs_sum_carbon),
    .by = c(subplot_plot_no, lu_no, lu_code_new, lu_strata_no, lu_strata_name)
  )

landuse_tree <- plot_tree |>
  summarise(
    count_plot = n(),
    sum_area_ha = sum(plot_area_ha),
    sum_tree = sum(plot_sum_tree),
    sum_ba = sum(plot_sum_ba),
    sum_agb = sum(plot_sum_agb),
    sum_carbon = sum(plot_sum_carbon),
    .by = c(lu_no, lu_code_new)
  ) |>
  mutate(
    nb_tree_ha = sum_tree / sum_area_ha,
    ba_ha = sum_ba / sum_area_ha,
    agb_ha = sum_agb / sum_area_ha,
    carbon_ha = sum_carbon / sum_area_ha,
  ) |>
  select(-starts_with("sum_")) |>
  arrange(lu_no)
landuse_tree

write_csv(landuse_tree, paste0("results/tables/cstock-landuse-tree", Sys.Date(), ".csv"))


##
## standing deadwood ####
##

dw_aggregate <- dw |>
  rename(lu_no = lcs_lu_class_no) |>
  mutate(
    dw_lcs_area = case_when(
      lcs_name == "center" ~ 12^2, ## Square of 12x12 m for all trees
      lcs_name != "center" & dw_dbh <= 30 ~ round((pi*8^2 - 12^2)/4), ## Quarter of 8 m radius circle minus the 12x12 m square 
      lcs_name != "center" & dw_dbh > 30 ~ round((pi*16^2 - 12^2)/4), ## Quarter of 16 m radius circle minus the 12x12 m square
      TRUE ~ NA_real_
    ),
    dw_lcs_weight = round(10000/dw_lcs_area, 3)
  ) |>
  summarise(
    lcs_nb_dw_ha = sum(dw_lcs_weight), 
    lcs_agb_ha   = sum(dw_lcs_weight * dw_agb / 1000), 
    .by = c(subplot_plot_no, subplot_no, dw_lcs_no)
  ) |>
  arrange(subplot_plot_no, subplot_no, dw_lcs_no)

lcs_dw <- lcs_access |>
  left_join(dw_aggregate, by = join_by(subplot_plot_no, subplot_no, lcs_no == dw_lcs_no)) |>
  mutate(
    lcs_area_ha = if_else(lcs_no == 1, 12^2 / 10000, round((pi*16^2-12^2)/4, 0)/ 10000),
    lcs_sum_dw  = if_else(is.na(lcs_nb_dw_ha), 0, round(lcs_nb_dw_ha * lcs_area_ha, 3)),
    lcs_sum_agb = if_else(is.na(lcs_agb_ha), 0, round(lcs_agb_ha * lcs_area_ha, 3))
  )
  
plot_dw <- lcs_dw |>
  summarise(
    count_lcs = n(),
    plot_area_ha = sum(lcs_area_ha),
    plot_sum_dw = sum(lcs_sum_dw),
    plot_sum_agb = sum(lcs_sum_agb),
    .by = c(subplot_plot_no, lu_no, lu_code_new, lu_strata_no, lu_strata_name)
  )

landuse_dw <- plot_dw |>
  summarise(
    count_plot = n(),
    sum_area_ha = sum(plot_area_ha),
    sum_dw = sum(plot_sum_dw),
    sum_agb = sum(plot_sum_agb),
    .by = c(lu_no, lu_code_new)
  ) |>
  mutate(
    nb_dw_ha = sum_dw / sum_area_ha,
    agb_ha = sum_agb / sum_area_ha,
    carbon_ha = agb_ha * CF
  ) |>
  select(-starts_with("sum_")) |>
  arrange(lu_no)
landuse_dw

write_csv(landuse_dw, paste0("results/tables/cstock-landuse_dw", Sys.Date(), ".csv"))


##
## stumps ####
##

stump_aggregate <- stump |>
  rename(lu_no = lcs_lu_class_no) |>
  mutate(
    stump_lcs_area = case_when(
      lcs_name == "center" ~ 12^2, ## Square of 12x12 m for all trees
      lcs_name != "center" & stump_diameter_mean <= 30 ~ round((pi*8^2 - 12^2)/4), ## Quarter of 8 m radius circle minus the 12x12 m square 
      lcs_name != "center" & stump_diameter_mean > 30 ~ round((pi*16^2 - 12^2)/4), ## Quarter of 16 m radius circle minus the 12x12 m square
      TRUE ~ NA_real_
    ),
    stump_lcs_weight = round(10000/stump_lcs_area, 3)
  ) |>
  summarise(
    lcs_nb_stump_ha = sum(stump_lcs_weight), 
    lcs_agb_ha   = sum(stump_lcs_weight * stump_agb / 1000), 
    .by = c(subplot_plot_no, subplot_no, stump_lcs_no)
  ) |>
  arrange(subplot_plot_no, subplot_no, stump_lcs_no)

lcs_stump <- lcs_access |>
  left_join(stump_aggregate, by = join_by(subplot_plot_no, subplot_no, lcs_no == stump_lcs_no)) |>
  mutate(
    lcs_area_ha = if_else(lcs_no == 1, 12^2 / 10000, round((pi*16^2-12^2)/4, 0)/ 10000),
    lcs_sum_stump  = if_else(is.na(lcs_nb_stump_ha), 0, round(lcs_nb_stump_ha * lcs_area_ha, 3)),
    lcs_sum_agb = if_else(is.na(lcs_agb_ha), 0, round(lcs_agb_ha * lcs_area_ha, 3))
  )

plot_stump <- lcs_stump |>
  summarise(
    count_lcs = n(),
    plot_area_ha = sum(lcs_area_ha),
    plot_sum_stump = sum(lcs_sum_stump),
    plot_sum_agb = sum(lcs_sum_agb),
    .by = c(subplot_plot_no, lu_no, lu_code_new)
  )

landuse_stump <- plot_stump |>
  summarise(
    count_plot = n(),
    sum_area_ha = sum(plot_area_ha),
    sum_stump = sum(plot_sum_stump),
    sum_agb = sum(plot_sum_agb),
    .by = c(lu_no, lu_code_new)
  ) |>
  mutate(
    nb_stump_ha = sum_stump / sum_area_ha,
    agb_ha = sum_agb / sum_area_ha,
    carbon_ha = agb_ha * CF
  ) |>
  select(-starts_with("sum_")) |>
  arrange(lu_no)
landuse_stump

write_csv(landuse_stump, paste0("results/tables/cstock-landuse-stump", Sys.Date(), ".csv"))



##
## Lying deadwood ####
##

## Many subplots and plots have no deadwood, they should get 0 if accessed

## make a table with lu at subplot center
## full join all accessible plots with LU then LDW
subplot_measured <- lcs |>
  filter(lcs_no == 1) |>
  mutate(lu_no = as.numeric(case_when(
    lcs_lu_class_txt == "16AC" ~ "161",
    lcs_lu_class_txt == "16EC" ~ "162",
    lcs_lu_class_txt == "16PN" ~ "163",
    lcs_lu_class_txt == "16RB" ~ "164",
    lcs_lu_class_txt == "16TK" ~ "165",
    lcs_lu_class_txt == "16OTH" ~ "169",
    TRUE ~ lcs_lu_class_txt
  ))) |>
  select(-lcs_no, -lcs_name, -lcs_lu_class_txt) |>
  rename(subplot_plot_no = lcs_plot_no, subplot_no = lcs_subplot_no) |>
  left_join(anci$lc, by = join_by(lu_no))

subplot_ldw <- subplot_measured |>
  left_join(ldw_aggregate, by = join_by(subplot_plot_no, subplot_no)) |>
  mutate(
    subplot_area_ha = round(pi * 16^2 / 10000, 3),
    subplot_agb_ha = if_else(is.na(ldw_agb_ha), 0, ldw_agb_ha / 1000),
    subplot_sum_agb = subplot_agb_ha * subplot_area_ha
  )
  
plot_ldw <- subplot_ldw |>
  summarise(
    count_subplot = n(),
    plot_area_ha = sum(subplot_area_ha),
    plot_sum_agb = sum(subplot_sum_agb),
    .by = c(subplot_plot_no, lu_no, lu_code_new)
  )

landuse_ldw <- plot_ldw |>
  summarise(
    count_plot = n(),
    sum_area_ha = sum(plot_area_ha),
    sum_agb = sum(plot_sum_agb),
    .by = c(lu_no, lu_code_new)
  ) |>
  mutate(
    agb_ha = sum_agb / sum_area_ha,
    carbon_ha = agb_ha * 0.47,
  ) |>
  select(-starts_with("sum_")) |>
  arrange(lu_no)
landuse_ldw

write_csv(landuse_ldw, paste0("results/tables/cstock-landuse-ldw", Sys.Date(), ".csv"))

