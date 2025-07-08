
## Check
# tt <- tree |> filter(subplot_plot_no == 74) |> select(-starts_with("ONA"), -ends_with("old"))

## tree_plot_weight



## Nb tree per diameter class 
lcs_ntree_dbhclass <- tree |>
  group_by(subplot_plot_no, subplot_no, tree_lcs_no, lu_strata_no, lu_strata_name, tree_dbh_class_reduced) |>
  summarise(lcs_nb_tree = sum(tree_weight), .groups = "drop") |>
  arrange(subplot_plot_no, subplot_no, tree_lcs_no, tree_dbh_class_reduced) |>
  mutate(
    lcs_area    = if_else(tree_lcs_no == 1, 12^2 / 10000, round((pi*16^2-12^2)/4, 0) / 10000),
    lcs_sum_tree   = round(lcs_nb_tree * lcs_area, 3),
  )

plot_ntree_dbhclass <- lcs_ntree_dbhclass |>
  group_by(subplot_plot_no, lu_strata_no, lu_strata_name, tree_dbh_class_reduced) |>
  summarise(
    count_lcs = n(),
    plot_sum_area = sum(lcs_area),
    plot_sum_tree = sum(lcs_sum_tree),
    .groups = "drop"
  )

strata_ntree_dbhclass <- plot_ntree_dbhclass |>
  group_by(lu_strata_no, lu_strata_name, tree_dbh_class_reduced) |>
  summarise(
    count_plot = n(),
    strata_sum_area = sum(plot_sum_area),
    strata_sum_tree = sum(plot_sum_tree),
    .groups = "drop"
  ) |>
  mutate(
    nb_tree_ha = strata_sum_tree / strata_sum_area
  )

strata_ntree_dbhclass |>
  ggplot(aes(x = tree_dbh_class_reduced)) +
  geom_col(aes(y = nb_tree_ha, fill = lu_strata_name), width = 4) +
  scale_x_continuous(breaks = 0:10* 10) +
  theme(legend.position = "none") +
  facet_wrap(~lu_strata_name)

## Group nb tree / ha per lcs ####

lcs_nb_tree <- tree |>
  summarise(
    lcs_nb_tree = sum(tree_weight), 
    lcs_ba      = sum(tree_weight * tree_ba),
    lcs_agb     = sum(tree_weight * tree_agb_final / 1000), 
    lcs_carbon  = sum(tree_weight * tree_carbon / 1000),
    .by = c(subplot_plot_no, subplot_no, tree_lcs_no, lu_strata_no, lu_strata_name)
    ) |>
  arrange(subplot_plot_no, subplot_no, tree_lcs_no) |>
  mutate(
    lcs_area_ha    = if_else(tree_lcs_no == 1, 12^2 / 10000, round((pi*16^2-12^2)/4, 0)/ 10000),
    lcs_sum_tree   = round(lcs_nb_tree * lcs_area_ha),
    lcs_sum_ba     = round(lcs_ba * lcs_area_ha),
    lcs_sum_agb    = round(lcs_agb * lcs_area_ha),
    lcs_sum_carbon = round(lcs_carbon * lcs_area_ha)
  )

plot_nb_tree <- lcs_nb_tree |>
  summarise(
    count_lcs = n(),
    plot_area_ha = sum(lcs_area_ha),
    plot_sum_tree = sum(lcs_sum_tree),
    plot_sum_ba = sum(lcs_sum_ba),
    plot_sum_agb = sum(lcs_sum_agb),
    plot_sum_carbon = sum(lcs_sum_carbon),
    .by = c(subplot_plot_no, lu_strata_no, lu_strata_name)
  )

stat_nb_tree <- plot_nb_tree |>
  summarise(
    count_plot = n(),
    strata_sum_area_ha = sum(plot_area_ha),
    strata_sum_tree = sum(plot_sum_tree),
    strata_sum_ba = sum(plot_sum_ba),
    strata_sum_agb = sum(plot_sum_agb),
    strata_sum_carbon = sum(plot_sum_carbon),
    .by = c(lu_strata_no, lu_strata_name)
  ) |>
  mutate(
    nb_tree_ha = strata_sum_tree / strata_sum_area_ha,
    ba_ha = strata_sum_ba / strata_sum_area_ha,
    agb_ha = strata_sum_agb / strata_sum_area_ha,
    carbon_ha = strata_sum_carbon / strata_sum_area_ha,
  )
stat_nb_tree
