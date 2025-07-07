
## Check
tt <- tree |> filter(subplot_plot_no == 74) |> select(-starts_with("ONA"), -ends_with("old"))

## Group nb tree / ha per lcs ####

lcs_nb_tree <- tree |>
  summarise(
    lcs_nb_tree = sum(tree_weight), 
    lcs_ba      = sum(tree_weight * tree_ba),
    lcs_agb     = sum(tree_weight * tree_agb_final / 1000), 
    lcs_carbon  = sum(tree_weight * tree_agb_final * (1 + tree_rs) * 0.47 / 1000),
    .by = c(subplot_plot_no, subplot_no, tree_lcs_no, lu_strata_no, lu_strata_name)
    ) |>
  arrange(subplot_plot_no, subplot_no, tree_lcs_no) |>
  mutate(
    lcs_area_ha    = if_else(tree_lcs_no == 1, 12^2 / 1000, round((pi*16^2-12^2)/4, 0)/ 1000),
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
