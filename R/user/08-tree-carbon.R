
## See CF in 00-common.R

tree <- tree |>
  mutate(tree_carbon = round(tree_agb_final * (1 + tree_rs) * CF, 3))
