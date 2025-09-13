
## Checks
# table(tree$lcs_name, useNA = "ifany")

tree <- tree |>
  mutate(
    tree_weight = case_when(
      tree_dbh < 30 ~ pi * 16^2 / (pi * 8^2),
      tree_dbh >= 30  ~ 1 
    ),
    tree_spha = if_else(tree_dbh < 30, pi * 8^2 / 10000, pi * 16^2 / 10000),
    tree_weight_spha = 1 / tree_spha
  )


## NOT CORRECT - Weight is based on ratio of small circle to biog circle
# tree <- tree |>
#   mutate(
#     tree_lcs_area = case_when(
#       lcs_name == "center" ~ 12^2, ## Square of 12x12 m for all trees
#       lcs_name != "center" & tree_dbh <= 30 ~ round((pi*8^2 - 12^2)/4), ## Quarter of 8 m radius circle minus the 12x12 m square
#       lcs_name != "center" & tree_dbh > 30 ~ round((pi*16^2 - 12^2)/4), ## Quarter of 16 m radius circle minus the 12x12 m square
#       TRUE ~ NA_real_
#     ),
#     tree_weight_spha = round(10000/tree_lcs_area, 3)
#   )

