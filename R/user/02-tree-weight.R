
## Checks
# table(tree$lcs_name, useNA = "ifany")


tree <- tree |>
  mutate(
    tree_lcs_area = case_when(
      lcs_name == "center" ~ 12^2, ## Square of 12x12 m for all trees
      lcs_name != "center" & tree_dbh <= 30 ~ round((pi*8^2 - 12^2)/4), ## Quarter of 8 m radius circle minus the 12x12 m square 
      lcs_name != "center" & tree_dbh > 30 ~ round((pi*16^2 - 12^2)/4), ## Quarter of 16 m radius circle minus the 12x12 m square
      TRUE ~ NA_real_
    ),
    tree_weight = round(10000/tree_lcs_area, 3)
  )

