
## !!! CROSS CHECK in 'R/user/xxx-tree-join.R': 
## Table 'lcs' is join with 'tree_lcs_no_new' means tree weight based on equal distance

tree_ <- tree_ |>
  mutate(
    tree_lcs_area = case_when(
      lcs_location == "center" ~ 12^2, ## Square of 12x12 m for all trees
      lcs_location != "center" & tree_dbh <= 30 ~ round((pi*8^2 - 12^2)/4), ## Quarter of 8 m radius circle minus the 12x12 m square 
      lcs_location != "center" & tree_dbh > 30 ~ round((pi*16^2 - 12^2)/4), ## Quarter of 16 m radius circle minus the 12x12 m square
      TRUE ~ NA_real_
    ),
    tree_weight = round(10000/tree_lcs_area, 3)
  )

