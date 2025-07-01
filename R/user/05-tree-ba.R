
tree_ <- tree_ |>
  mutate(
    tree_ba = round(pi * (tree_dbh / 200)^2, 2)
  )