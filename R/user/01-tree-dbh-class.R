
tree_ <- tree_ |>
  mutate(
    tree_dbh_class = floor(tree_dbh / 10 ) * 10,
    tree_dbh_class_reduced = if_else(tree_dbh_class >= 100, 100, tree_dbh_class),
    tree_dbh_nest = if_else(tree_dbh < 30, "small", "big")
  )
