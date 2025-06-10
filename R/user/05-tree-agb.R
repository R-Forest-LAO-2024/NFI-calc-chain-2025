
table(tree$lcs_name, tree$lcs_code_new)

tree <- tree |>
  mutate(
    tree_agb = case_when(
      lcs_code_new == "EG" ~ 0.3112 * tree_dbh^2.2331,
      lcs_code_new == "MD" ~ 0.523081 * tree_dbh^2,
      lcs_code_new == "DD" ~ 0.2137 * tree_dbh^2.2575,
      lcs_code_new == "CF" ~ 0.1277 * tree_dbh^2.3944,
      lcs_code_new == "MCB" ~ 0.1277 * tree_dbh^2.3944,
      
      TRUE ~ 0
    )
  )

