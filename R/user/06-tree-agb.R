
table(tree$lu_name, tree$lu_code_new)

tree <- tree |>
  mutate(
    tree_agb_final = case_when(
      lu_code_new == "EG"    ~ 0.3112 * tree_dbh^2.2331,
      lu_code_new == "MD"    ~ 0.523081 * tree_dbh^2,
      lu_code_new == "DD"    ~ 0.2137 * tree_dbh^2.2575,
      lu_code_new == "CF"    ~ 0.1277 * tree_dbh^2.3944,
      lu_code_new == "MCB"   ~ 0.1277 * tree_dbh^2.3944,
      lu_code_new == "P_AC"  ~ 0.1173 * tree_dbh^2.454,  # 0.3112 * tree_dbh^2.2331,
      lu_code_new == "P_EC"  ~ 0.199 * tree_dbh^2.185,   # 0.3112 * tree_dbh^2.2331,
      lu_code_new == "P_RB"  ~ 0.0082 * tree_dbh^2.5623, # 0.3112 * tree_dbh^2.2331,
      lu_code_new == "P_TK"  ~ 0.077 * tree_dbh^2.546,   # 0.3112 * tree_dbh^2.2331,
      lu_code_new == "P_OTH" ~ 0.3112 * tree_dbh^2.2331,
      lu_code_new == "RV"    ~ 0.6 * exp(-1.499 + 2.148 * log(tree_dbh) + 0.207 * (log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3),
      lu_code_new == "B"     ~ 0.6 * exp(-1.499 + 2.148 * log(tree_dbh) + 0.207 * (log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3),
      TRUE ~ 0
    ),
    tree_agb_final = round(tree_agb_final, 3),
    tree_agb_chave14 = round(exp(-1.803 - 0.976*plot_E + 0.976*log(wd_all) + 2.673*log(tree_dbh) -0.0299*(log(tree_dbh))^2), 3),
    tree_agb_chave05 = round(wd_all * exp(-1.499 + 2.148 * log(tree_dbh) + 0.207 * (log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3), 3),
    tree_agb_chave05_wd06 = round(0.6 * exp(-1.499 + 2.148 * log(tree_dbh) + 0.207 * (log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3), 3)
  )


## Check
table(tree$lu_strata_name)

tree |>
  filter(
    lu_strata_name != "Nonforest", 
    !lu_code_new %in% c("P_AC", "P_EC", "P_OTH", "P_RB", "P_TK"),
    ) |>
  ggplot(aes(x = tree_dbh)) +
  geom_line(aes(y = tree_agb_final)) +
  geom_line(aes(y = tree_agb_chave05_wd06), linetype = "dashed") +
  geom_point(aes(y = tree_agb_chave14), size = 0.8, col = "darkred", alpha = 0.4) +
  #geom_point(aes(y = tree_agb_chave05), size = 0.8, col = "darkorange", shape = 4, alpha = 0.4) +
  facet_wrap(~lu_code_new) +
  labs(
    x = "DBH",
    y = "AGB",
    caption = "orange cross: Chave05, red point: Chave14\nline: historical models, dash: chave05 WD=0.6g/cm3"
  )


tree |>
  filter(
    lu_code_new %in% c("P_AC", "P_EC", "P_OTH", "P_RB", "P_TK"),
  ) |>
  ggplot(aes(x = tree_dbh)) +
  geom_line(aes(y = tree_agb_final)) +
  geom_line(aes(y = tree_agb_chave05_wd06), linetype = "dashed") +
  geom_point(aes(y = tree_agb_chave14), size = 0.8, col = "darkred", alpha = 0.4) +
  #geom_point(aes(y = tree_agb_chave05), size = 0.8, col = "darkorange", shape = 4, alpha = 0.4) +
  facet_wrap(~lu_code_new) +
  labs(
    x = "DBH",
    y = "AGB",
    caption = "orange cross: Chave05, red point: Chave14\nline: historical models, dash: chave05 WD=0.6g/cm3"
  )


