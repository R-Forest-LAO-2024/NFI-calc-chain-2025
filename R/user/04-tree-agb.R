
table(tree$lu_name, tree$lu_code_new)

tree <- tree |>
  mutate(
    tree_agb_final = case_when(
      lu_code_new == "EG"    ~ 0.3112 * tree_dbh^2.2331,
      lu_code_new == "MD"    ~ 0.523081 * tree_dbh^2,
      lu_code_new == "DD"    ~ 0.2137 * tree_dbh^2.2575,
      lu_code_new == "CF"    ~ 0.1277 * tree_dbh^2.3944,
      lu_code_new == "MCB"   ~ 0.1277 * tree_dbh^2.3944,
      lu_code_new == "P_AC"  ~ 0.1173 * tree_dbh^2.454,
      lu_code_new == "P_EC"  ~ 0.199 * tree_dbh^2.185,
      lu_code_new == "P_RB"  ~ 0.0082 * (pi*tree_dbh)^2.5623, ## Rubber model uses circumference
      lu_code_new == "P_TK"  ~ 0.077 * tree_dbh^2.546,
      # lu_code_new == "P_AC"  ~ 0.3112 * tree_dbh^2.2331,  
      # lu_code_new == "P_EC"  ~ 0.3112 * tree_dbh^2.2331,    
      # lu_code_new == "P_RB"  ~ 0.3112 * tree_dbh^2.2331, 
      # lu_code_new == "P_TK"  ~ 0.3112 * tree_dbh^2.2331, 
      lu_code_new == "P_OTH" ~ 0.3112 * tree_dbh^2.2331,
      lu_code_new == "RV"    ~ 0.6 * exp(-1.499 + 2.148 * log(tree_dbh) + 0.207 * (log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3),
      lu_code_new == "B"     ~ 0.6 * exp(-1.499 + 2.148 * log(tree_dbh) + 0.207 * (log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3),
      TRUE ~ 0
    ),
    tree_agb_final = round(tree_agb_final, 3),
    # tree_agb_chave14 = round(exp(-1.803 - 0.976*plot_E + 0.976*log(wd_all) + 2.673*log(tree_dbh) -0.0299*(log(tree_dbh))^2), 3),
    # tree_agb_chave05 = round(wd_all * exp(-1.499 + 2.148 * log(tree_dbh) + 0.207 * (log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3), 3),
    tree_agb_chave14_wd06 = round(exp(-1.803 - 0.976*plot_E + 0.976*log(0.6) + 2.673*log(tree_dbh) -0.0299*(log(tree_dbh))^2), 3),
    tree_agb_chave05_wd06 = round(0.6 * exp(-1.499 + 2.148*log(tree_dbh) + 0.207*(log(tree_dbh))^2 - 0.0281*(log(tree_dbh))^3), 3),
    tree_agb_EG = 0.3112 * tree_dbh^2.2331,
  )


## Check
table(tree$lu_strata_name)

gg_tree_agb <- tree |>
  filter(
    lu_strata_name != "Nonforest", 
    !lu_code_new %in% c("P_AC", "P_EC", "P_OTH", "P_RB", "P_TK"),
    ) |>
  ggplot(aes(x = tree_dbh)) +
  geom_line(aes(y = tree_agb_chave05_wd06), linetype = "dashed") +
  geom_point(aes(y = tree_agb_chave14_wd06), size = 0.8, alpha = 0.8, shape = 4) +
  geom_line(aes(y = tree_agb_EG), size = 0.8) +
  geom_line(aes(y = tree_agb_final), col = "darkred", linewidth = 0.8) +
  facet_wrap(~lu_code_new) +
  labs(
    x = "DBH",
    y = "AGB",
    caption = "red line: selected model, line: EG model,\ndash: chave05, cross: Chave14"
  )
print(gg_tree_agb)

gg_tree_agbp <- tree |>
  filter(
    lu_code_new %in% c("P_AC", "P_EC", "P_OTH", "P_RB", "P_TK"),
  ) |>
  ggplot(aes(x = tree_dbh)) +
  geom_line(aes(y = tree_agb_chave05_wd06), linetype = "dashed") +
  geom_point(aes(y = tree_agb_chave14_wd06), size = 0.8, alpha = 0.8, shape = 4) +
  geom_line(aes(y = tree_agb_EG), size = 0.8) +
  geom_line(aes(y = tree_agb_final), col = "darkred", linewidth = 0.8) +
  facet_wrap(~lu_code_new) +
  labs(
    x = "DBH",
    y = "AGB",
    caption = "red line: selected model, line: EG model,\ndash: chave05, cross: Chave14"
  )
print(gg_tree_agbp)

