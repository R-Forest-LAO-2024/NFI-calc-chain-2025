

data_clean$tree <- data_clean$tree |>
  mutate(
    tree_x = cos((90 - tree_azimuth) * pi/180) * tree_distance,
    tree_y = sin((90 - tree_azimuth) * pi/180) * tree_distance,
    tree_lcs_no = case_when(
      tree_distance <= 8 ~ 1,
      tree_azimuth > 315 | tree_azimuth <=45  ~ 2,
      tree_azimuth >  45 & tree_azimuth <=90  ~ 3,
      tree_azimuth >  90 & tree_azimuth <=225 ~ 4,
      tree_azimuth > 225 & tree_azimuth <=315 ~ 5,
      TRUE ~ NA_integer_
    ),
    tree_lcs_no_new = case_when(
      abs(tree_x) <= 6 & abs(tree_y) <= 6 ~ 1,
      tree_azimuth > 315 | tree_azimuth <=45  ~ 2,
      tree_azimuth >  45 & tree_azimuth <=90  ~ 3,
      tree_azimuth >  90 & tree_azimuth <=225 ~ 4,
      tree_azimuth > 225 & tree_azimuth <=315 ~ 5,
      TRUE ~ NA_integer_
    )
  )
  #left_join(data_clean$)


## Check
data_clean$tree |>
  filter(ONA_parent_index == 2) |>
  ggplot(aes(x = tree_x, y = tree_y)) +
  geom_text(aes(label = tree_azimuth, color = as.character(tree_lcs_no_new))) +
  gg_treeplot_center +
  geom_path(data = data.frame(x = c(-6, -6, 6, 6, -6), y = c(-6, 6, 6, -6, -6)), aes(x, y)) +
  theme(legend.position = "none") +
  coord_fixed()
  
# 
# data_clean$subplot |> 
#   filter(ONA_index == 1) |> select(subplot_plot_no, subplot_no)
# 
# data_clean$lcs |> filter(lcs_plot_no == 484, lcs_subplot_no == "D")
