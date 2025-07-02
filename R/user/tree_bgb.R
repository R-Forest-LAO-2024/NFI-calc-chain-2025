
## Tree BGB require first subplot level tree agb estimate to assign the AGB threshold 

tmp_subplot_agb <- tree_ |>
  group_by(subplot_plot_no, subplot_no) |>
  summarise(subplot_agb = round(sum(tree_agb_final * tree_weight / 5)/ 1000), .groups = "drop") 

## Check
tmp_subplot_agb |>
  filter(subplot_agb > 500) |>
  ggplot(aes(x = subplot_plot_no, y = subplot_agb)) +
  geom_point(data = tmp_subplot_agb, aes(colour = as.character(subplot_plot_no))) +
  geom_text(aes(label = paste0(subplot_plot_no, subplot_no))) +
  theme(legend.position = "none")
