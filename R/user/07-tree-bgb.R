
## Tree BGB require first subplot level tree agb estimate to assign the AGB threshold 
tmp_subplot_agb <- tree |>
  group_by(subplot_plot_no, subplot_no) |>
  summarise(subplot_agb = round(sum(tree_agb_final * tree_weight / 5)/ 1000), .groups = "drop")

## Check
# tmp_subplot_agb |>
#   filter(subplot_agb > 500) |>
#   ggplot(aes(x = subplot_plot_no, y = subplot_agb)) +
#   geom_point(data = tmp_subplot_agb, aes(colour = as.character(subplot_plot_no))) +
#   geom_text(aes(label = paste0(subplot_plot_no, subplot_no))) +
#   theme(legend.position = "none")

# tmp_tree <- tree |> filter(subplot_plot_no == 301)

## Get subplot AGB back to tree
tree <- tree |>
  mutate(subplot_agb = NA) |>
  left_join(tmp_subplot_agb, by = join_by(subplot_plot_no, subplot_no), suffix = c("_rm", "")) |>
  select(-ends_with("_rm")) |>
  mutate(
    tree_rs = case_when(
      lu_code_new == "CF" & subplot_agb <  50 ~ 0.46,
      lu_code_new == "CF" & subplot_agb <= 150 ~ 0.32,
      lu_code_new == "CF" & subplot_agb >  150 ~ 0.23,
      lu_code_new != "CF" & subplot_agb <  125 ~ 0.2,
      lu_code_new != "CF" & subplot_agb >= 125 ~ 0.24,
      TRUE ~ NA_real_
    ),
    tree_bgb = round(tree_agb_final * tree_rs, 3),
  )

rm(tmp_subplot_agb)
