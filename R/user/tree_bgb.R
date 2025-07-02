
## Tree BGB require first subplot level tree agb estimate to assign the AGB threshold 
tmp_subplot_agb <- tree_ |>
  group_by(subplot_plot_no, subplot_no) |>
  summarise(subplot_agb = round(sum(tree_agb_final * tree_weight / 5)/ 1000), .groups = "drop") |>
  

## Check
tmp_subplot_agb |>
  filter(subplot_agb > 500) |>
  ggplot(aes(x = subplot_plot_no, y = subplot_agb)) +
  geom_point(data = tmp_subplot_agb, aes(colour = as.character(subplot_plot_no))) +
  geom_text(aes(label = paste0(subplot_plot_no, subplot_no))) +
  theme(legend.position = "none")

# tmp_tree <- tree_ |>
#   filter(subplot_plot_no == 301)

## Get subplot AGB back to tree_
tmp_tree <- tree_ |>
  left_join(tmp_subplot_agb, by = join_by(subplot_plot_no, subplot_no)) |>
  mutate(
    tree_rs = case_when(
      lcs_code_new == "CF" & subplot_agb <  50 ~ 0.46,
      lcs_code_new == "CF" & subplot_agb <= 150 ~ 0.32,
      lcs_code_new != "CF" & subplot_agb >  150 ~ 0.23,
      lcs_code_new != "CF" & subplot_agb <  125 ~ 0.2,
      lcs_code_new != "CF" & subplot_agb >= 125 ~ 0.24,
      TRUE ~ NA_real_
    ),
    tree_bgb = round(tree_agb_final * tree_rs, 3),
  )

## Get back data into tree_ if join didn't destroy the table
if (nrow(tmp_tree) == nrow(tree_)) {
  tree_ <- tmp_tree
  rm(tmp_tree)
} else {
  stop("Issue with joining 'subplot_agb' to 'tree_' table")
}
