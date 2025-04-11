

tmp <- list()


luvs_lc_class <- data_clean$subplot |>
  select(subplot_plot_no, subplot_no, subplot_id, starts_with("subplot_lc_class")) |>
  pivot_longer(
    cols = c(starts_with("subplot_lc_class")), 
    values_to = "luvs_lc_class",
    names_to = "luvs_no", 
    names_pattern = "subplot_lc_class_?(.*)"
    )
 

plot_lc_class <- luvs_lc_class |>
  mutate(
    lc_code = paste0("lc", luvs_lc_class),
    count = 1
    ) |>
  pivot_wider(id_cols = subplot_plot_no, names_from = lc_code, values_from = count, values_fn = sum, values_fill = 0)



tmp$check_nb_luvs <- luvs_lc_class |>
  summarise(count = n(), .by = subplot_plot_no)

table(tmp$check_nb_luvs$count, useNA = "ifany")


tmp$check_nb_luvs2 <- luvs_lc_class |>
  summarise(count = n(), .by = subplot_id)

table(tmp$check_nb_luvs2$count, useNA = "ifany")


##
## Check how many plot have a unique LC class
##

plot_nsp <- data_clean$subplot |>
  rename(plot_no = subplot_plot_no) |>
  filter(subplot_access == "accessible") |>
  summarise(count_sp = n(), .by = plot_no)
  

plot_lc1 <- data_clean$subplot |>
  rename(plot_no = subplot_plot_no, lc_class = subplot_lc_class_center) |>
  summarise(count_splc = n(), .by = c(plot_no, lc_class))

nrow(data_clean$subplot)
length(unique(data_clean$subplot$subplot_id))
length(unique(data_clean$subplot$subplot_plot_no))

plot_lc2 <- plot_lc1 |> 
  mutate(lc_code = paste0("lc", lc_class)) |>
  pivot_wider(id_cols = plot_no, names_from = lc_code, values_from = count_splc)
  
  left_join(plot_nsp, by = join_by(plot_no)) |>
  
  mutate(unique_lc = if_else(count_sp == count_splc, "unique", "not unique"))

table(plot_lc2$unique_lc)



