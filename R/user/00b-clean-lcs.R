
## Clean errors in land cover sections

tmp <- list()

## Since duplicates found in subplot ID, lcs need to be recompiled from clean subplot

tmp$lcs_code <- tibble(
  lcs_no = 1:5,
  lcs_name = c("center", "north", "east", "south", "west"),
)

data_clean$lcs <- data_clean$subplot |>
  select(subplot_plot_no, subplot_no, starts_with("subplot_lc_class")) |>
  pivot_longer(
    cols = c(starts_with("subplot_lc_class")), 
    values_to = "lcs_lu_class_txt",
    names_to = "lcs_name", 
    names_pattern = "subplot_lc_class_?(.*)"
  ) |>
  left_join(tmp$lcs_code, by = join_by(lcs_name)) |>
  rename(lcs_plot_no = subplot_plot_no, lcs_subplot_no = subplot_no) |>
  select(lcs_plot_no, lcs_subplot_no, lcs_no, lcs_name, lcs_lu_class_txt)


## Check
tmp$lcs_check <- data_clean$lcs |>
  mutate(lcs_id = paste0(lcs_plot_no, lcs_subplot_no, lcs_no))

message("Unique LCS codes: ", nrow(tmp$lcs_check) == length(unique(tmp$lcs_check$lcs_id)))

rm(tmp)
