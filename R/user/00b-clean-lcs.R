
## Clean errors in land cover sections

tmp <- list()

## Since duplicates found in subplot ID, lcs need to be recompiled from clean subplot

tmp$lcs_code <- tibble(
  lcs_location = c("center", "north", "east", "south", "west"),
  lcs_no = 1:5
)

data_clean$lcs <- data_clean$subplot |>
  select(subplot_plot_no, subplot_no, starts_with("subplot_lc_class")) |>
  pivot_longer(
    cols = c(starts_with("subplot_lc_class")), 
    values_to = "lcs_class",
    names_to = "lcs_location", 
    names_pattern = "subplot_lc_class_?(.*)"
  ) |>
  left_join(tmp$lcs_code, by = join_by(lcs_location)) |>
  rename(lcs_plot_no = subplot_plot_no, lcs_subplot_no = subplot_no)


## Check
tmp$lcs_check <- data_clean$lcs |>
  mutate(lcs_id = paste0(lcs_plot_no, lcs_subplot_no, lcs_no))

message("Unique LCS codes: ", nrow(tmp$lcs_check) == length(unique(tmp$lcs_check$lcs_id)))

rm(tmp)
