
## Rename columns and convert text to numeric

data_prep$ldw <- data_init$ldw |>
  rename_with(str_remove, pattern = ".*__") |>
  rename(ldw_no = ONA_no, ldw_diameter = diameter) |>
  mutate(across(c(ldw_diameter, ldw_density, ldw_hollow_d1, ldw_hollow_d2), as.numeric))

