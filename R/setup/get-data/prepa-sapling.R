

## Rename columns and convert text to numeric

data_prep$sapling <- data_init$sapling_init |>
  rename_with(str_remove, pattern = ".*__") |>
  rename_with(str_replace, pattern = "smallt_", replacement = "sapling_") |>
  rename_with(str_replace, pattern = "meas_", replacement = "sapling_") |>
  rename(sapling_no = ONA_no) |>
  rename(sapling_dbh = sapling_dbh_reg) |>
  mutate(
    sapling_distance = as.numeric(sapling_distance),
    sapling_azimuth = as.numeric(sapling_azimuth),
    sapling_dbh = as.numeric(sapling_dbh)
  )

## Test
# summary(data_prep$sapling)
