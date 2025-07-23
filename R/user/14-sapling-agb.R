
sapling <- sapling |>
  mutate(
    sapling_agb = 0.6 * exp(-1.499 + 2.148 * log(sapling_dbh) + 0.207 * (log(sapling_dbh))^2 - 0.0281*(log(sapling_dbh))^3)
  )


