

sf_plot <- anci$ceo |>
  select(pl_orig_fid, pl_xy) |>
  left_join(anci$ceo_nfi_id, by = join_by(pl_orig_fid)) |>
  filter(!is.na(plotid)) |>
  mutate(
    x = str_remove(pl_xy, ".*,"),
    y = str_remove(pl_xy, ",.*")
  ) |>
  select(plot_no = plotid, x , y) |>
  st_as_sf(coords = c("x", "y"), crs = 4326)

sf_plot_E <- sf_plot |>
  mutate(plot_E = terra::extract(anci$rs_chaveE, vect(sf_plot))[,2])

ggplot() +
  geom_sf(data = anci$sf_country) +
  geom_sf(data = sf_plot_E, aes(color = plot_E))

anci$plot_chaveE <- sf_plot_E |>
  as_tibble() |>
  select(-geometry) |>
  arrange(plot_no)
