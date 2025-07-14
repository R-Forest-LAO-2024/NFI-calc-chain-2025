
subplot |>
  mutate(x = subplot_gps_lon, y = subplot_gps_lat) |>
  filter(!is.na(x), !is.na(y)) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_write(file.path(path$res$data, "subplot_gps.kml"), delete_dsn = T)

anci$ceo |>
  mutate(x = str_remove(pl_xy, ".*,"), y = str_remove(pl_xy, ",.*")) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_write(file.path(path$res$data, "CEO_ph1.kml"), delete_dsn = T)

anci$ceo |>
  filter(pl_orig_fid %in% anci$ceo_nfi_id_all$pl_orig_fid) |>
  mutate(x = str_remove(pl_xy, ".*,"), y = str_remove(pl_xy, ",.*")) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_write(file.path(path$res$data, "CEO_ph2.kml"), delete_dsn = T)

