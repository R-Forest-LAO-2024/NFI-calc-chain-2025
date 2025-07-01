
## Consistency with previous NFI

anci$wd_default <- 0.6


## Species/genus specific tables

# table(anci$wd_raw$wd_region)

anci$wd_species <- anci$wd_raw |>
  filter(wd_region %in% c("South-East Asia", "South-East Asia (tropical)", "China")) |>
  summarise(wd_species_mean = round(mean(wd_value), 3), .by = "wd_species")

anci$wd_genus <- anci$wd_raw |>
  filter(wd_region %in% c("South-East Asia", "South-East Asia (tropical)", "China")) |>
  mutate(wd_genus = word(wd_species)) |>
  summarise(wd_genus_mean = round(mean(wd_value), 3), .by = "wd_genus")

# anci$wd_default <- anci$wd_raw |>
#   filter(wd_region %in% c("South-East Asia", "South-East Asia (tropical)", "China")) |>
#   summarise(wd_default = round(mean(wd_value), 3)) |>
#   pull(wd_default)

