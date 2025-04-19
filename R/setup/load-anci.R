
anci <- list()

anci$sf_country <- st_read(file.path(path$dat$anci, "gadm/gadm41_LAO_0.json"), quiet = T)
anci$sf_prov <- st_read(file.path(path$dat$anci, "gadm/gadm41_LAO_1.json"), quiet = T)
anci$sf_dist <- st_read(file.path(path$dat$anci, "gadm/gadm41_LAO_2.json"), quiet = T)


anci$lc <- read_csv(file.path(path$dat$anci, "land-cover.csv"), show_col_types = F)

anci$species_list <- read_csv(file.path(path$dat$anci, "species-list.csv"), show_col_types = F)

anci$subplot_plan <- read_csv(file.path(path$dat$anci, "subplot-planned-location.csv"), show_col_types = F) |>
  select(
    subplot_plot_no = plotid, subplot_no = smpl_plot, 
    subplot_plan_lon = lon, subplot_plan_lat = lat
  ) |>
  mutate(
    subplot_id = case_when(
      subplot_plot_no < 10  ~ paste0("00", subplot_plot_no, subplot_no), 
      subplot_plot_no < 100 ~ paste0("0", subplot_plot_no, subplot_no), 
      TRUE ~ paste0(subplot_plot_no, subplot_no)
    )
  )

## CEO
anci$lc_ceo <- read_csv(file.path(path$dat$anci, "land-cover-ceo.csv"), show_col_types = F)

anci$ceo <- read_csv(file.path(path$dat$anci, usr$get_ceofile), show_col_types = F)


