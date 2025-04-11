
anci <- list()

anci$sf_country <- st_read(file.path(path$dat$anci, "gadm/gadm41_LAO_0.json"), quiet = T)
anci$sf_prov <- st_read(file.path(path$dat$anci, "gadm/gadm41_LAO_1.json"), quiet = T)
anci$sf_dist <- st_read(file.path(path$dat$anci, "gadm/gadm41_LAO_2.json"), quiet = T)


anci$lc <- read_csv(file.path(path$dat$anci, "land_cover.csv"), show_col_types = F)

anci$species_list <- read_csv(file.path(path$dat$anci, "species_list.csv"), show_col_types = F)

anci$treeplot_plan <- read_csv(file.path(path$dat$anci, "treeplot_planned.csv"), show_col_types = F) |>
  select(
    treeplot_plot_no = plotid, treeplot_no = smpl_plot, 
    treeplot_plan_lon = lon, treeplot_plan_lat = lat
  ) |>
  mutate(
    treeplot_id = case_when(
      treeplot_plot_no < 10  ~ paste0("00", treeplot_plot_no, treeplot_no), 
      treeplot_plot_no < 100 ~ paste0("0", treeplot_plot_no, treeplot_no), 
      TRUE ~ paste0(treeplot_plot_no, treeplot_no)
    )
  )
