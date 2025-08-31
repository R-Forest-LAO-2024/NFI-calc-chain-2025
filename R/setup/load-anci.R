

##
## Load anci tables ####
##

anci <- list()

## + administrative boundaries ####
anci$sf_country <- st_read(file.path(path$dat$anci, "gadm/gadm41_LAO_0.json"), quiet = T)
anci$sf_prov <- st_read(file.path(path$dat$anci, "gadm/gadm41_LAO_1.json"), quiet = T)
anci$sf_dist <- st_read(file.path(path$dat$anci, "gadm/gadm41_LAO_2.json"), quiet = T)

## + Land cover table ####
anci$lc <- read_csv(file.path(path$dat$anci, "land-cover.csv"), show_col_types = F) |>
  rename_with(.cols = starts_with("lc_"), str_replace, "lc_", "lu_")

## + Species code list ####
anci$species_list <- read_csv(file.path(path$dat$anci, "species-list.csv"), show_col_types = F)

## + Planned location for each plot/subplot ####
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

## + CEO observations ####
anci$ceo <- read_csv(file.path(path$dat$anci, usr$get_ceofile), show_col_types = F)

anci$lc_ceo <- read_csv(file.path(path$dat$anci, "land-cover-ceo.csv"), show_col_types = F)

## + Matching codes between CEO and NFI plots ####
## With all phase 2 grid 
anci$ceo_nfi_id <- read_csv(file.path(path$dat$anci, "ceo-phase2-full-selection.csv"), show_col_types = F)

## + Several plots were shifted to new locations ####
## For these plots the CEO strata will be replaced with the FTM value at the new plot location
## Subplot A table compares plot level CEO/FTM and CEO_Class1 (?)
## Subplot FTM assigns FTM class to each shifted subplot location
anci$shifted_spA <- read_csv(file.path(path$dat$anci, "shifted_plots_subplot_A.csv"), show_col_types = F)
anci$shifted_ftm <- read_csv(file.path(path$dat$anci, "shifted_subplots_FTM2022.csv"), show_col_types = F)


## + Chave et al. 2014 environmental factor E raster file ####
if (!"E.bil" %in% list.files(path$dat$anci)) {
  download.file(
    url = "https://github.com/umr-amap/BIOMASS/raw/refs/heads/master/data-raw/climate_variable/E.zip",
    destfile = file.path(path$dat$anci, "E.zip")
  )
  unzip(zipfile = file.path(path$dat$anci, "E.zip"), exdir = path$dat$anci)
  unlink(file.path(path$dat$anci, "E.zip"))
  
}

anci$rs_chaveE <- terra::rast(file.path(path$dat$anci, "E.bil")) 

## + Zanne et al 2009 Global Wood Density database ####
if (!"wdData.csv" %in% list.files(path$dat$anci)) {
  download.file(
    url = "https://raw.githubusercontent.com/umr-amap/BIOMASS/refs/heads/master/data-raw/wdData.csv",
    destfile = file.path(path$dat$anci, "wdData.csv")
  )
}

anci$wd_raw <- read_csv(file.path(path$dat$anci, "wdData.csv"), show_col_types = F)

names(anci$wd_raw) <- c("wd_no", "wd_family", "wd_species", "wd_value", "wd_region", "wd_biblio")


##
## Make WD averages ####
##

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




##
## Make a clean list of all field plots from CEO ####
##

# anci$ceo_nfi_id |>
#   filter(Plot == "A") |>
#   mutate(x = LON, y = LAT) |>
#   st_as_sf(coords = c("x", "y"), crs = 4326) |>
#   st_write(file.path(path$res$test, "NFI-phase2-fullgrid-plot.kml"))

max_visited <- max(anci$ceo_nfi_id$plotid, na.rm = T)

list_plots_ph2_notvisited <- anci$ceo_nfi_id |>
  arrange(plotid, ID) |>
  filter(is.na(plotid)) |>
  select(plotid, ID) |>
  distinct() |>
  mutate(plotid_all = max_visited + row_number())

anci$ceo_nfi_id_all <- anci$ceo_nfi_id |>
  left_join(list_plots_ph2_notvisited, by = join_by(plotid, ID)) |>
  mutate(
    plotid_all = if_else(!is.na(plotid), plotid, plotid_all),
    plot_visited = if_else(!is.na(plotid), TRUE, FALSE)
  ) |>
  select(pl_orig_fid = ORIG_FID, ID, plotid_all, plot_visited) |>
  distinct()

rm(max_visited, list_plots_ph2_notvisited)

write_csv(anci$ceo_nfi_id_all, file.path(path$res$data, "nfi-phase2-codes-full.csv"))




##
## Prepare chave E ####
##

sf_plot <- anci$ceo |>
  select(pl_orig_fid, pl_xy) |>
  left_join(anci$ceo_nfi_id_all, by = join_by(pl_orig_fid)) |>
  filter(!is.na(plotid_all)) |>
  mutate(
    x = str_remove(pl_xy, ".*,"),
    y = str_remove(pl_xy, ",.*")
  ) |>
  select(plot_no = plotid_all, x , y) |>
  st_as_sf(coords = c("x", "y"), crs = 4326)

sf_plot_E <- sf_plot |>
  mutate(plot_E = terra::extract(anci$rs_chaveE, vect(sf_plot))[,2])

# ggplot() +
#   geom_sf(data = anci$sf_country) +
#   geom_sf(data = sf_plot_E, aes(color = plot_E))

anci$plot_chaveE <- sf_plot_E |>
  as_tibble() |>
  select(-geometry) |>
  arrange(plot_no)



