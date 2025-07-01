
anci <- list()

## administrative boundaries ####
anci$sf_country <- st_read(file.path(path$dat$anci, "gadm/gadm41_LAO_0.json"), quiet = T)
anci$sf_prov <- st_read(file.path(path$dat$anci, "gadm/gadm41_LAO_1.json"), quiet = T)
anci$sf_dist <- st_read(file.path(path$dat$anci, "gadm/gadm41_LAO_2.json"), quiet = T)

## Land cover table ####
anci$lc <- read_csv(file.path(path$dat$anci, "land-cover.csv"), show_col_types = F)

## Species code list ####
anci$species_list <- read_csv(file.path(path$dat$anci, "species-list.csv"), show_col_types = F)

## Planned location for each plot/subplot ####
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

## CEO observations ####
anci$ceo <- read_csv(file.path(path$dat$anci, usr$get_ceofile), show_col_types = F)

anci$lc_ceo <- read_csv(file.path(path$dat$anci, "land-cover-ceo.csv"), show_col_types = F)

anci$ceo_nfi_id <-  read_csv(file.path(path$dat$anci, "ceo-phase2-codes.csv"), show_col_types = F)

anci$ceo_nfi_id2 <- read_csv(file.path(path$dat$anci, "ceo-phase2-full-selection.csv"), show_col_types = F) |>
  mutate(
    visited = if_else(!is.na(plotid), TRUE, FALSE)
  )

## + Make a clean list of all field plots from CEO ####
# anci$ceo_nfi_id2 |>
#   filter(Plot == "A") |>
#   mutate(x = LON, y = LAT) |>
#   st_as_sf(coords = c("x", "y"), crs = 4326) |>
#   st_write(file.path(path$res$test, "NFI-phase2-fullgrid-plot.kml"))

max_visited <- max(anci$ceo_nfi_id2$plotid, na.rm = T)

list_plots_ph2_notvisited <- anci$ceo_nfi_id2 |>
  arrange(plotid, ID) |>
  filter(is.na(plotid)) |>
  select(plotid, ID) |>
  distinct() |>
  mutate(plotid_all = max_visited + row_number())

anci$ceo_nfi_id_all <- 

## Chave et al. 2014 environmental factor E raster file ####

if (!"E.bil" %in% list.files(path$dat$anci)) {
  download.file(
    url = "https://github.com/umr-amap/BIOMASS/raw/refs/heads/master/data-raw/climate_variable/E.zip",
    destfile = file.path(path$dat$anci, "E.zip")
  )
  unzip(zipfile = file.path(path$dat$anci, "E.zip"), exdir = path$dat$anci)
  unlink(file.path(path$dat$anci, "E.zip"))
  
}

anci$rs_chaveE <- terra::rast(file.path(path$dat$anci, "E.bil")) 

## Zanne et al 2009 Global Wood Density database ####
if (!"wdData.csv" %in% list.files(path$dat$anci)) {
  download.file(
    url = "https://raw.githubusercontent.com/umr-amap/BIOMASS/refs/heads/master/data-raw/wdData.csv",
    destfile = file.path(path$dat$anci, "wdData.csv")
  )
}

anci$wd_raw <- read_csv(file.path(path$dat$anci, "wdData.csv"), show_col_types = F)

names(anci$wd_raw) <- c("wd_no", "wd_family", "wd_species", "wd_value", "wd_region", "wd_biblio")


## R:S values
# treeplot_rs = case_when(
#   lc_class == "CF" & treeplot_agb_ha <  50 ~ 0.46,
#   lc_class == "CF" & treeplot_agb_ha <= 150 ~ 0.32,
#   lc_class == "CF" & treeplot_agb_ha >  150 ~ 0.23,
#   lc_class != "CF" & treeplot_agb_ha <  125 ~ 0.2,
#   lc_class != "CF" & treeplot_agb_ha >= 125 ~ 0.24,
#   TRUE ~ NA_real_
# )
