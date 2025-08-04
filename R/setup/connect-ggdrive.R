
## NOT USED
## Can be adapted to a process where the ONA tables are moved to google drive and retrieved here

# library(googledrive)
# library(googlesheets4)
# 
# tmp <- list()
# 
# googledrive::drive_auth(email = "gaelsola.pro@gmail.com")
# googlesheets4::gs4_auth(email = "gaelsola.pro@gmail.com")
# 
# tmp$gg_dir <- drive_get("NFI4")
# tmp$gg_id <- drive_ls("NFI4")
# 
# if (!"plot_with_admin" %in% tmp$gg_id$name) {
#   
#   if (!"plot_with_admin" %in% drive_ls("~", recursive = FALSE)$name) {
#     googlesheets4::gs4_create("plot_with_admin")
#   }
#   
#   googledrive::drive_mv("plot_with_admin", path = tmp$gg_dir$id)
#   
# }
# 
# tmp$gg_plot_ss <- drive_get("NFI4/plot_with_admin")$id
# 
# ## GET ISO codes for province districts based on GPS coords
# sf_plot <- plot_c1 |>
#   filter(!is.na(plot_coord_lat), !is.na(plot_coord_lon)) |>
#   mutate(lon = plot_coord_lon, lat = plot_coord_lat) |>
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
#   st_join(anci$sf_prov) |> 
#   st_join(anci$sf_dist) |>
#   mutate(
#     ISO_1 = str_replace_all(HASC_1, "\\.", "-"),
#     ISO_2 = str_replace_all(HASC_2, "\\.", "-")
#   ) |>
#   select(
#     starts_with("treeplot_"), starts_with("plot_"), starts_with("ISO_"), starts_with()
#     )
# 
# plot_out <- sf_plot |> 
#   as_tibble() |>
#   mutate(
#     geometry = paste0(plot_coord_lon, ", ", plot_coord_lat)
#   )
# 
# googlesheets4::write_sheet(plot_out, ss = tmp$gg_plot_ss, sheet = "plot")
