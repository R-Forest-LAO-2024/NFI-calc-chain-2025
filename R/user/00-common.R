

##
## Objects ####
##

## + Create a AGB models parameter table ####
# agb_models <- tibble(
#   lc_class = c("EF", "DD", "MDF", "CF", "MCB"),
#   agb_equation = rep("AGB = a * DBH^b", 5),
#   param_a = c(0.3112, 0.2137, 0.523081, 0.1277, 0.1277),
#   param_b = c(2.2331, 2.2575, 2       , 2.3944, 2.3944)
# )

## + Carbon fraction ####
CF <- 0.47



## 
## misc ####
##

## + Make a clean list of all field plots from CEO ####

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
## Functions ####
##

## + Make plot circles in ggplots ####
gg_showplot <- function(center, vec_radius, n, color = "black"){
  
  theta = seq(0, 2*pi, length = n)
  
  map(vec_radius, function(r){
    data_circle <- data.frame(
      theta = theta,
      x = center[1] + r * cos(theta),
      y = center[2] + r * sin(theta)
    )
    
    geom_path(data = data_circle, aes(x = x, y = y), color = color)
    
  })
  
}


## + Function to make a tree positioning graph with land cover class  ####

## !!! FOR TESTING ONLY - needs tree from "03-tree-lcs-join.R" first
# .tree = tree |> filter(subplot_plot_no == 12, subplot_no == "A")
# .sp_center = c(0, 60)
## !!!

gg_subplot_lcs <- function(.tree, .sp_center = c(0, 0)) {
  
  lcs_frame   <- data.frame(
    x = c(.sp_center[1] - 6, .sp_center[1] - 6, .sp_center[1] + 6, .sp_center[1] + 6, .sp_center[1] - 6), 
    y = c(.sp_center[2] - 6, .sp_center[2] + 6, .sp_center[2] + 6, .sp_center[2] - 6, .sp_center[2] - 6)
    )
  lcs_points  <- data.frame(
    x = c(.sp_center[1], .sp_center[1]     , .sp_center[1] + 12, .sp_center[1]     , .sp_center[1] - 12), 
    y = c(.sp_center[2], .sp_center[2] + 12, .sp_center[2]     , .sp_center[2] - 12, .sp_center[2]     )
    )
  lcs_NE_line <- data.frame(
    x1 = .sp_center[1] + 6, 
    y1 = .sp_center[2] + 6, 
    x2 = .sp_center[1] + 16 * cos(pi/4), 
    y2 = .sp_center[2] + 16 * sin(pi/4)
    )
  lcs_NW_line <- data.frame(
    x1 = .sp_center[1] - 6, 
    y1 = .sp_center[2] + 6, 
    x2 = .sp_center[1] + 16 * cos(3*pi/4), 
    y2 = .sp_center[2] + 16 * sin(3*pi/4)
    )
  lcs_SW_line <- data.frame(
    x1 = .sp_center[1] - 6, 
    y1 = .sp_center[2] - 6, 
    x2 = .sp_center[1] + 16 * cos(5*pi/4), 
    y2 = .sp_center[2] + 16 * sin(5*pi/4)
    )
  lcs_SE_line <- data.frame(
    x1 = .sp_center[1] + 6, 
    y1 = .sp_center[2] - 6, 
    x2 = .sp_center[1] + 16 * cos(7*pi/4), 
    y2 = .sp_center[2] + 16 * sin(7*pi/4)
    )
  
  list(
    gg_showplot(center = .sp_center, vec_radius = 8, n = 100, color = "grey"),
    gg_showplot(center = .sp_center, vec_radius = 16, n = 100),
    geom_path(data = lcs_frame, aes(x, y)),
    geom_segment(data = lcs_NE_line, aes(x = x1, y = y1, xend = x2, yend = y2)),
    geom_segment(data = lcs_NW_line, aes(x = x1, y = y1, xend = x2, yend = y2)),
    geom_segment(data = lcs_SW_line, aes(x = x1, y = y1, xend = x2, yend = y2)),
    geom_segment(data = lcs_SE_line, aes(x = x1, y = y1, xend = x2, yend = y2)),
    geom_point(data = lcs_points, aes(x = x, y = y), size = 2, shape = 3),
    geom_point(data = lcs_points, aes(x = x, y = y), size = 2, shape = 21),
    geom_point(data = .tree, aes(x = (.sp_center[1] + tree_x), y = (.sp_center[2] + tree_y), color = lcs_code_new, size = tree_dbh_nest)),
    geom_text_repel(data = .tree, aes(x = (.sp_center[1] + tree_x), y = (.sp_center[2] + tree_y), label = tree_no, color = lcs_code_new), size = 4, show.legend = F) 
  )
    #geom_text_repel(data = .tree, aes(x = tree_x, y = tree_y, label = paste0(tree_distance, "/", tree_azimuth), color = as.character(tree_lcs_no_new)), size = 4) +
    # theme(legend.position = "none") +
    # coord_fixed() +
    # labs(
    #   subtitle = paste0("Subplot code: ", unique(.tree$subplot_plot_no), unique(.tree$subplot_no)),
    #   x = "",
    #   y = "",
    #   color = "lcs",
    #   #caption = "Labels: distance/azimuth"
    # )
  
}


## + Create ggplot treeplot circles elements at the position of each treeplot in a plot
# gg_treeplot_center <- gg_showplot(center = c(0, 0), vec_radius = c(8, 16), n = 100)
# 
# gg_treeplot_all <- list(
#   ## A
#   gg_treeplot_center,
#   ## B
#   gg_showplot(center = c(0, 60),vec_radius = c(8, 16), n = 100),
#   ## C
#   gg_showplot(center = c(0, 120),vec_radius = c(8, 16), n = 100),
#   ## D
#   gg_showplot(center = c(0, 180),vec_radius = c(8, 16), n = 100),
#   ## E
#   gg_showplot(center = c(60, 0),vec_radius = c(8, 16), n = 100),
#   ## F
#   gg_showplot(center = c(120, 0),vec_radius = c(8, 16), n = 100),
#   ## G
#   gg_showplot(center = c(180, 0),vec_radius = c(8, 16), n = 100),
#   coord_fixed(),
#   theme_bw()
# )
