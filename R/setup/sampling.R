sampling <- list()

sampling$base_unit <- 1

sampling$plot_area <- data.frame(
  plant_type   = c("tree", "tree", "sapling", "seedling"),
  dbh_min      = c(30, 10, 5, 0),
  plot_radius  = c(16,  8, 2, 2),
  offset_x     = c( 0,  0, 5, 5),
  offset_y     = c( 0,  0, 0, 0)
)
sampling$ldw_length <- 32

