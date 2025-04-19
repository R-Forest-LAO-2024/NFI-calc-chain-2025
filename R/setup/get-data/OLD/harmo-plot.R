
## Plots can only be made as a subset of treeplot.
## Make sure harmo-treeplot.R ran first.

## Initiate list to store temporary objects
tmp <- list()

## make plot as subset of treeplot with treeplot_no  == 'A'
tmp$plot_coords <- data_harmo$treeplot |>
  filter(treeplot_no == "A") |>
  select(
    plot_no = treeplot_plot_no, 
    plot_coord_lon = treeplot_coord_lon,
    plot_coord_lat = treeplot_coord_lat,
    plot_coord_source = treeplot_coord_source,
    plot_coord_x = treeplot_coord_x,
    plot_coord_y = treeplot_coord_y,
    plot_coord_xy_crs = treeplot_coord_xy_crs,
    plot_lc_class = treeplot_lc_class,
    plot_lc_type = treeplot_lc_type
    )
  
## make plot time a combination of min and max of treeplot
tmp$plot_time <- data_harmo$treeplot |>
  rename(plot_no = treeplot_plot_no) |>
  group_by(plot_no) |>
  summarise(
    plot_survey_time = min(treeplot_survey_time),
    plot_time_start  = min(treeplot_time_start),
    plot_time_end    = max(treeplot_time_end),
    plot_date_measure = min(treeplot_date_measure),
    plot_count_tp = n()
  ) |>
  mutate(plot_access = if_else(plot_count_tp < 4, "partial", "full"))

table(tmp$plot_time$plot_count_tp, useNA = "ifany")

## !!! THIS WILL BE SOLVED BY CORRECTING TYPOS IN TREEPLOT
## check 
tmp$plot_no_time   <- tmp$plot_time$plot_no |> sort()
tmp$plot_no_coords <- tmp$plot_coords$plot_no |> sort()

length(tmp$plot_no_time)
length(tmp$plot_no_coords)

tmp$missing_plot <- tmp$plot_no_time[!tmp$plot_no_time %in% tmp$plot_no_coords]

## Combine data
data_harmo$plot <- tmp$plot_coords |>
  left_join(tmp$plot_time, by = "plot_no")

nrow(data_harmo$plot)

## Remove tmp objects 
rm(tmp)


