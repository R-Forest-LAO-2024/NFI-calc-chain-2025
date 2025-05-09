
## Check
if(!"tree_lcs_no" %in% names(tree)) stop("Run tree analysis scripts first")
if(!"tree_dbh_nest" %in% names(tree)) stop("Run tree analysis scripts first")

## List for tmp objects
tmp <- list()

## Create directory for the figures
tmp$dir <- paste0(path$res$fig, "/suplot-lcs")

if (!tmp$dir %in% list.dirs(path$res$fig)) dir.create(tmp$dir)


## Getr all plot numbers
tmp$plot_no <- subplot |> pull(subplot_plot_no) |> unique() |> sort()

## For each plot number, create figure and save it in tmp$dir

## x = tmp$plot_no[1]

walk(tmp$plot_no, function(x){
  
  ONA_numbers <- tree |> 
    filter(subplot_plot_no == x) |> 
    pull(ONA_parent_index) |> unique() |>
    sort()
  
  plot_txt    <- case_when(
    x <  10 ~ paste0("00", x),
    x < 100 ~ paste0("0", x),
    TRUE ~ as.character(x)
  )
  
  tmp$spA <- tree |> filter(subplot_plot_no == x, subplot_no == "A")
  tmp$spB <- tree |> filter(subplot_plot_no == x, subplot_no == "B")
  tmp$spC <- tree |> filter(subplot_plot_no == x, subplot_no == "C")
  tmp$spD <- tree |> filter(subplot_plot_no == x, subplot_no == "D")
  
  out <- ggplot() +
    ## Real distance if needed
    # gg_subplot_lcs(.tree = tmp$spA, .sp_center = c( 0,   0)) +
    # gg_subplot_lcs(.tree = tmp$spB, .sp_center = c( 0,  60)) +
    # gg_subplot_lcs(.tree = tmp$spC, .sp_center = c( 0, 120)) +
    # gg_subplot_lcs(.tree = tmp$spD, .sp_center = c(60,   0)) +
    ## Fake distance to make figure easier to read
    gg_subplot_lcs(.tree = tmp$spA, .sp_center = c( 0,  0)) +
    gg_subplot_lcs(.tree = tmp$spB, .sp_center = c( 0, 36)) +
    gg_subplot_lcs(.tree = tmp$spC, .sp_center = c( 0, 72)) +
    gg_subplot_lcs(.tree = tmp$spD, .sp_center = c(36,  0)) +
    scale_size_discrete(range = c(2, 1)) +
    theme_void() +
    theme(
      legend.position  = "inside",
      legend.position.inside = c(0.7, 0.7),
      ) +
    coord_fixed() +
    labs(
      title = paste0("Plot number: ", x),
      subtitle = paste0("ONA index: ", paste(ONA_numbers, collapse = ", ")),
      x = "",
      y = "",
      size = "DBH size",
      color = "Land cover",
      caption = "Distance between subplot centers: 60 m"
    )
  
  print(out)
  
  ggsave(
    plot = out, filename = file.path(tmp$dir, paste0("land-cover-sections-", plot_txt, ".png")),
    width = 12, height = 8, dpi = 300, bg = "white"
  )
  
  
})

tt <- tree |> filter(subplot_plot_no == 27)
