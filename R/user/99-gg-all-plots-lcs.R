
tmp <- list()

tmp$plot_no <- subplot |> pull(subplot_plot_no) |> unique() |> sort()

x = tmp$plot_no[10]

walk(tmp$plot_no, function(x){
  
  tmp$spA <- tree02 |> filter(subplot_plot_no == x, subplot_no == "A")
  tmp$spB <- tree02 |> filter(subplot_plot_no == x, subplot_no == "B")
  tmp$spC <- tree02 |> filter(subplot_plot_no == x, subplot_no == "C")
  tmp$spD <- tree02 |> filter(subplot_plot_no == x, subplot_no == "D")
  
  ggplot() +
    ## Real distance if needed
    # gg_subplot_lcs(.tree = tmp$spA, .sp_center = c( 0,   0)) +
    # gg_subplot_lcs(.tree = tmp$spB, .sp_center = c( 0,  60)) +
    # gg_subplot_lcs(.tree = tmp$spC, .sp_center = c( 0, 120)) +
    # gg_subplot_lcs(.tree = tmp$spD, .sp_center = c(60,   0)) +
    ## Fake distance to make figure easier to read
    gg_subplot_lcs(.tree = tmp$spA, .sp_center = c( 0,  0)) +
    gg_subplot_lcs(.tree = tmp$spB, .sp_center = c( 0, 40)) +
    gg_subplot_lcs(.tree = tmp$spC, .sp_center = c( 0, 80)) +
    gg_subplot_lcs(.tree = tmp$spD, .sp_center = c(40,  0)) +
    theme_void() +
    theme(legend.position = "none") +
    coord_fixed() +
    labs(
      subtitle = paste0("Plot number: ", x),
      x = "",
      y = "",
      color = "lcs",
      caption = "Distance between subplot centers: 60 m"
    )
  
  print(tmp$gg_lcs_example_new)
  
  ggsave(
    file.path(path$res$fig, "example-land-cover-section_new.png"), tmp$gg_lcs_example_new,
    width = 12, height = 8, dpi = 300
  )
  
  
})