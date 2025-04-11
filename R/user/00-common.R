
## Functions ####

## + Make plot circles in ggplots ####
gg_showplot <- function(center, vec_radius, n){
  
  theta = seq(0, 2*pi, length = n)
  
  map(vec_radius, function(r){
    data_circle <- data.frame(
      theta = theta,
      x = center[1] + r * cos(theta),
      y = center[2] + r * sin(theta)
    )
    
    geom_path(data = data_circle, aes(x = x, y = y))
    
  })
  
}


## Objects ####

## + Create a AGB models parameter table ####
agb_models <- tibble(
  lc_class = c("EF", "DD", "MDF", "CF", "MCB"),
  agb_equation = rep("AGB = a * DBH^b", 5),
  param_a = c(0.3112, 0.2137, 0.523081, 0.1277, 0.1277),
  param_b = c(2.2331, 2.2575, 2       , 2.3944, 2.3944)
)

## + Carbon fraction ####
CF <- 0.47


## + Create ggplot treeplot circles elements at the position of each treeplot in a plot
gg_treeplot_center <- gg_showplot(center = c(0, 0), vec_radius = c(8, 16), n = 100)

gg_treeplot_all <- list(
  ## A
  gg_treeplot_center,
  ## B
  gg_showplot(center = c(0, 60),vec_radius = c(8, 16), n = 100),
  ## C
  gg_showplot(center = c(0, 120),vec_radius = c(8, 16), n = 100),
  ## D
  gg_showplot(center = c(0, 180),vec_radius = c(8, 16), n = 100),
  ## E
  gg_showplot(center = c(60, 0),vec_radius = c(8, 16), n = 100),
  ## F
  gg_showplot(center = c(120, 0),vec_radius = c(8, 16), n = 100),
  ## G
  gg_showplot(center = c(180, 0),vec_radius = c(8, 16), n = 100),
  coord_fixed(),
  theme_bw()
)