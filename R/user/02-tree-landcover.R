

tmp <- list()

## Extract codes from subplot table
tmp$subplot_codes <- subplot |>
  select(ONA_index, subplot_plot_no, subplot_no, subplot_lc_class_center)

length(unique(subplot$ONA_index))
nrow(subplot)
length(unique(tree$ONA_parent_index))

tree2 <- tree |>
  mutate(
    tree_x = cos((90 - tree_azimuth) * pi/180) * tree_distance,
    tree_y = sin((90 - tree_azimuth) * pi/180) * tree_distance,
    tree_lcs_no = case_when(
      tree_distance <= 8 ~ 1,
      tree_azimuth > 315 | tree_azimuth <=45  ~ 2,
      tree_azimuth >  45 & tree_azimuth <=90  ~ 3,
      tree_azimuth >  90 & tree_azimuth <=225 ~ 4,
      tree_azimuth > 225 & tree_azimuth <=315 ~ 5,
      TRUE ~ NA_integer_
    ),
    tree_lcs_no_new = case_when(
      abs(tree_x) <= 6 & abs(tree_y) <= 6 ~ 1,
      tree_azimuth > 315 | tree_azimuth <=45  ~ 2,
      tree_azimuth >  45 & tree_azimuth <=90  ~ 3,
      tree_azimuth >  90 & tree_azimuth <=225 ~ 4,
      tree_azimuth > 225 & tree_azimuth <=315 ~ 5,
      TRUE ~ NA_integer_
    )
  ) |>
  left_join(tmp$subplot_codes, by = join_by(ONA_parent_index == ONA_index)) |>
  left_join(data_clean$lcs, by = join_by(subplot_plot_no == lcs_plot_no, subplot_no == lcs_subplot_no, tree_lcs_no == lcs_no))


table(tree2$lcs_class, useNA = "ifany")

tmp$lcs_check <- lcs |>
  mutate(lcs_id = paste0(lcs_plot_no, lcs_subplot_no, lcs_no))

length(unique(tmp$lcs_check$lcs_id))
nrow(lcs)

## Checks 
## Original split of Land cover sections
tmp$lcs_location <- data.frame(x = c(0, 0, 12, 0, -12), y = c(0, 12, 0, -12, 0))
tmp$NE_line <- data.frame(x1 = 8 * cos(pi/4)  , y1 = 8 * sin(pi/4)  , x2 = 16 * cos(pi/4)  , y2 = 16 * sin(pi/4))
tmp$NW_line <- data.frame(x1 = 8 * cos(3*pi/4), y1 = 8 * sin(3*pi/4), x2 = 16 * cos(3*pi/4), y2 = 16 * sin(3*pi/4))
tmp$SW_line <- data.frame(x1 = 8 * cos(5*pi/4), y1 = 8 * sin(5*pi/4), x2 = 16 * cos(5*pi/4), y2 = 16 * sin(5*pi/4))
tmp$SE_line <- data.frame(x1 = 8 * cos(7*pi/4), y1 = 8 * sin(7*pi/4), x2 = 16 * cos(7*pi/4), y2 = 16 * sin(7*pi/4))

data_clean$tree |>
  filter(ONA_parent_index == 2) |>
  ggplot() +
  gg_showplot(center = c(0, 0), vec_radius = c(16), n = 100) +
  gg_showplot(center = c(0, 0), vec_radius = c(8), n = 100) +
  geom_segment(data = tmp$NE_line, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(data = tmp$NW_line, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(data = tmp$SW_line, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(data = tmp$SE_line, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_point(data = tmp$lcs_location, aes(x = x, y = y), size = 2, shape = 3) +
  geom_point(data = tmp$lcs_location, aes(x = x, y = y), size = 2, shape = 21) +
  geom_point(aes(x = tree_x, y = tree_y, color = as.character(tree_lcs_no))) +
  geom_text_repel(aes(x = tree_x, y = tree_y, label = tree_azimuth, color = as.character(tree_lcs_no))) +
  theme(legend.position = "none") +
  coord_fixed()

## New split of Land cover sections
tmp$lcs_location <- data.frame(x = c(0, 0, 12, 0, -12), y = c(0, 12, 0, -12, 0))
tmp$NE_line <- data.frame(x1 =  6, y1 =  6, x2 = 16 * cos(pi/4)  , y2 = 16 * sin(pi/4))
tmp$NW_line <- data.frame(x1 = -6, y1 =  6, x2 = 16 * cos(3*pi/4), y2 = 16 * sin(3*pi/4))
tmp$SW_line <- data.frame(x1 = -6, y1 = -6, x2 = 16 * cos(5*pi/4), y2 = 16 * sin(5*pi/4))
tmp$SE_line <- data.frame(x1 =  6, y1 = -6, x2 = 16 * cos(7*pi/4), y2 = 16 * sin(7*pi/4))

data_clean$tree |>
  filter(ONA_parent_index == 2) |>
  ggplot() +
  gg_showplot(center = c(0, 0), vec_radius = c(16), n = 100) +
  gg_showplot(center = c(0, 0), vec_radius = c(8), n = 100, color = "grey60") +
  geom_path(data = data.frame(x = c(-6, -6, 6, 6, -6), y = c(-6, 6, 6, -6, -6)), aes(x, y)) +
  geom_segment(data = tmp$NE_line, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(data = tmp$NW_line, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(data = tmp$SW_line, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(data = tmp$SE_line, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_point(data = tmp$lcs_location, aes(x = x, y = y), size = 2, shape = 3) +
  geom_point(data = tmp$lcs_location, aes(x = x, y = y), size = 2, shape = 21) +
  geom_point(aes(x = tree_x, y = tree_y, color = as.character(tree_lcs_no_new))) +
  geom_text_repel(aes(x = tree_x, y = tree_y, label = tree_azimuth, color = as.character(tree_lcs_no_new))) +
  theme(legend.position = "none") +
  coord_fixed()
  