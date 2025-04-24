

## !!! FOR TESTING ONLY
# tree_bkp <- tree
# rm(tree_bkp)
## !!!

tmp <- list()

## Extract codes from subplot table
tmp$subplot_codes <- subplot |> select(ONA_index, subplot_plot_no, subplot_no, subplot_lc_class_center)

tree <- tree |>
  left_join(tmp$subplot_codes, by = join_by(ONA_parent_index == ONA_index)) |>
  left_join(data_clean$lcs, by = join_by(subplot_plot_no == lcs_plot_no, subplot_no == lcs_subplot_no, tree_lcs_no == lcs_no)) |>
  mutate(
    lcs_class_num = case_when(
      lcs_class == "16AC" ~ "161",
      lcs_class == "16EC" ~ "162",
      lcs_class == "16PN" ~ "163",
      lcs_class == "16RB" ~ "164",
      lcs_class == "16TK" ~ "165",
      lcs_class == "16OTH" ~ "169",
      TRUE ~ lcs_class
    ),
    lcs_class_num = as.numeric(lcs_class_num)
  ) |>
  left_join(anci$lc, by = join_by(lcs_class_num == lc_no)) |>
  rename_with(.cols = starts_with("lc_"), str_replace, pattern = "lc_", replacement = "lcs_")


## Check for NAs
table(tree$lcs_class, useNA = "ifany")
table(tree$lcs_class_num, useNA = "ifany")
table(tree$lcs_code_new, useNA = "ifany")
table(tree$lcs_type, useNA = "ifany")

## Check NAs
tmp$check_na <- tree |>
  filter(is.na(lcs_class)) |>
  select(ONA_parent_index, subplot_plot_no, subplot_no, tree_lcs_no)

unique(tmp$check_na$ONA_parent_index)

## Checks 
## Original split of Land cover sections
tmp$lcs_location <- data.frame(x = c(0, 0, 12, 0, -12), y = c(0, 12, 0, -12, 0))
tmp$NE_line <- data.frame(x1 = 8 * cos(pi/4)  , y1 = 8 * sin(pi/4)  , x2 = 16 * cos(pi/4)  , y2 = 16 * sin(pi/4))
tmp$NW_line <- data.frame(x1 = 8 * cos(3*pi/4), y1 = 8 * sin(3*pi/4), x2 = 16 * cos(3*pi/4), y2 = 16 * sin(3*pi/4))
tmp$SW_line <- data.frame(x1 = 8 * cos(5*pi/4), y1 = 8 * sin(5*pi/4), x2 = 16 * cos(5*pi/4), y2 = 16 * sin(5*pi/4))
tmp$SE_line <- data.frame(x1 = 8 * cos(7*pi/4), y1 = 8 * sin(7*pi/4), x2 = 16 * cos(7*pi/4), y2 = 16 * sin(7*pi/4))

tmp$gg_tree <- tree |> filter(ONA_parent_index == 2)

tmp$gg_lcs_example <- ggplot(tmp$gg_tree) +
  gg_showplot(center = c(0, 0), vec_radius = c(16), n = 100) +
  gg_showplot(center = c(0, 0), vec_radius = c(8), n = 100) +
  geom_segment(data = tmp$NE_line, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(data = tmp$NW_line, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(data = tmp$SW_line, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(data = tmp$SE_line, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_point(data = tmp$lcs_location, aes(x = x, y = y), size = 2, shape = 3) +
  geom_point(data = tmp$lcs_location, aes(x = x, y = y), size = 2, shape = 21) +
  geom_point(aes(x = tree_x, y = tree_y, color = as.character(tree_lcs_no))) +
  geom_text_repel(aes(x = tree_x, y = tree_y, label = paste0(tree_distance, "/", tree_azimuth), color = as.character(tree_lcs_no)), size = 4) +
  theme(legend.position = "none") +
  coord_fixed() +
  labs(
    subtitle = paste0("Subplot code: ", unique(tmp$gg_tree$subplot_plot_no), unique(tmp$gg_tree$subplot_no)),
    x = "",
    y = "",
    caption = "Labels: distance/azimuth"
  )

print(tmp$gg_lcs_example)

ggsave(
  file.path(path$res$fig, "example-land-cover-section.png"), tmp$gg_lcs_example,
  width = 12, height = 8, dpi = 300
)


## New split of Land cover sections
tmp$lcs_location <- data.frame(x = c(0, 0, 12, 0, -12), y = c(0, 12, 0, -12, 0))
tmp$NE_line <- data.frame(x1 =  6, y1 =  6, x2 = 16 * cos(pi/4)  , y2 = 16 * sin(pi/4))
tmp$NW_line <- data.frame(x1 = -6, y1 =  6, x2 = 16 * cos(3*pi/4), y2 = 16 * sin(3*pi/4))
tmp$SW_line <- data.frame(x1 = -6, y1 = -6, x2 = 16 * cos(5*pi/4), y2 = 16 * sin(5*pi/4))
tmp$SE_line <- data.frame(x1 =  6, y1 = -6, x2 = 16 * cos(7*pi/4), y2 = 16 * sin(7*pi/4))

tmp$gg_tree <- tree |> filter(ONA_parent_index == 2)

tmp$gg_lcs_example_new <- ggplot(tmp$gg_tree) +
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
  geom_text_repel(aes(x = tree_x, y = tree_y, label = paste0(tree_distance, "/", tree_azimuth), color = as.character(tree_lcs_no_new)), size = 4) +
  theme(legend.position = "none") +
  coord_fixed() +
  labs(
    subtitle = paste0("Subplot code: ", unique(tmp$gg_tree$subplot_plot_no), unique(tmp$gg_tree$subplot_no)),
    x = "",
    y = "",
    caption = "Labels: distance/azimuth"
  )

print(tmp$gg_lcs_example_new)

ggsave(
  file.path(path$res$fig, "example-land-cover-section_new.png"), tmp$gg_lcs_example_new,
  width = 12, height = 8, dpi = 300
)

## save table in 'results'
write_csv(tree, file.path(path$res$data, "tree_with_lcs.csv"))  

## Remove tmp object
rm(tmp)