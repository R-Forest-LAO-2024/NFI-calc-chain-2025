
## Add land cover section based on:
## - assigned supblot section circles (tree_lcs_no)
## - equidistance between observation points (tree_lcs_no_new)
tree <- tree |>
  mutate(
    tree_x = cos((90 - tree_azimuth) * pi/180) * tree_distance,
    tree_y = sin((90 - tree_azimuth) * pi/180) * tree_distance,
    ## lcs_no based on circles
    # tree_lcs_no = case_when(
    #   tree_distance <= 8 ~ 1,
    #   tree_azimuth > 315 | tree_azimuth <=45  ~ 2,
    #   tree_azimuth >  45 & tree_azimuth <=135 ~ 3,
    #   tree_azimuth > 135 & tree_azimuth <=225 ~ 4,
    #   tree_azimuth > 225 & tree_azimuth <=315 ~ 5,
    #   TRUE ~ NA_integer_
    # ),
    ## lcs_no based on Thiessen polygons (equi-distance to LCS observation points)
    tree_lcs_no = case_when(
      abs(tree_x) <= 6 & abs(tree_y) <= 6 ~ 1,
      tree_azimuth > 315 | tree_azimuth <=45  ~ 2,
      tree_azimuth >  45 & tree_azimuth <=135 ~ 3,
      tree_azimuth > 135 & tree_azimuth <=225 ~ 4,
      tree_azimuth > 225 & tree_azimuth <=315 ~ 5,
      TRUE ~ NA_integer_
    )
  )


