
## Check stumps
# data_prep$stump
# View(data_prep$stump)


## Issue with height mixing m and cm. if h > 2 -> h / 100 to have in m
## Adding land cover section based on thiessen polygons

data_clean$stump <- data_prep$stump |>
  mutate(
    stump_height = as.numeric(stump_height),
    stump_height = if_else(stump_height > 2, stump_height / 100, stump_height),
    stump_x = cos((90 - stump_azimuth) * pi/180) * stump_distance,
    stump_y = sin((90 - stump_azimuth) * pi/180) * stump_distance,
    ## lcs_no based on circles
    # stump_lcs_no = case_when(
    #   stump_distance <= 8 ~ 1,
    #   stump_azimuth > 315 | stump_azimuth <=45  ~ 2,
    #   stump_azimuth >  45 & stump_azimuth <=135 ~ 3,
    #   stump_azimuth > 135 & stump_azimuth <=225 ~ 4,
    #   stump_azimuth > 225 & stump_azimuth <=315 ~ 5,
    #   TRUE ~ NA_integer_
    # ),
    ## lcs_no based on Thiessen polygons (equi-distance to LCS observation points)
    stump_lcs_no = case_when(
      abs(stump_x) <= 6 & abs(stump_y) <= 6 ~ 1,
      stump_azimuth > 315 | stump_azimuth <=45  ~ 2,
      stump_azimuth >  45 & stump_azimuth <=135 ~ 3,
      stump_azimuth > 135 & stump_azimuth <=225 ~ 4,
      stump_azimuth > 225 & stump_azimuth <=315 ~ 5,
      TRUE ~ NA_integer_
    )
  )
  
## Check
summary(as.numeric(data_prep$stump$stump_height))
summary(as.numeric(data_clean$stump$stump_height))
