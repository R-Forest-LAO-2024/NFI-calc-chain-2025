
## AGB of stump is cylinder volume * wd (0.57): AGB = pi * (mean_d/200)^2 * height * wd
stump <- stump |>
  mutate(
    stump_v_m3 = round(pi * (stump_diameter_mean/200)^2 * stump$stump_height, 3),
    stump_agb =  stump_v_m3 * 0.57 * 1000 ## 1000 kg/m3 = 1 g/cm3
  )
