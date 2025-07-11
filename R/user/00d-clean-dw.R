

## Standing deadwood recorded with the live tree data

tmp <- list()


## Checks
# data_prep$dw
# names(data_prep$dw)
# table(data_prep$dw$dw_class)

tmp$gg_prep <- data_prep$dw |>
  filter(dw_class == "decay 2 tall") |>
  ggplot(aes(y = dw_bole_height)) +
  geom_segment(aes(yend = dw_bole_height, x = dw_dbase, xend = dw_dtop), col = "grey40") +
  geom_point(aes(x = dw_dbase), size = 1) +
  geom_point(aes(x = dw_dtop), col = "darkred", size = 1) +
  labs(
    x = "D base (black) / D top (red) in cm",
    y = "Bole height in m",
    subtitle = "Top diameter correction: before (left) - after (right)"
  )
  


## 
## Make clean table ####
##

## Solve dw_top < 0 in case DBH > DBASE
## Add land cover section based on thiessen polygons

data_clean$dw <- data_prep$dw |>
  mutate(
    dw_dtop = if_else(dw_dtop < 0, 0, round(dw_dtop, 1)),
    dw_x = cos((90 - dw_azimuth) * pi/180) * dw_distance,
    dw_y = sin((90 - dw_azimuth) * pi/180) * dw_distance,
    ## lcs_no based on circles
    # dw_lcs_no = case_when(
    #   dw_distance <= 8 ~ 1,
    #   dw_azimuth > 315 | dw_azimuth <=45  ~ 2,
    #   dw_azimuth >  45 & dw_azimuth <=135 ~ 3,
    #   dw_azimuth > 135 & dw_azimuth <=225 ~ 4,
    #   dw_azimuth > 225 & dw_azimuth <=315 ~ 5,
    #   TRUE ~ NA_integer_
    # ),
    ## lcs_no based on Thiessen polygons (equi-distance to LCS observation points)
    dw_lcs_no = case_when(
      abs(dw_x) <= 6 & abs(dw_y) <= 6 ~ 1,
      dw_azimuth > 315 | dw_azimuth <=45  ~ 2,
      dw_azimuth >  45 & dw_azimuth <=135 ~ 3,
      dw_azimuth > 135 & dw_azimuth <=225 ~ 4,
      dw_azimuth > 225 & dw_azimuth <=315 ~ 5,
      TRUE ~ NA_integer_
    )
  )


## Cross-check
# View(data_clean$dw)

tmp$gg_clean <- data_clean$dw |>
  filter(dw_class == "decay 2 tall") |>
  ggplot(aes(y = dw_bole_height)) +
  geom_segment(aes(yend = dw_bole_height, x = dw_dbase, xend = dw_dtop), col = "grey40") +
  geom_point(aes(x = dw_dbase), size = 1) +
  #geom_point(aes(x = dw_dbh), col = "lightgreen", size = 1) +
  geom_point(aes(x = dw_dtop), col = "darkred", size = 1) +
  labs(
    x = "", 
    y = ""
  )

tmp$gg_corr <- ggpubr::ggarrange(tmp$gg_prep, tmp$gg_clean, align = "hv")
print(tmp$gg_corr)

ggsave(
  tmp$gg_corr, filename = "results/figures/dw-dtop-correction.png", 
  width = 15, height = 12, units = "cm", dpi = 300
  )

rm(tmp)
