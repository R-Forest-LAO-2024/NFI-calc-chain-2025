
## Initiate list to store temporary objects
tmp <- list()

## Read ancillary plot location to fill gaps when tablet couldn't record GPS.
length(unique(anci$treeplot_plan$treeplot_id))
length(unique(anci$treeplot_plan$treeplot_plot_no))

## Prepare treeplot ####
## + Remove QAQC plots
## + 
tmp$treeplot <- data_init$treeplot_init |>
  filter(plot_info__crew_lead != "QC") |>
  mutate(
    treeplot_plot_no = as.numeric(plot_info__plot_code_nmbr),
    treeplot_plot_id =  case_when(
      treeplot_plot_no < 10 ~ paste0("00", treeplot_plot_no),
      treeplot_plot_no < 100 ~ paste0("0", treeplot_plot_no),
      TRUE ~ as.character(treeplot_plot_no)
    ),
    treeplot_no = plot_info__sub_plot,
    treeplot_id = paste0(treeplot_plot_id, treeplot_no),
    treeplot_crew_lead = as.numeric(plot_info__crew_lead),
    treeplot_survey_time = as.numeric(survey_time),
    treeplot_time_start = as_datetime(plot_info__rec_time_start), 
    treeplot_time_end = as_datetime(rec_time_end),
    treeplot_date_measure = as_date(today),
    treeplot_gps_lon = as.numeric(plot_GPS___GPS_longitude),
    treeplot_gps_lat = as.numeric(plot_GPS___GPS_latitude),
    treeplot_gps_alt = as.numeric(plot_GPS___GPS_altitude),
    treeplot_gps_precision = as.numeric(plot_GPS___GPS_precision),
    treeplot_access = case_when(
      access__access_reason__slope      == "True" ~ "slope",
      access__access_reason__danger     == "True" ~ "danger",
      access__access_reason__distance   == "True" ~ "distance",
      access__access_reason__prohibited == "True" ~ "prohibited",
      access__access_reason__other      == "True" ~ "other",
      TRUE ~ "accessible"
    ),
    treeplot_access_rk = if_else(treeplot_access == "other", access__manual_reason, NA_character_),
    treeplot_lc_class = survey__lc__lc_class__lc_class1,
    treeplot_lc_type = survey__lc__lc_data__lc_type,
    treeplot_canopy_height = survey__lc__canopy__avg_height,
    treeplot_canopy_cover = survey__lc__canopy__can_cov
  ) |>
  select(ONA_treeplot_id = ONA_index, starts_with("treeplot_")) |>
  mutate(
    treeplot_no = case_when(
      ONA_treeplot_id == 109 ~ "B",
      ONA_treeplot_id == 113 ~ "B",
      ONA_treeplot_id == 265 ~ "C",
      ONA_treeplot_id == 440 ~ "C",
      ONA_treeplot_id == 980 ~ "B",
      TRUE ~ treeplot_no
    ),
    treeplot_plot_no = case_when(
      ONA_treeplot_id == 1269 ~ 333,
      ONA_treeplot_id == 1270 ~ 333,
      ONA_treeplot_id == 1271 ~ 333,
      ONA_treeplot_id == 1272 ~ 333,
      TRUE ~ treeplot_plot_no
    ),
    treeplot_plot_id = case_when(
      treeplot_plot_no < 10 ~ paste0("000", treeplot_plot_no),
      treeplot_plot_no < 100 ~ paste0("00", treeplot_plot_no),
      treeplot_plot_no < 1000 ~ paste0("0", treeplot_plot_no),
      TRUE ~ as.character(treeplot_plot_no)
    ),
    treeplot_id = paste0(treeplot_plot_id, treeplot_no)
  )

## Solve duplicates in treeplot table ####
tmp$n_treeplot <- length(unique(tmp$treeplot$treeplot_id))
message("Unique treeplot IDs: ", tmp$n_treeplot == nrow(tmp$treeplot))

if (tmp$n_treeplot != nrow(tmp$treeplot)) {
  
  tmp$check_dup <- tmp$treeplot |>
    group_by(treeplot_id) |>
    summarise(count = n()) |>
    filter(count > 1)
  
  tt <- tmp$check_dup_index <- tmp$treeplot |>
    filter(treeplot_id %in% tmp$check_dup$treeplot_id)
  
  ## !!! TEMPORARY RULE: Solve duplicates by keeping records that took the longest time
  tmp$max_duration <- tmp$treeplot |>
    group_by(treeplot_id) |>
    summarise(max_survey_time = max(treeplot_survey_time))
  
  tmp$treeplot1 <- tmp$treeplot |> 
    left_join(tmp$max_duration, by = "treeplot_id") |>
    filter(treeplot_survey_time == max_survey_time) |>
    select(-max_survey_time)
  
  message("Unique treeplot IDs after corr: ", tmp$n_treeplot == nrow(tmp$treeplot1))
  
} else {
  
  tmp$treeplot1 <- tmp$treeplot
  
}

# check missing tp
table(tmp$treeplot1$treeplot_no)

tmp$check_missing_tp <- tmp$treeplot1 |>
  group_by(treeplot_plot_no) |>
  summarise(count = n())

table(tmp$check_missing_tp$count, useNA = "ifany")

tmp$vec_missing_tp <- tmp$check_missing_tp |> filter(count == 1)

## !!! TO CONTINUE LATER
## TO CHECK 54 147 149 198 363 414 627 827 (see harmo-plot for vec of plot IDs)
# tt <- tmp$treeplot1
# tt2 <- tmp$treeplot1 |> filter(treeplot_plot_no == 149) |> select(ONA_treeplot_id, treeplot_id)

## !!! NEED TO MANUALLY CORRECT PLOT ID when sum of treeplot is not 4
tmp$treeplot1.1 <- tmp$treeplot1 |>
  mutate(
    treeplot_plot_no = case_when(
      treeplot_id == "054C" ~ 554,
      treeplot_id == "149D" ~ 419,
      TRUE ~ treeplot_plot_no
    ),
    ## RECALC plot_id, treeplot_id
    treeplot_plot_id = case_when(
      treeplot_plot_no < 10 ~ paste0("00", treeplot_plot_no),
      treeplot_plot_no < 100 ~ paste0("0", treeplot_plot_no),
      TRUE ~ as.character(treeplot_plot_no)
    ),
    treeplot_id = paste0(treeplot_plot_id, treeplot_no)
  )


## Check for missing GPS 
tmp$check_gps <- tmp$treeplot1 |>
  filter(is.na(treeplot_gps_lon) | is.na(treeplot_gps_lat)) |>
  pull(treeplot_id)

## Use planned coords for missing coords
tmp$treeplot2 <- tmp$treeplot1.1 |>
  left_join(anci$treeplot_plan, by = c("treeplot_plot_no", "treeplot_id", "treeplot_no")) |>
  mutate(
    treeplot_coord_lon    = if_else(is.na(treeplot_gps_lon), treeplot_plan_lon, treeplot_gps_lon),
    treeplot_coord_lat    = if_else(is.na(treeplot_gps_lat), treeplot_plan_lat, treeplot_gps_lat),
    treeplot_coord_source = if_else(is.na(treeplot_gps_lon)|is.na(treeplot_gps_lat), "plan", "gps")
    )

table(tmp$treeplot2$treeplot_coord_source, useNA = 'ifany')
summary(tmp$treeplot2$treeplot_coord_lon)

## Add meter coordinates
tmp$sf_treeplot_wgs84 <- tmp$treeplot2 |>
  mutate(treeplot_coord_lon2 = treeplot_coord_lon, treeplot_coord_lat2 = treeplot_coord_lat) |>
  st_as_sf(coords = c("treeplot_coord_lon2", "treeplot_coord_lat2"), crs = 4326)

tmp$sf_treeplot_m <- st_transform(tmp$sf_treeplot_wgs84, crs = "ESRI:54017")

tmp$coords_m <- st_coordinates(tmp$sf_treeplot_m)

data_harmo$treeplot <- tmp$sf_treeplot_m |>
  mutate(
    treeplot_coord_x = tmp$coords_m[,1],
    treeplot_coord_y = tmp$coords_m[,2],
    treeplot_coord_xy_crs = "ESRI:54017"
    ) |>
  as_tibble() |>
  select(-geometry)





## Checks 
# tmp$sf_treeplot_wgs84 |>
#   ggplot() + 
#   geom_sf(aes(color = treeplot_access)) +
#   scale_color_viridis_d() +
#   labs(color = "")

table(data_harmo$treeplot$treeplot_no, useNA = "ifany")

## Visual Check
res$gg_cumplot <- data_harmo$treeplot |>
  select(treeplot_date_measure, treeplot_plot_id, treeplot_crew_lead) |>
  distinct() |>
  arrange(treeplot_date_measure, treeplot_plot_id) |>
  group_by(treeplot_crew_lead) |>
  mutate(cum_plot = row_number()) |>
  ggplot() +
  geom_line(aes(x = treeplot_date_measure, y = cum_plot, color = as.character(treeplot_crew_lead))) +
  #facet_wrap(~crew_lead, nrow = 4) +
  #theme(legend.position = "bottom") +
  guides(x = guide_axis(n.dodge = 2)) +
  scale_color_viridis_d() +
  labs(color = "Crew")

## Remove tmp objects 
rm(tmp)