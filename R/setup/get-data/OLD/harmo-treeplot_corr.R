
## Initiate list to store temporary objects
tmp <- list()

## Read ancillary plot location to fill gaps when tablet couldn't record GPS.
length(unique(anci$subplot_plan$subplot_id))
nrow(anci$subplot_plan)
tmp$plan_nplot <- length(unique(anci$subplot_plan$subplot_plot_no))

## Compare with data 
nrow(data_init$subplot_init)
tmp$measured_nplot <- length(unique(data_init$subplot_init$plot_info__plot_code_nmbr))

message("NFI plot measurement percentage: ", round(tmp$measured_nplot / tmp$plan_nplot * 100), "%")

##
## Manual corrections of wrong plot and subplot codes #####
##

names(data_init$subplot_init)
summary(data_init$subplot_init$ONA_index)

table(data_init$subplot_init$plot_info__sub_plot, useNA = "ifany")

tmp$subplot1 <- data_init$subplot_init |>
  mutate(
    subplot_no = case_when(
      ONA_subplot_id == 109 ~ "B",
      ONA_subplot_id == 113 ~ "B",
      ONA_subplot_id == 265 ~ "C",
      ONA_subplot_id == 440 ~ "C",
      ONA_subplot_id == 980 ~ "B",
      TRUE ~ subplot_no
    ),
    subplot_plot_no = case_when(
      ONA_subplot_id == 1269 ~ 333,
      ONA_subplot_id == 1270 ~ 333,
      ONA_subplot_id == 1271 ~ 333,
      ONA_subplot_id == 1272 ~ 333,
      TRUE ~ subplot_plot_no
    )
  )


## Prepare subplot ####
# + Rename columns
# + 
tmp$subplot <- data_init$subplot_init |>
  # filter(plot_info__crew_lead != "QC") |>
  mutate(
    subplot_plot_no = as.numeric(plot_info__plot_code_nmbr),
    subplot_plot_id =  case_when(
      subplot_plot_no < 10 ~ paste0("00", subplot_plot_no),
      subplot_plot_no < 100 ~ paste0("0", subplot_plot_no),
      TRUE ~ as.character(subplot_plot_no)
    ),
    subplot_no = plot_info__sub_plot,
    subplot_id = paste0(subplot_plot_id, subplot_no),
    subplot_crew_lead = as.numeric(plot_info__crew_lead),
    subplot_survey_time = as.numeric(survey_time),
    subplot_time_start = as_datetime(plot_info__rec_time_start), 
    subplot_time_end = as_datetime(rec_time_end),
    subplot_date_measure = as_date(today),
    subplot_gps_lon = as.numeric(plot_GPS___GPS_longitude),
    subplot_gps_lat = as.numeric(plot_GPS___GPS_latitude),
    subplot_gps_alt = as.numeric(plot_GPS___GPS_altitude),
    subplot_gps_precision = as.numeric(plot_GPS___GPS_precision),
    subplot_access = case_when(
      access__access_reason__slope      == "True" ~ "slope",
      access__access_reason__danger     == "True" ~ "danger",
      access__access_reason__distance   == "True" ~ "distance",
      access__access_reason__prohibited == "True" ~ "prohibited",
      access__access_reason__other      == "True" ~ "other",
      TRUE ~ "accessible"
    ),
    subplot_access_rk = if_else(subplot_access == "other", access__manual_reason, NA_character_),
    subplot_lc_class = survey__lc__lc_class__lc_class1,
    subplot_lc_type = survey__lc__lc_data__lc_type,
    subplot_canopy_height = survey__lc__canopy__avg_height,
    subplot_canopy_cover = survey__lc__canopy__can_cov
  ) |>
  select(ONA_subplot_id = ONA_index, starts_with("subplot_")) |>
  mutate(
    ,
    subplot_plot_id = case_when(
      subplot_plot_no < 10 ~ paste0("000", subplot_plot_no),
      subplot_plot_no < 100 ~ paste0("00", subplot_plot_no),
      subplot_plot_no < 1000 ~ paste0("0", subplot_plot_no),
      TRUE ~ as.character(subplot_plot_no)
    ),
    subplot_id = paste0(subplot_plot_id, subplot_no)
  )

## Solve duplicates in subplot table ####
tmp$n_subplot <- length(unique(tmp$subplot$subplot_id))
message("Unique subplot IDs: ", tmp$n_subplot == nrow(tmp$subplot))

if (tmp$n_subplot != nrow(tmp$subplot)) {
  
  tmp$check_dup <- tmp$subplot |>
    group_by(subplot_id) |>
    summarise(count = n()) |>
    filter(count > 1)
  
  tt <- tmp$check_dup_index <- tmp$subplot |>
    filter(subplot_id %in% tmp$check_dup$subplot_id)
  
  ## !!! TEMPORARY RULE: Solve duplicates by keeping records that took the longest time
  tmp$max_duration <- tmp$subplot |>
    group_by(subplot_id) |>
    summarise(max_survey_time = max(subplot_survey_time))
  
  tmp$subplot1 <- tmp$subplot |> 
    left_join(tmp$max_duration, by = "subplot_id") |>
    filter(subplot_survey_time == max_survey_time) |>
    select(-max_survey_time)
  
  message("Unique subplot IDs after corr: ", tmp$n_subplot == nrow(tmp$subplot1))
  
} else {
  
  tmp$subplot1 <- tmp$subplot
  
}

# check missing tp
table(tmp$subplot1$subplot_no)

tmp$check_missing_tp <- tmp$subplot1 |>
  group_by(subplot_plot_no) |>
  summarise(count = n())

table(tmp$check_missing_tp$count, useNA = "ifany")

tmp$vec_missing_tp <- tmp$check_missing_tp |> filter(count == 1)

## !!! TO CONTINUE LATER
## TO CHECK 54 147 149 198 363 414 627 827 (see harmo-plot for vec of plot IDs)
# tt <- tmp$subplot1
# tt2 <- tmp$subplot1 |> filter(subplot_plot_no == 149) |> select(ONA_subplot_id, subplot_id)

## !!! NEED TO MANUALLY CORRECT PLOT ID when sum of subplot is not 4
tmp$subplot1.1 <- tmp$subplot1 |>
  mutate(
    subplot_plot_no = case_when(
      subplot_id == "054C" ~ 554,
      subplot_id == "149D" ~ 419,
      TRUE ~ subplot_plot_no
    ),
    ## RECALC plot_id, subplot_id
    subplot_plot_id = case_when(
      subplot_plot_no < 10 ~ paste0("00", subplot_plot_no),
      subplot_plot_no < 100 ~ paste0("0", subplot_plot_no),
      TRUE ~ as.character(subplot_plot_no)
    ),
    subplot_id = paste0(subplot_plot_id, subplot_no)
  )


## Check for missing GPS 
tmp$check_gps <- tmp$subplot1 |>
  filter(is.na(subplot_gps_lon) | is.na(subplot_gps_lat)) |>
  pull(subplot_id)

## Use planned coords for missing coords
tmp$subplot2 <- tmp$subplot1.1 |>
  left_join(anci$subplot_plan, by = c("subplot_plot_no", "subplot_id", "subplot_no")) |>
  mutate(
    subplot_coord_lon    = if_else(is.na(subplot_gps_lon), subplot_plan_lon, subplot_gps_lon),
    subplot_coord_lat    = if_else(is.na(subplot_gps_lat), subplot_plan_lat, subplot_gps_lat),
    subplot_coord_source = if_else(is.na(subplot_gps_lon)|is.na(subplot_gps_lat), "plan", "gps")
    )

table(tmp$subplot2$subplot_coord_source, useNA = 'ifany')
summary(tmp$subplot2$subplot_coord_lon)

## Add meter coordinates
tmp$sf_subplot_wgs84 <- tmp$subplot2 |>
  mutate(subplot_coord_lon2 = subplot_coord_lon, subplot_coord_lat2 = subplot_coord_lat) |>
  st_as_sf(coords = c("subplot_coord_lon2", "subplot_coord_lat2"), crs = 4326)

tmp$sf_subplot_m <- st_transform(tmp$sf_subplot_wgs84, crs = "ESRI:54017")

tmp$coords_m <- st_coordinates(tmp$sf_subplot_m)

data_harmo$subplot <- tmp$sf_subplot_m |>
  mutate(
    subplot_coord_x = tmp$coords_m[,1],
    subplot_coord_y = tmp$coords_m[,2],
    subplot_coord_xy_crs = "ESRI:54017"
    ) |>
  as_tibble() |>
  select(-geometry)





## Checks 
# tmp$sf_subplot_wgs84 |>
#   ggplot() + 
#   geom_sf(aes(color = subplot_access)) +
#   scale_color_viridis_d() +
#   labs(color = "")

table(data_harmo$subplot$subplot_no, useNA = "ifany")

## Visual Check
res$gg_cumplot <- data_harmo$subplot |>
  select(subplot_date_measure, subplot_plot_id, subplot_crew_lead) |>
  distinct() |>
  arrange(subplot_date_measure, subplot_plot_id) |>
  group_by(subplot_crew_lead) |>
  mutate(cum_plot = row_number()) |>
  ggplot() +
  geom_line(aes(x = subplot_date_measure, y = cum_plot, color = as.character(subplot_crew_lead))) +
  #facet_wrap(~crew_lead, nrow = 4) +
  #theme(legend.position = "bottom") +
  guides(x = guide_axis(n.dodge = 2)) +
  scale_color_viridis_d() +
  labs(color = "Crew")

## Remove tmp objects 
rm(tmp)