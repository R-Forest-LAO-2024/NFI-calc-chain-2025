
## Initiate list to store temporary objects
tmp <- list()

## Prepare subplot ####
# + Rename columns
# + Group columns that have information spread over multiple columns
# + Keep only columns used for analysis for now 


tmp$subplot <- data_init$subplot_init |>
  mutate(
    subplot_plot_no = as.numeric(plot_info__plot_code_nmbr),
    subplot_plot_id =  case_when(
      subplot_plot_no < 10 ~ paste0("00", subplot_plot_no),
      subplot_plot_no < 100 ~ paste0("0", subplot_plot_no),
      TRUE ~ as.character(subplot_plot_no)
    ),
    subplot_no = plot_info__sub_plot,
    subplot_id = paste0(subplot_plot_id, subplot_no),
    subplot_crew_lead = plot_info__crew_lead,
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
    subplot_lc_class_center = survey__lc__lc_class__lc_class1,
    subplot_lc_type_center = survey__lc__lc_data__lc_type,
    subplot_canopy_height = survey__lc__canopy__avg_height,
    subplot_canopy_cover = survey__lc__canopy__can_cov,
    subplot_lc_class_north = survey__lc__lc_class_north__lc_class2,
    subplot_lc_type_north = survey__lc__lc_data_north__lc_type2,
    subplot_lc_class_east = survey__lc__lc_class_east__lc_class3,
    subplot_lc_type_east = survey__lc__lc_data_east__lc_type3,
    subplot_lc_class_south = survey__lc__lc_class_south__lc_class4,
    subplot_lc_type_south = survey__lc__lc_data_south__lc_type4,
    subplot_lc_class_west = survey__lc__lc_class_west__lc_class5,
    subplot_lc_type_west = survey__lc__lc_data_west__lc_type5
  ) |>
  select(ONA_index, starts_with("subplot_"))


data_prep$subplot <- tmp$subplot |>
  filter(subplot_crew_lead != "QC") |>
  mutate(subplot_crew_lead = as.numeric(subplot_crew_lead))

data_prep$subplot_qc <- tmp$subplot |>
  filter(subplot_crew_lead == "QC")

rm(tmp)
