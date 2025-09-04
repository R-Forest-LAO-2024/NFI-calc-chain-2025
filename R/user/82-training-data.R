

##
## Make subplot table ####
##

## 3 mistakes left uncorrected

training_subplot <- data_prep$subplot |>
  filter(!ONA_index %in% c(1819, 2237, 2245)) |>
  mutate(
    subplot_no = case_when(
      #ONA_index == 109 ~ "B", ## 631C is actually 631B
      #ONA_index == 113 ~ "B", ## 632C is actually 632B
      #ONA_index == 265 ~ "C", ## 553D is actually 553C
      ONA_index == 440 ~ "C", ## 336B is actually 336C
      ONA_index == 980 ~ "B", ## 232C is actually 232B
      ONA_index == 2125 ~ "C", ## issue with 620B duplicate, so B becomes C and C becomes D
      ONA_index == 2126 ~ "D",
      ONA_index ==  103 ~ "C", ## issue with 627B
      TRUE ~ subplot_no
    ),
    plot_no = case_when(
      ONA_index == 1269 ~ 333, ## plot_no typo, should be 333
      ONA_index == 1270 ~ 333,
      ONA_index == 1271 ~ 333,
      ONA_index == 1272 ~ 333,
      ONA_index == 1417 ~ 359, ## plot_no 369(B) is actually 359(B)
      subplot_crew_lead == 2 & ONA_index ==  102 ~ 636, # subplot_id == "363B"
      subplot_crew_lead == 4 & ONA_index == 1135 ~ 168, # subplot_id == "198D"
      subplot_crew_lead == 6 & ONA_index ==  270 ~ 554, # subplot_id == "054C"
      subplot_crew_lead == 6 & ONA_index == 1069 ~ 414, # subplot_id == "141A"  
      subplot_crew_lead == 8 & ONA_index ==  819 ~ 419, # subplot_id == "149D"
      TRUE ~ subplot_plot_no
    ),
    ## Recalc subplot ID
    plot_id = case_when(
      subplot_plot_no < 10 ~ paste0("00", subplot_plot_no),
      subplot_plot_no < 100 ~ paste0("0", subplot_plot_no),
      TRUE ~ as.character(subplot_plot_no)
    ),
    subplot_id = paste0(subplot_plot_id, subplot_no)
  ) |>
  select(
    ONA_index, plot_no, subplot_no, subplot_id, subplot_crew_lead, subplot_time_start, subplot_time_end, subplot_date_measure, 
    subplot_gps_lon,	subplot_gps_lat, subplot_gps_alt, subplot_gps_precision, subplot_access,
    subplot_lc_class_center, subplot_lc_type_center, subplot_canopy_height, subplot_canopy_cover
    )


## Check
tt_dup <- training_subplot |>
  group_by(subplot_id) |>
  summarise(count = n(), .groups = "drop") |>
  filter(count > 1) |>
  pull(subplot_id)
tt_dup


##
## Other tables ####
##

training_lcs <- lcs_ |> 
  select(plot_no = lcs_plot_no, subplot_no = lcs_subplot_no, lcs_no, lcs_name, lc_no = lcs_lu_class_no, lc_code = lu_code_new) 

training_tree <- tree |>
  select(
    plot_no = subplot_plot_no, subplot_no, lcs_no = tree_lcs_no, tree_no, tree_stem_no, tree_dbh, 
    tree_species_code, tree_species_binomial, tree_distance, tree_azimuth, tree_x, tree_y
    )

training_anci_plotE <- anci$plot_chaveE

training_anci_ph1 <- ph1_data

training_anci_ph2 <- ph2_subplot

##
## Save tables ####
##

write_csv(training_subplot, file.path(path$res$test, "training_subplot.csv"))
write_csv(training_lcs    , file.path(path$res$test, "training_lcs.csv"))
write_csv(training_tree , file.path(path$res$test, "training_tree.csv"))
write_csv(training_anci_plotE , file.path(path$res$test, "training_anci_plotE.csv"))
write_csv(training_anci_ph1 , file.path(path$res$test, "training_anci_phase1.csv"))
write_csv(training_anci_ph2 , file.path(path$res$test, "training_anci_phase2.csv"))


