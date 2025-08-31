
## Initiate list to store temporary objects
tmp <- list()

## Check progression of NFI
## Read ancillary plot location to fill gaps when tablet couldn't record GPS.
length(unique(anci$subplot_plan$subplot_id))
nrow(anci$subplot_plan)
tmp$plan_nplot <- length(unique(anci$subplot_plan$subplot_plot_no))

## Compare with data
length(unique(data_prep$subplot$subplot_id))
nrow(data_prep$subplot)
tmp$measured_nplot <- length(unique(data_prep$subplot$subplot_plot_no))

message("NFI plot measurement percentage: ", round(tmp$measured_nplot / tmp$plan_nplot * 100), "%")

##
## Manual corrections for duplicates in subplot codes #####
##

## + Check ####

# names(data_prep$subplot)
# summary(data_prep$subplot$ONA_index)
# 
# tmp$check_dup <- data_prep$subplot |>
#   summarise(count = n(), .by = c(subplot_plot_no, subplot_no))
# 
# table(tmp$check_dup$count, useNA = "ifany")
# 
# print(filter(tmp$check_dup, count > 1))
## >> Duplicates were data entry errors, need to manually edit the data


## + !!! Modify data following manual check of the duplicates ####

# summary(data_prep$subplot$subplot_plot_no)
# table(data_prep$subplot$subplot_no, useNA = "ifany")

tmp$subplot1 <- data_prep$subplot |>
  mutate(
    subplot_no = case_when(
      ONA_index == 109 ~ "B", ## 631C is actually 631B
      ONA_index == 113 ~ "B", ## 632C is actually 632B
      ONA_index == 265 ~ "C", ## 553D is actually 553C
      ONA_index == 440 ~ "C", ## 336B is actually 336C
      ONA_index == 980 ~ "B", ## 232C is actually 232B
      TRUE ~ subplot_no
    ),
    subplot_plot_no = case_when(
      ONA_index == 1269 ~ 333, ## plot_no typo, should be 333
      ONA_index == 1270 ~ 333,
      ONA_index == 1271 ~ 333,
      ONA_index == 1272 ~ 333,
      ONA_index == 1417 ~ 359, ## plot_no 369(B) is actually 359(B)
      TRUE ~ subplot_plot_no
    ),
    ## RECALC plot_id, subplot_id
    subplot_plot_id = case_when(
      subplot_plot_no < 10 ~ paste0("000", subplot_plot_no),
      subplot_plot_no < 100 ~ paste0("00", subplot_plot_no),
      subplot_plot_no < 1000 ~ paste0("0", subplot_plot_no),
      TRUE ~ as.character(subplot_plot_no)
    ),
    subplot_id = paste0(subplot_plot_id, subplot_no)
  )

## Check if problem solved
# tmp$subplot1 |>
#   summarise(count = n(), .by = c(subplot_plot_no, subplot_no)) |>
#   filter(count > 1) |> 
#   nrow()


##
## Manual corrections when more than 4 subplots per plot ####
##

## + CHECK ####
## >> Need manual checks of the 3/1 maybe wrong plot ID. 
## >> Could also be checked by distance to planed location 

tmp$check_sp <- tmp$subplot1 |>
  arrange(subplot_plot_no) |>
  summarise(count = n(), .by = subplot_plot_no)

table(tmp$check_sp$count, useNA = "ifany")

tmp$check_sp2 <- tmp$check_sp |> filter(count != 4)
nrow(tmp$check_sp2)


## Arrange subplots not counting to 4 per plot by crew_lead and time_start to find potential errors

tmp$check_sp2 <- tmp$subplot1 |> 
  filter(subplot_plot_no %in% pull(tmp$check_sp2, subplot_plot_no)) |>
  arrange(subplot_crew_lead, subplot_time_start) |>
  select(ONA_index, subplot_plot_no, subplot_no, subplot_crew_lead, subplot_time_start, subplot_time_end)

# View(tmp$check_sp2)


## + !!! Modify data to correct entry typos ####
tmp$subplot2 <- tmp$subplot1 |>
  mutate(
    subplot_plot_no = case_when(
      subplot_crew_lead == 2 & ONA_index ==  102 ~ 636, # subplot_id == "0363B"
      subplot_crew_lead == 4 & ONA_index == 1135 ~ 168, # subplot_id == "0198D"
      subplot_crew_lead == 6 & ONA_index ==  270 ~ 554, # subplot_id == "0054C"
      subplot_crew_lead == 6 & ONA_index == 1069 ~ 414, # subplot_id == "0141A"  
      subplot_crew_lead == 8 & ONA_index ==  819 ~ 419, # subplot_id == "0149D"
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

## Check
tmp$check_sp3 <- tmp$subplot2 |>
  summarise(count = n(), .by = subplot_plot_no) |>
  filter(count != 4)

nrow(tmp$check_sp3)


## Make a table of remaining plots with less than 4 subplots
tmp$subplot_missing <- tmp$subplot2 |>
  filter(subplot_plot_no %in% pull(tmp$check_sp3, subplot_plot_no))

# View(tmp$subplot_missing)
write_csv(tmp$subplot_missing, file.path(path$res$test, "plot_less4sp.csv"))

## Make a spatial file of plots with missing subplots
tmp$sf_subplot_missing <- tmp$subplot_missing |>
  mutate(lon = subplot_gps_lon, lat = subplot_gps_lat) |>
  filter(!is.na(lon), !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

st_write(tmp$sf_subplot_missing, file.path(path$res$test, "plot_less4sp.kml"), append = F)

## Check again for potential duplicates
message("Unique subplot codes: ", length(unique(tmp$subplot2$subplot_id)) == nrow(tmp$subplot2))

## IF not equal, investigate
tmp$dup <- tmp$subplot2 |>
  summarise(count = n(), .by = subplot_id) |>
  filter(count > 1) |>
  pull(subplot_id) |>
  sort()

print(tmp$dup)

## Clean again 
tmp$subplot3 <- tmp$subplot2 |>
  filter(!ONA_index %in% c(1819, 2237, 2245)) |>
  mutate(
    subplot_no = case_when(
      ONA_index == 2125 ~ "C", ## issue with 620B duplicate, so B becomes C and C becomes D
      ONA_index == 2126 ~ "D",
      ONA_index ==  103 ~ "C", ## issue with 627B
      TRUE ~ subplot_no
    ),
    ## Recalc subplot ID
    subplot_id = paste0(subplot_plot_id, subplot_no)
  )

## Check again for potential duplicates
message("Unique subplot codes: ", length(unique(tmp$subplot3$subplot_id)) == nrow(tmp$subplot3))



##
## Pass cleaned data to data_clean ####
##

data_clean$subplot <- tmp$subplot3


rm(tmp)

