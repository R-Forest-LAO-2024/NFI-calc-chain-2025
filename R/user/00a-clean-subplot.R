
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
      ONA_index == 109 ~ "B",
      ONA_index == 113 ~ "B",
      ONA_index == 265 ~ "C",
      ONA_index == 440 ~ "C",
      ONA_index == 980 ~ "B",
      TRUE ~ subplot_no
    ),
    subplot_plot_no = case_when(
      ONA_index == 1269 ~ 333,
      ONA_index == 1270 ~ 333,
      ONA_index == 1271 ~ 333,
      ONA_index == 1272 ~ 333,
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

tmp$check_sp <- tmp$subplot1 |>
  arrange(subplot_plot_no) |>
  summarise(count = n(), .by = subplot_plot_no)

table(tmp$check_sp$count, useNA = "ifany")

print(filter(tmp$check_sp, count != 4))

write_csv(filter(tmp$check_sp, count != 4), file.path(path$res$test, "plot_less4sp.csv"))

## >> Need manual checks of the 3/1 maybe wrong plot ID. 
## >> Could also be checked by distance to planed location 


## + !!! Modify data to correct entry typos ####
tmp$subplot2 <- tmp$subplot1 |>
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

tmp$subplot2 |>
  summarise(count = n(), .by = subplot_plot_no) |>
  filter(count != 4) |>
  nrow()


## Check again for potential duplicates
message("Unique subplot codes: ", length(unique(tmp$subplot2$subplot_id)) == nrow(tmp$subplot2))


##
## Pass cleaned data to data_clean ####
##

data_clean$subplot <- tmp$subplot2


rm(tmp)

