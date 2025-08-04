
## Lying deadwood follow 2 times 16 m line transects

## Checks
# names(data_prep$ldw)
# View(data_prep$ldw)


## No visible issue

data_clean$ldw <- data_prep$ldw |>
  filter(!(ldw_diameter == 10.05 & ldw_hollow_d2 == 30.1)) |>
  mutate(
    ldw_hollow_d1 = case_when(
      ldw_diameter == 40.4 & ldw_hollow_d1 == 1.9 ~ 19,
      TRUE ~ ldw_hollow_d1
    )
  )
