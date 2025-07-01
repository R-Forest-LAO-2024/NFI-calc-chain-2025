
tmp <- list()

names(anci$ceo)
table(anci$ceo$LC)

tmp$ceo <- anci$ceo |>
  left_join(anci$lc_ceo, by = c("LC" = "lc_code"))


tmp$ceo_sample_total <- nrow(anci$ceo)
tmp$ceo_country_area <- tmp$ceo_sample_total * usr$CEO_grid_with * usr$CEO_grid_height * 100

tmp$ceo_strata <- tmp$ceo |>
  summarise(strata_count = n(), .by = lc_strata_no) |>
  mutate(
    strata_area = strata_count / tmp$ceo_sample_total * tmp$ceo_country_area,
    strata_weight = round(strata_count / tmp$ceo_sample_total, 4)
    ) |>
  arrange(lc_strata_no)

tmp$ceo_strata

## total 1067 plots

nrow(anci$subplot_plan)

## 2161

# 540 planned  / 533

## 6/7 plots no team

## ~40 team went but no access

## 1300 LC point info needed out of 10700
540 * 4 * 5
