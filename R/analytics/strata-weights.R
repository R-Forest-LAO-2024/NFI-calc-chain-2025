
tmp <- list()

names(anci$ceo)
table(anci$ceo$LC)

tmp$ceo <- anci$ceo |> 
  left_join(anci$lc_ceo, by = c("LC" = "lc_code")) |>
  left_join(anci$ceo_nfi_id, by = join_by(pl_orig_fid))


tmp$ceo_sample_total <- nrow(anci$ceo)
tmp$ceo_country_area <- tmp$ceo_sample_total * usr$CEO_grid_with * usr$CEO_grid_height * 100

tmp$ceo_strata <- tmp$ceo |>
  summarise(strata_count = n(), .by = c(lc_strata_no, lc_strata_name)) |>
  mutate(
    strata_area = strata_count / tmp$ceo_sample_total * tmp$ceo_country_area,
    strata_weight = round(strata_count / tmp$ceo_sample_total, 4)
    ) |>
  arrange(lc_strata_no)

tmp$ceo_strata


## TOTAL CEO
nrow(tmp$ceo)

## TOTAL PHASE 2 plan: 540
tmp$ceo |> filter(!is.na(plotid)) |> nrow()

## TOTAL PHASE 2 recorded: 534
subplot |> pull(subplot_plot_no) |> unique() |> length()


nrow(anci$subplot_plan)

## 2161

# 540 planned  / 533

## 6/7 plots no team

## ~40 team went but no access

## 1300 LC point info needed out of 10700

