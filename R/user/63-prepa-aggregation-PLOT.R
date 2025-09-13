

## Make AGB simple average - fast code, not clean but to the point

##
## Make plot level main LC ####
##

tmp_plotlc <- ph2_subplot |>
  filter(access, plot_id <= 636, (lc_no <= 22 | lc_no >= 160)) |>
  mutate(lc_no = if_else(lc_no == 16, 169, lc_no)) |>
  summarise(count_lc = n(), .by = c(plot_id, lc_no))


## Extract plots with unique IDs
vec_plot_unique <- tmp_plotlc |>
  summarise(count = n(), .by = plot_id) |>
  filter(count == 1) |>
  pull(plot_id)

tmp_plotlc_unique <- tmp_plotlc |>
  filter(plot_id %in% vec_plot_unique) |>
  select(plot_id, lc_no)

## Find max LC for duplicates
tmp_maxlc <- tmp_plotlc |>
  filter(!plot_id %in% vec_plot_unique) |>
  summarise(maxlc = max(count_lc), .by = plot_id) |>
  mutate(is_max = T)

tmp_plot_lcmax <- tmp_plotlc |>
  filter(!plot_id %in% vec_plot_unique) |>
  left_join(tmp_maxlc, by = join_by(plot_id, count_lc == maxlc)) |>
  filter(is_max)

## Extract plot with max LC
tmp_plotlc_maxlc <- tmp_plot_lcmax |>
  filter(count_lc != 10) |>
  select(plot_id, lc_no)

## solve no max (10-10) with min LC
tmp_plotlc_nomax <- tmp_plot_lcmax |>
  filter(count_lc == 10) |>
  summarise(minlc = min(lc_no), .by = plot_id) |>
  rename(lc_no = minlc)
  
length(unique(tmp_plotlc$plot_id))
nrow(tmp_plotlc_unique) + nrow(tmp_plotlc_maxlc) + nrow(tmp_plotlc_nomax)

tmp_plotlc2 <- bind_rows(tmp_plotlc_unique, tmp_plotlc_maxlc, tmp_plotlc_nomax) |> arrange(plot_id)

length(unique(tmp_plotlc$plot_id))
length(unique(tmp_plotlc2$plot_id))

## Cross-check
vec_dup <- tmp_plotlc2 |>
  summarise(count = n(), .by = plot_id) |>
  filter(count > 1) |>
  pull(plot_id)

tt <- tmp_plotlc2 |> filter(plot_id %in% vec_dup)

## Final tuning
tmp_plotlc3 <- tmp_plotlc2 |>
  filter(
    !(plot_id ==  75 & lc_no == 22),
    !(plot_id == 112 & lc_no == 13),
    !(plot_id == 330 & lc_no == 22),
    !(plot_id == 363 & lc_no == 15),
    !(plot_id == 469 & lc_no == 22),
    !(plot_id == 469 & lc_no == 21),
    !(plot_id == 595 & lc_no == 21)
    )

plot_lc <- tmp_plotlc3

rm(list = str_subset(ls(), pattern = "tmp_"))



##
## Aggregate AGB ####
##

sp_agb <- tree |>
  rename(plot_id = subplot_plot_no) |>
  summarise(
    spagb = sum(tree_agb_final * tree_weight_spha)/1000, 
    .by = c(plot_id, subplot_no)
    )

plot_agb <- sp_agb |>
  summarise(pagb = mean(spagb), .by = plot_id)

ftype_agb <- plot_agb |>
  left_join(plot_lc, by = join_by(plot_id)) |>
  group_by(lc_no) |>
    summarise(
      plot_count = n(),
      ftype_agb = mean(pagb),
      ftype_agb_sd = sd(pagb)
    ) |>
  mutate(
    ftype_agb_se = ftype_agb_sd / sqrt(plot_count),
    ftype_agb_me = qt(0.975, plot_count - 1) * ftype_agb_se,
    ftype_agb_U  = ftype_agb_me / ftype_agb * 100,
    ftype_agb_cilower = ftype_agb - ftype_agb_me,
    ftype_agb_ciupper = ftype_agb + ftype_agb_me
  )



