
## Initiate list to store temporary objects
tmp <- list()

tmp$tree1 <- data_init$tree_init1 |> mutate(harmo_src = "init1")
tmp$tree2 <- data_init$tree_init2 |> mutate(harmo_src = "init2")

## Prepare tree data to combine nest1 ad nest2
names(tmp$tree1) <- str_remove(names(tmp$tree1), pattern = ".*__")
names(tmp$tree1) <- str_remove_all(names(tmp$tree1), pattern = "_nest[0-9]")

names(tmp$tree2) <- str_remove(names(tmp$tree2), pattern = ".*__")
names(tmp$tree2) <- str_remove_all(names(tmp$tree2), pattern = "_nest[0-9]")

## TEST
# print(names(tmp$tree1))
# print(names(tmp$tree2))
# print(match(names(tmp$tree1), names(tmp$tree2)))

## Correct column name error
names(tmp$tree1)[15] <- "t_height_dbh"
names(tmp$tree2)[15] <- "t_height_dbh"
names(tmp$tree2)[24] <- names(tmp$tree1)[24]

## Keep track of error corrections
write_lines(
  paste0(
    local_time("UTC"), ": Harmonization of tree tables \n",
    "Corrected column name error in tree_data_nest1 \n
    - col 15: height_dbh -> t_height_dbh \n
    Corrected column name error in tree_data_nest2 \n
    - col 15: t_height_dbh -> t_height_dbh \n
    - col 24: tree_dead_cl2_tall/t_dead_height_dbh_short -> tree_dead_cl2_tall/t_dead_height_dbh_tall \n
  "),
  file = here("log.txt"), 
  append = TRUE
)

## Group tree, deadwood and stump for both nested circles
tmp$combi <- bind_rows(tmp$tree1, tmp$tree2)

## TREE
data_harmo$tree <- tmp$combi |>
  filter(t_livedead ==  "1") |>
  mutate(
    tree_harmo_src = harmo_src,
    tree_no = as.numeric(t_nb),
    tree_stem_go = stem_go,
    tree_stem_no = as.numeric(stem_nb),
    tree_distance = as.numeric(t_dist),
    tree_azimuth = as.numeric(t_az),
    tree_species_code = t_species_name,
    tree_species_newlocal = t_species_other,
    tree_dbh = as.numeric(t_dbh),
    tree_pom = as.numeric(t_height_dbh),
    tree_dbh100_url = t_dbh100
    ) |>
  select(
    starts_with("ONA_"),
    starts_with("tree_")
    )

## STUMP
data_harmo$stump <- tmp$combi |>
  filter(t_deadcl == "3") |>
  mutate(
    stump_no            = as.numeric(t_nb),
    stump_stem_no       = as.numeric(stem_nb),
    stump_distance      = as.numeric(t_dist),
    stump_azimuth       = as.numeric(t_az),
    stump_diameter1     = as.numeric(diameter1),
    stump_diameter2     = as.numeric(diameter2),
    stump_diameter_mean = as.numeric(avg_diam), ## mean(c(diameter1, diameter2), na.rm = TRUE),
    stump_more50_url    = stump_more50,
    stump_height        = height_st,
    stump_cut_class     = stump_cut_cl,
    stump_age           = age_stump,
    stump_rk            = remarks
    ) |>
  select(starts_with("ONA_"), starts_with("stump_"))

data_harmo$dw <- tmp$combi |>
  filter(t_livedead == "2", t_deadcl != "3") |>
  mutate(
    dw_no = as.numeric(t_nb),
    dw_stem_no = as.numeric(stem_nb),
    dw_distance = as.numeric(t_dist),
    dw_azimuth  = as.numeric(t_az),
    dw_class = as.numeric(t_deadcl),
    dw_tallshort = as.numeric(t_deadcl2_tallshort),
    dw_class = case_when(
      dw_class == 1 ~ "decay 1", 
      dw_class == 2 & t_deadcl2_tallshort == 1 ~ "decay 2 short",
      dw_class == 2 & t_deadcl2_tallshort == 2 ~ "decay 2 tall",
      TRUE ~ NA_character_
    ),
    dw_bole_height = case_when(
      t_deadcl == 2 & t_deadcl2_tallshort == 1 ~ as.numeric(t_dead_height_short),
      t_deadcl == 2 & t_deadcl2_tallshort == 2 ~ as.numeric(t_dead_height_tall),
      TRUE ~ NA_real_
    ),
    dw_dbh = case_when(
      t_deadcl == 1 ~ as.numeric(t_dbh),
      t_deadcl == 2 & t_deadcl2_tallshort == 1 ~ as.numeric(t_dead_DBH_short),
      t_deadcl == 2 & t_deadcl2_tallshort == 2 ~ as.numeric(t_dead_DBH_tall),
      TRUE ~ NA_real_
    ),
    dw_pom = case_when(
      t_deadcl == 1 ~ as.numeric(t_height_dbh),
      t_deadcl == 2 & t_deadcl2_tallshort == 1 ~ as.numeric(t_dead_height_dbh_short),
      t_deadcl == 2 & t_deadcl2_tallshort == 2 ~ as.numeric(t_dead_height_dbh_tall),
      TRUE ~ NA_real_
    ),
    dw_dbase = case_when(
      t_deadcl == 2 & t_deadcl2_tallshort == 1 ~ as.numeric(t_dead_DB_short),
      t_deadcl == 2 & t_deadcl2_tallshort == 2 ~ as.numeric(t_dead_DB_tall),
      TRUE ~ NA_real_
    ),
    dw_dtop = case_when(
      t_deadcl == 2 & t_deadcl2_tallshort == 1 ~ as.numeric(t_dead_DT_short),
      t_deadcl == 2 & t_deadcl2_tallshort == 2 ~ dw_dbase - (dw_bole_height * ((dw_dbase - dw_dbh) / 130 * 100)),
      TRUE ~ NA_real_
    )
  ) |>
  select(starts_with("ONA_"), starts_with("dw_"))


## Check
# nrow(tmp$combi) == nrow(data_harmo$tree) + nrow(data_harmo$stump) + nrow(data_harmo$dw)

## Clean tmp elements
rm(tmp)
