

tmp <- list()

## Correct multi-stems not well reported in the field and tree number now based on nest1 and nest2 + inc. deadwood

##
## Replace NAs in tree_stem_go and tree_stem_no ####
##

## + Automatic replacement of NAs for unique stems ####

tmp$tree <- data_prep$tree |>
  mutate(
    tree_stem_go_old = tree_stem_go,
    tree_stem_no_old = tree_stem_no,
    tree_stem_go = if_else(is.na(tree_stem_go), "one", tree_stem_go),
    tree_stem_no = if_else(is.na(tree_stem_no) & tree_stem_go == "one", 1, tree_stem_no)
  )


## Check  
table(tmp$tree$tree_stem_go, useNA = "ifany")
table(tmp$tree$tree_stem_no, useNA = "ifany")


## + Manual correction of NAs in multi-stems trees ####

## Make a table of all subplots with NAs
tmp$check <- tmp$tree |> filter(is.na(tree_stem_no))

tmp$check2 <- tmp$check |> pull(ONA_parent_index) |> unique() |> sort() 

tmp$check3 <- tmp$tree |> filter(ONA_parent_index %in% tmp$check2)

## Manual correction of NAs in multi 
## Steps:
## - Screen through tmp$check3 with:
## View(tmp$check3)
## - Sort by ONA_index followed by ONA_parent index
## - See what number is missing based on previous and following records
## - Edit the value with mutate() anmd case_when()
## Note: Trees from different subplots (i.e different ONA_parent_index) can have the same ONA_index, 
## because ONA_index is assigned for nest1 and nest2 separately
tmp$tree2 <- tmp$tree |>
  mutate(
    tree_stem_no = case_when(
      tree_harmo_src == "nest1" & ONA_index ==   50 ~ 3,
      tree_harmo_src == "nest1" & ONA_index ==   59 ~ 1,
      tree_harmo_src == "nest1" & ONA_index ==   61 ~ 3,
      tree_harmo_src == "nest1" & ONA_index ==   71 ~ 1,
      tree_harmo_src == "nest1" & ONA_index ==   83 ~ 3,
      tree_harmo_src == "nest1" & ONA_index ==  389 ~ 1,
      tree_harmo_src == "nest1" & ONA_index ==  464 ~ 1,
      tree_harmo_src == "nest1" & ONA_index ==  912 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 1017 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 1018 ~ 2,
      
      tree_harmo_src == "nest1" & ONA_index == 1465 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 1846 ~ 2,
      tree_harmo_src == "nest1" & ONA_index == 2297 ~ 2,
      tree_harmo_src == "nest1" & ONA_index == 2365 ~ 2,
      tree_harmo_src == "nest1" & ONA_index == 2440 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 2457 ~ 3,
      tree_harmo_src == "nest1" & ONA_index == 2468 ~ 1,
      tree_harmo_src == "nest2" & ONA_index ==  759 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 2740 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 2815 ~ 1,
      
      tree_harmo_src == "nest1" & ONA_index == 2936 ~ 2,
      tree_harmo_src == "nest1" & ONA_index == 2988 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 4103 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 4218 ~ 1,
      tree_harmo_src == "nest2" & ONA_index == 1388 ~ 2,
      tree_harmo_src == "nest1" & ONA_index == 4534 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 4540 ~ 2,
      tree_harmo_src == "nest1" & ONA_index == 4654 ~ 2,
      tree_harmo_src == "nest2" & ONA_index == 1481 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 5238 ~ 1,
      
      tree_harmo_src == "nest1" & ONA_index == 5399 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 5476 ~ 1,
      tree_harmo_src == "nest2" & ONA_index == 1807 ~ 2,
      tree_harmo_src == "nest1" & ONA_index == 5581 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 6312 ~ 1,
      tree_harmo_src == "nest2" & ONA_index == 2079 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 6837 ~ 2,
      tree_harmo_src == "nest1" & ONA_index == 6917 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 6948 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 6961 ~ 1,
      
      tree_harmo_src == "nest1" & ONA_index == 7006 ~ 3,
      tree_harmo_src == "nest1" & ONA_index == 7040 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 7049 ~ 2,
      tree_harmo_src == "nest1" & ONA_index == 7184 ~ 2,
      tree_harmo_src == "nest1" & ONA_index == 7195 ~ 2,
      tree_harmo_src == "nest1" & ONA_index == 7200 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 7431 ~ 2,
      tree_harmo_src == "nest1" & ONA_index == 7667 ~ 1,
      tree_harmo_src == "nest1" & ONA_index == 8121 ~ 1,
      
      TRUE ~ tree_stem_no
    ) 
  )

## Check
tmp$check4 <- tmp$tree2 |> filter(is.na(tree_stem_no))
message("Trees with missisng stem_no after correction: ", nrow(tmp$check4))

## + Manual correction of multi-stems missing value 1 ####

## Screen through multi-stems and vidually check that all are sequences starting with 1
tmp$check5 <- tmp$tree2 |> filter(tree_stem_go == "multi")

# tmp$tree2 |> filter(ONA_parent_index ==   36)
# tmp$tree2 |> filter(ONA_parent_index ==   54)
# tmp$tree2 |> filter(ONA_parent_index ==   99)
# tmp$tree2 |> filter(ONA_parent_index ==  502)
# tmp$tree2 |> filter(ONA_parent_index ==  741)
# tmp$tree2 |> filter(ONA_parent_index ==  830)
# tmp$tree2 |> filter(ONA_parent_index == 1287)
# tmp$tree2 |> filter(ONA_parent_index == 1346)
# tmp$tree2 |> filter(ONA_parent_index ==  192)
# tmp$tree2 |> filter(ONA_parent_index ==  740)
# tmp$tree2 |> filter(ONA_parent_index == 1241)
# tmp$tree2 |> filter(ONA_parent_index == 1328)
tmp$tree2 |> filter(ONA_parent_index == 83)
tmp$tree2 |> filter(ONA_parent_index == 121)

## Manual correction of missing stem_no value 1
tmp$tree3 <- tmp$tree2 |>
  mutate(
    tree_stem_no = case_when(
      ONA_parent_index ==   36 & ONA_index ==  237 ~ 1,
      ONA_parent_index ==   36 & ONA_index ==  238 ~ 2,
      ONA_parent_index ==   54 & ONA_index ==  315 ~ 1,
      ONA_parent_index ==   54 & ONA_index ==  316 ~ 2,
      ONA_parent_index ==   54 & ONA_index ==  317 ~ 3,
      ONA_parent_index ==   99 & ONA_index ==  476 ~ 1,
      ONA_parent_index ==  502 & ONA_index == 2920 ~ 1,
      ONA_parent_index ==  502 & ONA_index == 2921 ~ 2,
      ONA_parent_index ==  502 & ONA_index == 2922 ~ 3,
      ONA_parent_index ==  502 & ONA_index == 2923 ~ 4,
      ONA_parent_index ==  502 & ONA_index == 2924 ~ 5,
      ONA_parent_index ==  741 & ONA_index == 4285 ~ 1,
      ONA_parent_index ==  830 & ONA_index == 4834 ~ 1,
      ONA_parent_index == 1287 & ONA_index == 7650 ~ 1,
      ONA_parent_index ==  192 & ONA_index ==  367 ~ 1,
      ONA_parent_index ==  740 & ONA_index == 1382 ~ 1,
      ONA_parent_index ==  740 & ONA_index == 1383 ~ 2,
      ONA_parent_index == 1328 & ONA_index == 2470 ~ 1,
      ONA_parent_index ==   83 & ONA_index ==  425 ~ 1,
      ONA_parent_index ==  121 & ONA_index ==  708 ~ 1,
      ONA_parent_index ==  121 & ONA_index ==  709 ~ 2,
      TRUE ~ tree_stem_no
    ),
    tree_stem_go = case_when(
      ONA_parent_index ==  1346 & ONA_index == 8072 ~ "multi",
      ONA_parent_index ==  1241 & ONA_index == 2353 ~ "multi",
      TRUE ~ tree_stem_go
    )
  )

##
## Correct tree_no based on tree_stem_no == 1 ####
## 

## Check
# tmp$tree |> nrow()
# tmp$tree3 |> filter(tree_stem_no == 1) |> nrow()

## + re-create tree_no based on row number for each plot ####
tmp$first_stem <- tmp$tree3 |> 
  filter(tree_stem_no == 1) |>
  arrange(ONA_parent_index, tree_harmo_src, ONA_index) |>
  group_by(ONA_parent_index) |>
  mutate(tree_no_corr = row_number()) |>
  ungroup() |>
  select(ONA_parent_index, ONA_index_join = ONA_index, tree_harmo_src, tree_no_corr)

## + Add tree_no back to tree table ####
tmp$tree4 <- tmp$tree3 |>
  mutate(ONA_index_join = if_else(tree_stem_go == "multi", ONA_index - tree_stem_no + 1, ONA_index)) |>
  left_join(tmp$first_stem, by = join_by(ONA_parent_index, ONA_index_join, tree_harmo_src)) |>
  mutate(
    tree_no_old = tree_no,
    tree_no = tree_no_corr
    ) |>
  select(-tree_no_corr, -ONA_index_join)

## Check
# summary(tmp$tree4$tree_no)


##
## Add tree land cover section based on distance and azimuth ####
##

## Add land cover section based on:
## - assigned supblot section circles 
## - equidistance between observation points: Thiessen polygon >> FINAL SOLUTION

tmp$tree5 <- tmp$tree4 |>
  mutate(
    tree_x = cos((90 - tree_azimuth) * pi/180) * tree_distance,
    tree_y = sin((90 - tree_azimuth) * pi/180) * tree_distance,
    ## lcs_no based on circles
    # tree_lcs_no = case_when(
    #   tree_distance <= 8 ~ 1,
    #   tree_azimuth > 315 | tree_azimuth <=45  ~ 2,
    #   tree_azimuth >  45 & tree_azimuth <=135 ~ 3,
    #   tree_azimuth > 135 & tree_azimuth <=225 ~ 4,
    #   tree_azimuth > 225 & tree_azimuth <=315 ~ 5,
    #   TRUE ~ NA_integer_
    # ),
    ## lcs_no based on Thiessen polygons (equi-distance to LCS observation points)
    tree_lcs_no = case_when(
      abs(tree_x) <= 6 & abs(tree_y) <= 6 ~ 1,
      tree_azimuth > 315 | tree_azimuth <=45  ~ 2,
      tree_azimuth >  45 & tree_azimuth <=135 ~ 3,
      tree_azimuth > 135 & tree_azimuth <=225 ~ 4,
      tree_azimuth > 225 & tree_azimuth <=315 ~ 5,
      TRUE ~ NA_integer_
    )
  )



##
## Pass data to 'data_clean' ####
##

data_clean$tree <- tmp$tree5


rm(tmp)





# ##
# ## Check main species ####
# ##
# 
# 
# main_species <- data_harmo$tree |>
#   group_by(tree_species_code) |>
#   summarise(count = n()) |>
#   filter(tree_species_code != "0", tree_species_code != "9999") |>
#   slice_max(n = 10, order_by = count) |>
#   left_join(anci$species_list, by = "tree_species_code")
# 
# print(main_species)
# 
# write_csv(main_species, here(file.path(path$res$tab, "main-species-noscalefactor.csv")))
# 
# main_species_corr <- data_harmo$tree |>
#   mutate(
#     tree_plot_radius = if_else(tree_dbh < 30, 8, 16),
#     tree_scale_factor = 10000 / (pi * tree_plot_radius^2),
#     tree_count = 1
#   ) |>
#   group_by(tree_species_code) |>
#   summarise(
#     count = sum(tree_count),
#     count_sf = sum(tree_scale_factor)
#   ) |>
#   filter(tree_species_code != "0", tree_species_code != "9999") |>
#   slice_max(n = 10, order_by = count_sf) |>
#   left_join(anci$species_list, by = "tree_species_code")
# 
# print(main_species_corr)
# 
# write_csv(main_species_corr, here(file.path(path$res$tab, "main-species.csv")))

