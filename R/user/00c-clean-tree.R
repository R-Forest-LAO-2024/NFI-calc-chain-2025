

tmp <- list()

## Correct multi-stems not well reported in the field and tree number now based on nest1 and nest2 + inc. deadwood

##
## Check stem_go not working well ####
##

tmp$tree <- data_prep$tree |>
  mutate(
    tree_stem_go = if_else(is.na(tree_stem_go), "one", tree_stem_go),
    tree_stem_no = if_else(is.na(tree_stem_no) & tree_stem_go == "one", 1, tree_stem_no)
  )

tmp$check <- tmp$tree |>
  filter(is.na(tree_stem_no))

tmp$check2 <- tmp$check |> pull(ONA_parent_index) |> unique() |> sort() 

tmp$check3 <- tmp$tree |> filter(ONA_parent_index %in% tmp$check2)

## Manual correction of NAs in multi
## Screen through tmp$check3 with:
## View(tmp$check3)
## Then sort by ONA_index followed by sort by ONA_parent index
## !! Trees from different subplots (i.e different ONA_parent_index) can have the same ONA_index, 
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
      tree_harmo_src == "nest1" & ONA_index ==  759 ~ 1,
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
      
      TRUE ~ tree_stem_no
    ) 
  )

## FLAG multi 1 missing sometimes

## Check
table(data_clean$tree$tree_stem_go, useNA = "ifany")
table(data_clean$tree$tree_stem_no, useNA = "ifany")
 
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

