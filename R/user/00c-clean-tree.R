

tmp <- list()

## Correct multi-stems not well reported in the field and tree number now based on nest1 and nest2 + inc. deadwood

##
## Check stem_go not working well ####
##

tmp$tree <- data_prep$tree |>
  mutate(
    tree_stem_go_old = tree_stem_go,
    tree_stem_no_old = tree_stem_no,
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

tmp$check4 <- tmp$tree2 |> filter(is.na(tree_stem_no))
message("Trees with missisng stem_no after correction: ", nrow(tmp$check4))

## FLAG multi first stem no "1" missing sometimes - possible entry error or deadwood in coppice?
tmp$check5 <- tmp$tree2 |> filter(tree_stem_go == "multi")

tmp$tree2 |> filter(ONA_parent_index ==  36)
tmp$tree2 |> filter(ONA_parent_index ==  54)
tmp$tree2 |> filter(ONA_parent_index ==  99)
tmp$tree2 |> filter(ONA_parent_index == 502)
tmp$tree2 |> filter(ONA_parent_index == 741)
tmp$tree2 |> filter(ONA_parent_index == 830)
## Manual correction of missing stem_no 1
tmp$tree3 <- tmp$tree2 |>
  mutate(
    tree_stem_no = case_when(
      ONA_parent_index ==  36 & ONA_index ==  237 ~ 1,
      ONA_parent_index ==  36 & ONA_index ==  238 ~ 2,
      ONA_parent_index ==  54 & ONA_index ==  315 ~ 1,
      ONA_parent_index ==  54 & ONA_index ==  316 ~ 2,
      ONA_parent_index ==  54 & ONA_index ==  317 ~ 3,
      ONA_parent_index ==  99 & ONA_index ==  476 ~ 1,
      ONA_parent_index == 502 & ONA_index == 2920 ~ 1,
      ONA_parent_index == 502 & ONA_index == 2921 ~ 2,
      ONA_parent_index == 502 & ONA_index == 2922 ~ 3,
      ONA_parent_index == 502 & ONA_index == 2923 ~ 4,
      ONA_parent_index == 502 & ONA_index == 2924 ~ 5,
      ONA_parent_index == 741 & ONA_index == 4285 ~ 1,
      ONA_parent_index == 830 & ONA_index == 4834 ~ 1,
      
      TRUE ~ tree_stem_no
    )
  )

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

