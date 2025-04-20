# 
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


##
## Check stem_go not working well ####
##


## TBD, for now each stem is a different tree

data_clean$tree <- data_prep$tree
