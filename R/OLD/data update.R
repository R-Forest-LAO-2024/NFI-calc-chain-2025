
data_file_exist <- list.files("data-source")

if (usr_data_update == T | length(data_file_exist) == 0)
## Empty source data folder
unlink("data-source")


## Download latest version
download.file(url = "ONA", destfile = "data-source")

## Read data and harmo
tree <- read_csv("data-source/tree.csv")
plot <- read_csv("data-source/plot.csv")

tree_harmo <- tree |> rename(...) |> left_join(plot)

write_csv(tree_harmo, "data/tree_harmo.csv")

## Filter only new data
tree_harmo_new <- tree_harmo |> filter(...)


## Clean new data
## - Script to make, treeplot map, dbh - h graph, and tables of N tree per DBH class
## Visual clean


## Manual editing of outlier
tree_clean_new <- tree_harmo_new |> 
  mutate(...) |>
  mutate(approved_by = "Name of officer", approved_time = Sys.time(), )


## Add new data to already clean data
tree_clean_all <- bind_rows(tree_clean_old, tree_clean_new)

write_csv(tree_clean_all, paste0("data-clean/tree", Sys.time(), ".csv"))


