

tmp <- list()

if (!"tab" %in% ls()) tab <- list()
if (!"fig" %in% ls()) fig <- list()
if (!"val" %in% ls()) val <- list()


##
## VALUES ####
##

val$nplot_ph1 <- nrow(ph1_data)

val$nplot_ph2 <- length(unique(ph2_subplot$plot_id))

val$nplot_visit <- ph2_subplot |>
  filter(plot_id <= 636) |>
  pull(plot_id) |>
  unique() |>
  length()

val$nplot_access <- ph2_subplot |>
  filter(plot_id <= 636, access) |>
  pull(plot_id) |>
  unique() |>
  length()



##
## TABLES ####
##

## + 1. Nh ####

tmp$stratum_name <- anci$lc |>
  select(stratum = lu_strata_no, stratum_name = lu_strata_name) |>
  distinct()

tab$tbl_Nh <- res3_list$subpop_stratum |> 
  distinct(subpop, stratum, Nh) |>
  summarise(Nh = sum(Nh), .by = stratum) |>
  left_join(tmp$stratum_name, by = join_by(stratum)) |>
  arrange(stratum) |>
  select(-stratum) |>
  pivot_wider(names_from = stratum_name, values_from = Nh) %>%
  mutate(Total = rowSums(.))

## + 2. nh history ####

tmp$tbl_access <- ph2_subplot |>
  group_by(plot_id, stratum) |>
  summarise(access = any(access), .groups = "drop") |>
  mutate(visit = if_else(plot_id <= 636, TRUE, FALSE)) |>
  group_by(stratum, visit, access) |>
  summarise(count = n(), .groups = "drop") |>
  left_join(tmp$stratum_name, by = join_by(stratum)) |>
  select(-stratum) |>
  pivot_wider(names_from = stratum_name, values_from = count) |>
  mutate(across(where(is.integer), \(x) replace_na(x, 0))) |>
  arrange(desc(visit), desc(access)) %>%
  rowwise() %>%
  mutate(
    Status = case_when(
      visit & access ~ "1. Visited and accessed",
      visit & !access ~ "2. Visited but no access",
      !visit & access ~ "3. Not visited but accessible",
      !visit & !access ~ "4. Not visited and no access"
    ),
    Total = rowSums(across(where(is.numeric)))
    ) |>
  select(Status, everything(), -visit, -access)
tmp$tbl_access 

tmp$tbl_access_tot <- flatten(list(Status = "Total", colSums(select(tmp$tbl_access, where(is.numeric)))))

tab$tbl_access <- bind_rows(tmp$tbl_access, tmp$tbl_access_tot)
tab$tbl_access

## + 3. Nb of measurements ####

tab$tbl_entity <- tibble(
  Entity = c("Tree", "Standing DW", "Stumps", "Sapling", "Lying DW"),
  count  = c(nrow(tree), nrow(dw), nrow(stump), nrow(sapling), nrow(ldw))
) |> pivot_wider(names_from = Entity, values_from = count)
tab$tbl_entity

## + 4. Total C ####
tmp$lc_name <- anci$lc |> select(lc_no = lu_no, `LC code` = lu_code_new, `LC name` = lu_name, `LC name Lao` = lu_name_lao)

tab$tbl_carbon <- res3_list$totals_short |> 
  filter(attr == "Ctot", lc_no <= 30 | lc_no > 160) |>
  left_join(tmp$lc_name, by = join_by(lc_no)) |>
  select(`LC no` = lc_no, `LC code`, `LC name`, `LC name Lao`, Ctot = Rd, `U (perc)` = Rd_mep)




##
## FIGURES ####
##

## Land cover section example ####

source(here("R/user/99-gg-example-tree-with-lcs.R"))

fig$lcs_ex <- tmp$gg_lcs_example_new
# fig$lcs_ex_old <- tmp$gg_lcs_example
