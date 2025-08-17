

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

## + Nh ####

tmp$stratum_name <- anci$lc |>
  select(stratum = lu_strata_no, stratum_name = lu_strata_name) |>
  distinct()

tab$Nh <- res3_list$subpop_stratum |> 
  distinct(subpop, stratum, Nh) |>
  summarise(Nh = sum(Nh), .by = stratum) |>
  left_join(tmp$stratum_name, by = join_by(stratum)) |>
  arrange(stratum) |>
  select(-stratum) |>
  pivot_wider(names_from = stratum_name, values_from = Nh) %>%
  mutate(Total = rowSums(.))

## + nh history ####

tmp$access <- ph2_subplot |>
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
tmp$access 

tmp$access_tot <- flatten(list(Status = "Total", colSums(select(tmp$access, where(is.numeric)))))

tab$access <- bind_rows(tmp$access, tmp$access_tot)
tab$access

## + Nb of measurements ####

tab$entity <- tibble(
  Entity = c("Tree", "Standing DW", "Stumps", "Sapling", "Lying DW"),
  count  = c(nrow(tree), nrow(dw), nrow(stump), nrow(sapling), nrow(ldw))
) |> pivot_wider(names_from = Entity, values_from = count)
tab$entity

## + Total C ####
tmp$lc_name <- anci$lc |> select(lc_no = lu_no, `LC code` = lu_code_new, `LC name` = lu_name, `LC name Lao` = lu_name_lao)

tab$carbon <- res3_list$totals_short |> 
  filter(attr == "Ctot", lc_no <= 30 | lc_no > 160) |>
  left_join(tmp$lc_name, by = join_by(lc_no)) |>
  select(`LC no` = lc_no, `LC code`, `LC name`, `LC name Lao`, Ctot = Rd, `U (perc)` = Rd_mep)

## + AE Biomass ####

tab$allometry <- tibble(
  `LC class` = c("EG", "MD", "DD", "CF", "MCB", "P_AC", "P_EC", "P_RB", "P_TK", "P_OTH", "RV", "B"),
  Equation = c(
    "$0.3112 \\times D^{2.2331}$",
    "$0.523081 \\times D^{2}$",
    "$0.2137 \\times D^{2.2575}$",
    "$0.1277 \\times D^{2.3944}$",
    "$0.1277 \\times D^{2.3944}$",
    "$0.1173 \\times D^{2.454}$",
    "$0.199 \\times D^{2.185}$",
    "$0.0082 \\times (\\pi \\times D)^{2.5623}$",
    "$0.077 \\times D^{2.546}$",
    "$0.3112 \\times D^{2.2331}$",
    "$0.6 \\times exp(-1.499 + 2.148 \\times log(D) + 0.207 \\times (log(D))^2 - 0.0281 \\times (log(D))^3)$",
    "$0.6 \\times exp(-1.499 + 2.148 \\times log(D) + 0.207 \\times (log(D))^2 - 0.0281 \\times (log(D))^3)$"
    ),
  Source = c(rep("TBD", 12))
)


## + RS ratios ####

tab$rs <- tibble(
  Condition = c("CF, plot AGB < 50 t/ha", "CF, plot AGB <= 150 t/ha", "CF, plot AGB > 150 t/ha", "Other forest, plot AGB < 125 t/ha", "Other forest, plot AGB >= 125 t/ha"),
  RS = c(0.46, 0.32, 0.23, 0.2, 0.24)
)


# Lying Dead Wood
# round(pi^2 / (8 * 32) * sum_d_sq)* Lying Dead Wood density *1000.
# Sum_d-sq= sum of the diameters^2 along the line 32 is the length of the line in meters Van Wagner (1968)
# Lying dead wood (wood density, g/cm3)
# EF, Sound 0.39 ± 0.18 (8) "Development of specific allometric equations in Lao PDR"(2017)
# EF, Intermediate 0.34 ± 0.09 (15) "Development of specific allometric equations in Lao PDR"(2017)
# EF, Rotten 0.26 ± 0.10 (10) "Development of specific allometric equations in Lao PDR"(2017)
# DD, Sound 0.44 ± 0.14 (17) "Development of specific allometric equations in Lao PDR"(2017)
# DD, Intermediate 0.35 ± 0.19 (8) "Development of specific allometric equations in Lao PDR"(2017)
# DD, Rotten 0.32 ± 0.13 (15) "Development of specific allometric equations in Lao PDR"(2017)
# MDF, Sound 0.45 ± 0,14 (17) "Development of specific allometric equations in Lao PDR"(2017)
# MDF, Intermediate 0.3 ± 0.09 (10) "Development of specific allometric equations in Lao PDR"(2017)
# MDF, Rotten 0.29 ± 0.13 (7) "Development of specific allometric equations in Lao PDR"(2017)
# Other, Sound 0.44 ± 0.14 (42) "Development of specific allometric equations in Lao PDR"(2017)
# Other, Intermediate 0.33 ± 0.09 (33) "Development of specific allometric equations in Lao PDR"(2017)
# Other, Rotten 0.30 ± 0.12 (32) "Development of specific allometric equations in Lao PDR"(2017)


##
## FIGURES ####
##

## + Land cover section example ####

source(here("R/user/99-gg-example-tree-with-lcs.R"))

fig$lcs_ex <- tmp$gg_lcs_example_new
# fig$lcs_ex_old <- tmp$gg_lcs_example

## + AGB models ####

fig$tree_agb <- gg_tree_agb
fig$tree_agbp <- gg_tree_agbp

