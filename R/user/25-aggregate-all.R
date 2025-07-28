
tmp_lc <- anci$lc |> select(lc_no = lu_no, lc_code = lu_code_new)
vec_pools <- c("agb", "bgb", "sap_agb", "dw", "stump", "ldw", "Ctot")
##
## USING FUNCTION 3: ratio estimator, 2 phase sampling for stratification ####
##

## + AGB ####
allres3_agb <- nfi_aggregate3(
  .ph1_df = ph1_data, 
  .ph2_sp = ph2_sp_all, 
  .class_d = lc_no, 
  .attr_y = agb, 
  .attr_x = sp_area, 
  .aoi_area = 23680000
  ) 

res3_agb <- allres3_agb$totals_short |> 
  filter(lc_no <= 22 | lc_no >= 160) |>
  left_join(tmp_lc, by = join_by(lc_no)) |>
  select(lc_no, lc_code, everything())

write_csv(allres3_agb$plot, file.path(path$res$data, "plot-summary-live-tree-agb.csv"))


## + Combine all totals simplified ####
res3_totals <- map(vec_pools, function(x){
  
  tt <- nfi_aggregate3(
    .ph1_df = ph1_data, 
    .ph2_sp = ph2_sp_all, 
    .class_d = lc_no, 
    .attr_y = !!sym(x), 
    .attr_x = sp_area, 
    .aoi_area = 23680000
  )
  
  tt$totals_short |> 
    filter(lc_no < 30 | lc_no > 160) |>
    left_join(tmp_lc, by = join_by(lc_no)) |>
    select(lc_no, lc_code, everything())
  
}) |> list_rbind()

write_csv(res3_totals, file.path(path$res$data, paste0("res3-totals-allpools-", Sys.Date(),".csv")))


## + Combine all totals  ####

res3_all <- map(vec_pools, function(x){
  
  nfi_aggregate3(
    .ph1_df = ph1_data, 
    .ph2_sp = ph2_sp_all, 
    .class_d = lc_no, 
    .attr_y = !!sym(x), 
    .attr_x = sp_area, 
    .aoi_area = 23680000
  )
  
})

res3_all[[1]]

res3_all <- flatten(res3_all)
names(res3_all) <- as.vector(outer(c("plot", "subpop_stratum", "subpop", "totals", "totals_short"), vec_pools, paste, sep = "_"))

## + + Save all plot data ####
plots_all <- res3_all[str_detect(names(res3_all), "plot_")] |>
  list_rbind() |>
  pivot_wider(names_from = attr, names_prefix = "yid_", values_from = yid)

write_csv(plots_all, file.path(path$res$data, paste0("plot-summary-allvar", Sys.Date(),".csv")))

## + + Save all subpop_stratum ####
subpop_stratum_all <- res3_all[str_detect(names(res3_all), "subpop_stratum")] |>
  list_rbind()

write_csv(subpop_stratum_all, file.path(path$res$data, paste0("subpop_stratum-allvar", Sys.Date(),".csv")))




## + Combine province results
res3_prov <- map(c("agb", "bgb", "sap_agb", "dw", "stump", "ldw", "Ctot"), function(x){
  
  tt <- nfi_aggregate3(
    .ph1_df = ph1_data, 
    .ph2_sp = ph2_sp_all, 
    .class_d = lc_no, 
    .attr_y = !!sym(x), 
    .attr_x = sp_area, 
    .aoi_area = 23680000
  )
  
  tt$totals_short |> 
    filter(lc_no < 30 | lc_no > 160) |>
    left_join(tmp_lc, by = join_by(lc_no)) |>
    select(lc_no, lc_code, everything())
  
}) |> list_rbind()

write_csv(res3, file.path(path$res$data, paste0("res3-allpools-", Sys.Date(),".csv")))




