
tmp_lc <- anci$lc |> select(lc_no = lu_no, lc_code = lu_code_new)
tmp_plotgps <- anci$ceo |> 
  left_join(anci$ceo_nfi_id_all, by = join_by(pl_orig_fid)) |>
  filter(!is.na(plotid_all)) |>
  mutate(
    plot_lon = as.numeric(str_remove(pl_xy, ".*,")),
    plot_lat = as.numeric(str_remove(pl_xy, ",.*"))
  ) |>
  select(plot_id = plotid_all, prov_no = pl_pcode, prov_name = pl_prov, plot_lon, plot_lat) |>
  arrange(plot_id)
  

vec_pools <- c("agb", "bgb", "sap_agb", "dw", "stump", "ldw", "Ctot")

save_csv <- F
save_pre <- "res3-SPxLCS-"

## Save input tables
if (save_csv) write_csv(ph1_data, file.path(path$res$data, paste0(save_pre, "ph1-info-", Sys.Date(), ".csv")))
if (save_csv) write_csv(ph2_sp_all, file.path(path$res$data, paste0(save_pre, "ph2-subplot-allvars-", Sys.Date(), ".csv")))




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

if (save_csv) write_csv(allres3_agb$plot, file.path(path$res$data, paste0(save_pre, "plot-summary-live-tree-agb.csv")))




## + Combine everything  ####
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

res3_all <- flatten(res3_all)
names(res3_all) <- as.vector(outer(c("plot", "subpopstratum", "subpop", "totals", "totalshort"), vec_pools, paste, sep = "_"))

## + + Save all plot data ####
plot_all <- res3_all[str_detect(names(res3_all), "plot_")] |>
  list_rbind() |>
  pivot_wider(names_from = attr, names_prefix = "yid_", values_from = yid)

if (save_csv) write_csv(plot_all, file.path(path$res$data, paste0(save_pre, "plot-summary-allvar", Sys.Date(),".csv")))

## + + Save all subpop_stratum ####
subpop_stratum_all <- res3_all[str_detect(names(res3_all), "subpopstratum")] |>
  list_rbind()

if (save_csv) write_csv(subpop_stratum_all, file.path(path$res$data, paste0(save_pre, "subpopstratum-allvar", Sys.Date(),".csv")))

## + + Save all subpop ####
subpop_all <- res3_all[str_detect(names(res3_all), "subpop_")] |>
  list_rbind()

if (save_csv) write_csv(subpop_all, file.path(path$res$data, paste0(save_pre, "subpop-allvar", Sys.Date(),".csv")))


## + + Save all totals ####
totals_all <- res3_all[str_detect(names(res3_all), "totals_")] |>
  list_rbind()

if (save_csv) write_csv(totals_all, file.path(path$res$data, paste0(save_pre, "totals-allvar", Sys.Date(),".csv")))


## + + Save all totals simplified ####
totalshort_all <- res3_all[str_detect(names(res3_all), "totalshort_")] |>
  list_rbind()

if (save_csv) write_csv(totalshort_all, file.path(path$res$data, paste0(save_pre, "totals-short-allvar", Sys.Date(),".csv")))

res3_list <- list(
  ph1_data       = ph1_data,
  ph2_sp_all     = ph2_sp_all,
  plot_summary   = plot_all,
  subpop_stratum = subpop_stratum_all,
  subpop         = subpop_all,
  totals         = totals_all,
  totals_short   = totalshort_all
)

writexl::write_xlsx(res3_list, file.path(path$res$data, paste0(save_pre, "all-results", Sys.Date(),".xlsx")))


## + Combine province results
# res3_prov <- map(c("agb", "bgb", "sap_agb", "dw", "stump", "ldw", "Ctot"), function(x){
#   
#   tt <- nfi_aggregate3(
#     .ph1_df = ph1_data, 
#     .ph2_sp = ph2_sp_all, 
#     .class_d = lc_no, 
#     .attr_y = !!sym(x), 
#     .attr_x = sp_area, 
#     .aoi_area = 23680000
#   )
#   
#   tt$totals_short |> 
#     filter(lc_no < 30 | lc_no > 160) |>
#     left_join(tmp_lc, by = join_by(lc_no)) |>
#     select(lc_no, lc_code, everything())
#   
# }) |> list_rbind()
# 
# if (save_csv) write_csv(res3, file.path(path$res$data, paste0(save_pre, "allpools-", Sys.Date(),".csv")))



# ## NOT NEEDED: showcasing less abstraction
# ## + Combine all totals simplified ####
# res3_simple <- map(vec_pools, function(x){
#   
#   tt <- nfi_aggregate3(
#     .ph1_df = ph1_data, 
#     .ph2_sp = ph2_sp_all, 
#     .class_d = lc_no, 
#     .attr_y = !!sym(x), 
#     .attr_x = sp_area, 
#     .aoi_area = 23680000
#   )
#   
#   tt$totals_short |>
#     filter(lc_no < 30 | lc_no > 160) |>
#     left_join(tmp_lc, by = join_by(lc_no)) |>
#     select(lc_no, lc_code, everything())
#   
# }) |> list_rbind()
# 
# if (save_csv) write_csv(res3_simple, file.path(path$res$data, paste0(save_pre, "simple-allpools-", Sys.Date(),".csv")))
