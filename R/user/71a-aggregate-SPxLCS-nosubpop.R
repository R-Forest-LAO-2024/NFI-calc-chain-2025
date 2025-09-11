
tmp <- list()

##
## Prepare aggregation ####
##

## + Prepare ancillary ####
tmp$lc <- anci$lc |> select(lc_no = lu_no, lc_code = lu_code_new, lc_name = lu_name, lc_name_lao = lu_name_lao)
tmp$plotgps <- anci$ceo |> 
  left_join(anci$ceo_nfi_id_all, by = join_by(pl_orig_fid)) |>
  filter(!is.na(plotid_all)) |>
  mutate(
    plot_lon = as.numeric(str_remove(pl_xy, ".*,")),
    plot_lat = as.numeric(str_remove(pl_xy, ",.*"))
  ) |>
  select(plot_id = plotid_all, prov_no = pl_pcode, prov_name = pl_prov, plot_lon, plot_lat) |>
  arrange(plot_id)

## + prepare function inputs ####
vec_pools <- c("agb", "bgb", "sap_agb", "dw", "stump", "ldw", "Ctot")

ph1 <- ph1_data |> mutate(subpop = 1)
ph2 <- ph2_subplot |> mutate(subpop = 1)

## + Prepare prefix for filenames ####
save_pre <- "res3-SPxLCS-nosubpop-strata-solve-"

## Save input tables
if (usr$save_csv) write_csv(ph1, file.path(path$res$data, paste0(save_pre, "ph1-info-", Sys.Date(), ".csv")))
if (usr$save_csv) write_csv(ph2, file.path(path$res$data, paste0(save_pre, "ph2-subplot-allvars-", Sys.Date(), ".csv")))


##
## Aggregate subplots to totals ####
##

## aggregation function: 3. ratio estimator, 2 phase sampling for stratification
## subpopulation: no


## + AGB for demo ####
# res3_agb <- nfi_aggregate3(
#   .ph1_df = ph1_data, 
#   .ph2_sp = ph2_subplot, 
#   .class_d = lc_no, 
#   .attr_y = agb, 
#   .attr_x = sp_area, 
#   .aoi_area = 23680000
#   ) 


## + All tables and pools as list ####
res <- map(vec_pools, function(x){
  
  nfi_aggregate3(
    .ph1_df = ph1, 
    .ph2_sp = ph2, 
    .class_d = lc_no, 
    .attr_y = !!sym(x), 
    .attr_x = sp_area, 
    .aoi_area = 23680000
  )
  
}) |> list_flatten()



## + Combine pools for each table execpt plot level ####

tab_names <- unique(names(res))
tab_names2 <- tab_names[!tab_names %in% "plot"]

res_list <- map(tab_names2, function(x){
  res[str_detect(names(res), x)] |> list_rbind()
})

names(res_list) <- tab_names2


## + Combine plot summary data for all pools as wide table ####
## Plot data need pivot_wider()

plot_summary <- res[str_detect(names(res), "plot")] |>
  list_rbind() |>
  pivot_wider(names_from = attr, names_prefix = "yid_", values_from = yid)


##
## Create plot output with per ha values ####
##

plot_final <- plot_summary |>
  left_join(tmp$lc, by = join_by(lc_no)) |>
  left_join(tmp$plotgps, by = join_by(plot_id)) |>
  mutate(
    AGB_tha   = round(yid_agb / xid, 3),
    BGB_tha   = round(yid_bgb / xid, 3),
    sapB_tha  = round(yid_sap_agb / xid, 3),
    liveB_tha = AGB_tha + BGB_tha + sapB_tha,
    AGC_tha   = AGB_tha * CF,
    BGC_tha   = BGB_tha * CF,
    sapC_tha  = sapB_tha * CF,
    liveC_tha = liveB_tha * CF,
    DW_tha    = round(yid_dw / xid, 3),
    stump_tha = round(yid_stump / xid, 3),
    LDW_tha   = round(yid_ldw / xid, 3),
    totC_tha  = liveC_tha + (DW_tha + stump_tha + LDW_tha) * CF
  ) |>
  select(plot_id, lc_no, lc_code, prov_no, prov_name, plot_lon, plot_lat, AGB_tha, AGC_tha, BGB_tha, BGC_tha, ends_with("_tha"), everything())

## + Make unique plot table ####
## Take majority AGB
tmp$plot_match_agb <- plot_final |> summarise(agb_max = max(yid_agb), .by = plot_id)

tmp$plot_unique_agb <- plot_final |> 
  semi_join(tmp$plot_match_agb, by = join_by(plot_id, yid_agb == agb_max))

tmp$plot_dup <- tmp$plot_unique_agb |>
  summarise(count = n(), .by = plot_id) |>
  filter(count > 1) |>
  pull(plot_id)

## Take min LC for the remaining dups (all have 0 AGB)
tmp$plot_match_lc <- plot_final |>
  filter(plot_id %in% tmp$plot_dup) |>
  summarise(lc_min = min(lc_no), .by = plot_id)

tmp$plot_unique_lc <- tmp$plot_unique_agb |>
  filter(plot_id %in% tmp$plot_dup) |>
  semi_join(tmp$plot_match_lc, by = join_by(plot_id, lc_no == lc_min))

plot_unique <- tmp$plot_unique_agb |>
  filter(!plot_id %in% tmp$plot_dup) |>
  bind_rows(tmp$plot_unique_lc)


## + Make XLSX table ####
res_list$plot_summary <- plot_summary
res_list$plot_final  <- plot_final
res_list$plot_unique  <- plot_unique
res_list$ph1_data     <- ph1_data
res_list$ph2_subplot  <- ph2_subplot

res_list <- res_list[c("ph1_data", "ph2_subplot", "plot_summary", "plot_final", "plot_unique", tab_names2)]

writexl::write_xlsx(res_list, file.path(path$res$data, paste0(save_pre, "all-results-", Sys.Date(),".xlsx")))

## Make spatial plot tables
sf_plot <- res_list$plot_final |>
  mutate(x = plot_lon, y = plot_lat) |>
  st_as_sf(coords = c("x", "y"), crs = 4326)

st_write(sf_plot, file.path(path$res$data, str_replace_all(paste0(save_pre, "plot-final-", Sys.Date(), ".kml"), "-", "_")), delete_dsn = TRUE, quiet = T)
st_write(sf_plot, file.path(path$res$data, paste0(save_pre, "plot-final-", Sys.Date(), ".geojson")), delete_dsn = TRUE, quiet = T)

## + Keep results in new object ####
assign(str_replace_all(save_pre, "-", "_"), res_list)


## + Remove tmp objects ####
# rm(tmp, plot_summary, plot_final, plot_unique, res, tab_names, tab_names2)


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
# ## + Combine all totals simplified
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
