
tmp_lc <- anci$lc |> select(lc_no = lu_no, lc_code = lu_code_new, lc_name = lu_name, lc_name_lao = lu_name_lao)
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

save_pre <- "res3-SPxLCS-"

## Save input tables
if (usr$save_csv) write_csv(ph1_data, file.path(path$res$data, paste0(save_pre, "ph1-info-", Sys.Date(), ".csv")))
if (usr$save_csv) write_csv(ph2_sp_all, file.path(path$res$data, paste0(save_pre, "ph2-subplot-allvars-", Sys.Date(), ".csv")))




##
## USING FUNCTION 3: ratio estimator, 2 phase sampling for stratification ####
##

# ## + AGB ####
# allres3_agb <- nfi_aggregate3(
#   .ph1_df = ph1_data, 
#   .ph2_sp = ph2_sp_all, 
#   .class_d = lc_no, 
#   .attr_y = agb, 
#   .attr_x = sp_area, 
#   .aoi_area = 23680000
#   ) 
# 
# res3_agb <- allres3_agb$totals_short |> 
#   filter(lc_no <= 22 | lc_no >= 160) |>
#   left_join(tmp_lc, by = join_by(lc_no)) |>
#   select(lc_no, lc_code, everything())
# 
# if (usr$save_csv) write_csv(allres3_agb$plot, file.path(path$res$data, paste0(save_pre, "plot-summary-live-tree-agb.csv")))




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

if (usr$save_csv) write_csv(plot_all, file.path(path$res$data, paste0(save_pre, "plot-summary-allvar", Sys.Date(),".csv")))

## + + Save all subpop_stratum ####
subpop_stratum_all <- res3_all[str_detect(names(res3_all), "subpopstratum")] |>
  list_rbind() |>
  filter(!lc_no %in% 30:90)

if (usr$save_csv) write_csv(subpop_stratum_all, file.path(path$res$data, paste0(save_pre, "subpopstratum-allvar", Sys.Date(),".csv")))

## + + Save all subpop ####
subpop_all <- res3_all[str_detect(names(res3_all), "subpop_")] |>
  list_rbind() |>
  filter(!lc_no %in% 30:90)

if (usr$save_csv) write_csv(subpop_all, file.path(path$res$data, paste0(save_pre, "subpop-allvar", Sys.Date(),".csv")))


## + + Save all totals ####
totals_all <- res3_all[str_detect(names(res3_all), "totals_")] |>
  list_rbind() |>
  filter(!lc_no %in% 30:90)

if (usr$save_csv) write_csv(totals_all, file.path(path$res$data, paste0(save_pre, "totals-allvar", Sys.Date(),".csv")))


## + + Save all totals simplified ####
totalshort_all <- res3_all[str_detect(names(res3_all), "totalshort_")] |>
  list_rbind() |>
  filter(!lc_no %in% 30:90)

if (usr$save_csv) write_csv(totalshort_all, file.path(path$res$data, paste0(save_pre, "totals-short-allvar", Sys.Date(),".csv")))


## + Make XLSX table ####
plot_out <- plot_all |>
  left_join(tmp_lc, by = join_by(lc_no)) |>
  left_join(tmp_plotgps, by = join_by(plot_id)) |>
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

## + + Make unique plot table ####
## Take majority AGB
plot_match_agb <- plot_out |> summarise(agb_max = max(yid_agb), .by = plot_id)

plot_unique_init <- plot_out |>
  semi_join(plot_match_agb, by = join_by(plot_id, yid_agb == agb_max))

plot_dup <- plot_unique_init |>
  summarise(count = n(), .by = plot_id) |>
  filter(count > 1) |>
  pull(plot_id)

## Take min LC for the remaining dups (all have 0 AGB)
plot_match_lc <- plot_out |>
  filter(plot_id %in% plot_dup) |>
  summarise(lc_min = min(lc_no), .by = plot_id)

plot_unique_lc <- plot_unique_init |>
  filter(plot_id %in% plot_dup) |>
  semi_join(plot_match_lc, by = join_by(plot_id, lc_no == lc_min))

plot_unique <- plot_unique_init |>
  filter(!plot_id %in% plot_dup) |>
  bind_rows(plot_unique_lc)

## Make a list of all tables
res3_list <- list(
  ph1_data       = ph1_data,
  ph2_sp_all     = ph2_sp_all,
  plot_summary   = plot_all,
  plot_output    = plot_out,
  plot_unique    = plot_unique,
  subpop_stratum = subpop_stratum_all,
  subpop         = subpop_all,
  totals         = totals_all,
  totals_short   = totalshort_all
)

writexl::write_xlsx(res3_list, file.path(path$res$data, paste0(save_pre, "all-results-", Sys.Date(),".xlsx")))

## Make spatial plot tables
sf_plot <- plot_out |>
  mutate(x = plot_lon, y = plot_lat) |>
  st_as_sf(coords = c("x", "y"), crs = 4326)

st_write(sf_plot, file.path(path$res$data, paste0(save_pre, "plot-summary-", Sys.Date(), ".kml")))
st_write(sf_plot, file.path(path$res$data, paste0(save_pre, "plot-summary-", Sys.Date(), ".geojson")))


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
