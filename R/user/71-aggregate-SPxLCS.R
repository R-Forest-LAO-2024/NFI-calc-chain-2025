
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

## Edit aggregation function inputs if necessary
ph1_data <- ph1_data |> mutate(subpop = 1)
ph2_sp_all <- ph2_sp_all |> mutate(subpop = 1)

## Save input tables
if (usr$save_csv) write_csv(ph1_data, file.path(path$res$data, paste0(save_pre, "ph1-info-", Sys.Date(), ".csv")))
if (usr$save_csv) write_csv(ph2_sp_all, file.path(path$res$data, paste0(save_pre, "ph2-subplot-allvars-", Sys.Date(), ".csv")))



##
## USING FUNCTION 3: ratio estimator, 2 phase sampling for stratification ####
##

# ## + AGB for demo ####
# res3_agb <- nfi_aggregate3(
#   .ph1_df = ph1_data, 
#   .ph2_sp = ph2_sp_all, 
#   .class_d = lc_no, 
#   .attr_y = agb, 
#   .attr_x = sp_area, 
#   .aoi_area = 23680000
#   ) 

## + All pools ####

res3_list <- make_outputs(.pools = vec_pools, .fct_no = 3)

res3_names <- names(res3_list)

res3_names_noplot <- res3_names[res3_names != "plot_summary"]

## + Create plot output with per ha values ####
plot_out <- res3_list$plot_summary |>
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


## + Make XLSX table ####
res3_list$plot_output <- plot_out
res3_list$plot_unique <- plot_unique
res3_list$ph1_data <- ph1_data
res3_list$ph2_subplot <- ph2_sp_all

res3_list <- res3_list[c("ph1_data", "ph2_subplot", "plot_summary", "plot_output", "plot_unique", res3_names_noplot)]

writexl::write_xlsx(res3_list, file.path(path$res$data, paste0(save_pre, "all-results-", Sys.Date(),"-no-subpop.xlsx")))

## Make spatial plot tables
sf_plot <- res3_list$plot_out |>
  mutate(x = plot_lon, y = plot_lat) |>
  st_as_sf(coords = c("x", "y"), crs = 4326)

st_write(sf_plot, file.path(path$res$data, str_replace_all(paste0(save_pre, "plot-summary-", Sys.Date(), ".kml"), "-", "_")), delete_dsn = TRUE, quiet = T)
st_write(sf_plot, file.path(path$res$data, paste0(save_pre, "plot-summary-", Sys.Date(), ".geojson")), delete_dsn = TRUE, quiet = T)


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
