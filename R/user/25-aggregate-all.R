
tmp_lc <- anci$lc |> select(lc_no = lu_no, lc_code = lu_code_new)

##
## USING FUNCTION 3: ratio estimator, 2 phase sampling for stratification ####
##

## + AGB ####
allres3_agb <- nfi_aggregate3(
  .ph1_df = ph1_data, 
  .ph2_sp = ph2_subplot, 
  .ph2_en = tree2, 
  .class_d = lc_no, 
  .attr_y = agb, 
  .attr_x = sp_area, 
  .aoi_area = 23680000
  ) 

res3_agb <- allres3_agb$totals_short |> 
  filter(lc_no <= 22 | lc_no >= 160) |>
  left_join(tmp_lc, by = join_by(lc_no)) |>
  select(lc_no, lc_code, everything())

## + BGB ####
allres3_bgb <- nfi_aggregate3(
  .ph1_df = ph1_data, 
  .ph2_sp = ph2_subplot, 
  .ph2_en = tree2, 
  .class_d = lc_no, 
  .attr_y = bgb, 
  .attr_x = sp_area, 
  .aoi_area = 23680000
)

res3_bgb  <- allres3_bgb$totals_short |> 
  filter(lc_no <= 22 | lc_no >= 160) |>
  left_join(tmp_lc, by = join_by(lc_no)) |>
  select(lc_no, lc_code, everything())

## + Sapling ####
## TBD

## + DW (standing) ####
allres3_dw <- nfi_aggregate3(
  .ph1_df = ph1_data, 
  .ph2_sp = ph2_subplot, 
  .ph2_en = dw2, 
  .class_d = lc_no, 
  .attr_y = dw, 
  .attr_x = sp_area, 
  .aoi_area = 23680000
)

res3_dw  <- allres3_dw$totals_short |> 
  filter(lc_no <= 22 | lc_no >= 160) |>
  left_join(tmp_lc, by = join_by(lc_no)) |>
  select(lc_no, lc_code, everything())
res3_dw

## + Stumps ####
allres3_stump <- nfi_aggregate3(
  .ph1_df = ph1_data, 
  .ph2_sp = ph2_subplot, 
  .ph2_en = stump2, 
  .class_d = lc_no, 
  .attr_y = stump, 
  .attr_x = sp_area, 
  .aoi_area = 23680000
)

res3_stump  <- allres3_stump$totals_short |> 
  filter(lc_no <= 22 | lc_no >= 160) |>
  left_join(tmp_lc, by = join_by(lc_no)) |>
  select(lc_no, lc_code, everything())
res3_stump


## + ldw ####
## TBD

## + Combine f3 ####
res3 <- bind_rows(res3_agb, res3_bgb, res3_dw, res3_stump)

write_csv(res3, file.path(path$res$data, paste0("res3-", Sys.Date(), ".csv")))




