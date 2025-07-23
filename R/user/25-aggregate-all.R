
tmp_lc <- anci$lc |> select(lc_no = lu_no, lc_code = lu_code_new)

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


## Combine all ####
res3 <- map(c("agb", "bgb", "sap_agb", "dw", "stump", "ldw", "Ctot"), function(x){
  
  tt <- nfi_aggregate3(
    .ph1_df = ph1_data, 
    .ph2_sp = ph2_sp_all, 
    .class_d = lc_no, 
    .attr_y = !!sym(x), 
    .attr_x = sp_area, 
    .aoi_area = 23680000
  )
  
  tt$totals_short |> filter(lc_no < 30 | lc_no > 160)
  
}) |> list_rbind()





