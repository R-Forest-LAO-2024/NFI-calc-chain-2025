
## This function take a list of outputs from fct_nfi_aggregate*()
## And remake the various levels of aggregation

make_outputs <- function(.pools, .fct_no){
  
  ## !!! FOR TESTING ONLY
  # .pools = vec_pools
  # .fct_no = 3
  ## !!!
  
  ## + Call function ####
  f_name <- paste0("nfi_aggregate", .fct_no)
  
  res <- map(.pools, function(x){
    
    get(f_name)(
      .ph1_df = ph1_data, 
      .ph2_sp = ph2_sp_all, 
      .class_d = lc_no, 
      .attr_y = !!sym(x), 
      .attr_x = sp_area, 
      .aoi_area = 23680000
    )
    
  }) |> list_flatten()
  
  ## + Rename list ####
  f_levels <- unique(names(res))
  
  ## + Save other tables
  f_levels2 <- f_levels[!f_levels %in% "plot"]
  
  out_list <- map(f_levels2, function(x){
    
    res[str_detect(names(res), x)] |> list_rbind()
    
  })
  
  names(out_list) <- f_levels2
  
  ## + Save plot data ####
  ## Plot data need pivot_wider()
  plot_summary <- res[str_detect(names(res), "plot")] |>
    list_rbind() |>
    pivot_wider(names_from = attr, names_prefix = "yid_", values_from = yid)
  
  out_list$plot_summary <- plot_summary
  
  out_list <- out_list[c("plot_summary", f_levels2)]
  out_list
}