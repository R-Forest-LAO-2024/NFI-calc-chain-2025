
if (usr$save_csv) {
  
  write_csv(tree, file.path(path$res$data, paste0("tree_with_joins_calc-", Sys.Date(), ".csv")))
  
  write_csv(dw, file.path(path$res$data, paste0("dw_with_joins_calc-", Sys.Date(), ".csv")))
  
  write_csv(ldw_aggregate, file.path(path$res$data, paste0("ldw_subplot-with_joins_calc-", Sys.Date(), ".csv")))
  
  write_csv(stump, file.path(path$res$data, paste0("stump_with_joins_calc-", Sys.Date(), ".csv")))
  
  write_csv(sapling, file.path(path$res$data, paste0("sapling_with_joins_calc-", Sys.Date(), ".csv")))
  
  ## NTFP TO BE DONE
  # write_csv(ntfp, file.path(path$res$data, paste0("ntfp_with_joins_calc-", Sys.Date(), ".csv")))
  
}
