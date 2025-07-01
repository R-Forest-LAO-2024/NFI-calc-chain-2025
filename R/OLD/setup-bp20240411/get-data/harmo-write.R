
## Initiate list to store temporary objects
tmp <- list()


tmp$time <- local_time(usr$time_zone, .show_tz = F, .spe_chr = F)

walk(names(data_harmo), function(x){
  
  write_csv(data_harmo[[x]], file.path(path$dat$harmo, paste0(x, "-", tmp$time, ".csv")))
  
})

## Remove tmp objects
rm(tmp)