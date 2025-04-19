
## Initiate list to store temporary objects
tmp <- list()


tmp$time <- local_time(usr$time_zone, .show_tz = F, .spe_chr = F)

walk(names(data_prep), function(x){
  
  write_csv(data_prep[[x]], file.path(path$dat$prep, paste0(x, "_", tmp$time, ".csv")))
  
})

## Remove tmp objects
rm(tmp)
