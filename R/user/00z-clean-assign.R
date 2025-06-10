
##
## DO NOT EDIT MANUALLY
##

## THIS SCRIPT IS GENERATED TO ASSIGN LIST ELEMENTS TO THE ENVIRONMENT
## IT GETS ELEMENTS FROM 'data_clean' to the environment as objects

tmp <- list()

tmp$names <- names(data_clean)

walk(tmp$names, function(x){ assign(x, data_clean[[x]], envir = .GlobalEnv) })


## Save file to 'data/data-clean/'
tmp$time <- local_time("GMT", .spe_chr = F, .show_tz = F)

walk(tmp$names, function(x){ write_csv(data_clean[[x]], file.path(path$dat$clean, paste0(x, "-", tmp$time, ".csv"))) })


rm(tmp)