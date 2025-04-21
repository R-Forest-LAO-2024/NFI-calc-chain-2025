
##
## DO NOT EDIT MANUALLY
##

## THIS SCRIPT IS GENERATED TO ASSIGN LIST ELEMENTS TO THE ENVIRONMENT
## IT GETS ELEMENTS FROM 'data_clean' to the environment as objects

tmp <- list()

tmp$names <- names(data_clean)

walk(tmp$names, function(x){ assign(x, data_clean[[x]], envir = .GlobalEnv) })

rm(tmp)