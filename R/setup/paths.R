## paths
path     <- list()
path$dat <- list()
path$res <- list()

path$dat$parent <- here("data")
path$dat$src    <- here(file.path(path$dat$parent, "data-source"))
path$dat$anci   <- here(file.path(path$dat$parent, "data-ancillary"))
path$dat$prep   <- here(file.path(path$dat$parent, "data-prepared"))
path$dat$clean  <- here(file.path(path$dat$parent, "data-clean"))

path$res$parent <- here("results")
path$res$data   <- here(file.path(path$res$parent, "data-outputs"))
path$res$test   <- here(file.path(path$res$parent, "tests"))
path$res$fig    <- here(file.path(path$res$parent, "figures"))
path$res$tab    <- here(file.path(path$res$parent, "tables"))
path$res$html   <- here(file.path(path$res$parent, "html"))
path$res$report <- here(file.path(path$res$parent, "reports"))


## Create directories
walk(path$dat, dir.create, showWarning = F)
walk(path$res, dir.create, showWarning = F)

## Ignore data and results files in Git
## .gitignore edited manually instead
# usethis::use_git_ignore(path$dat$parent)
# usethis::use_git_ignore(path$res$parent)


