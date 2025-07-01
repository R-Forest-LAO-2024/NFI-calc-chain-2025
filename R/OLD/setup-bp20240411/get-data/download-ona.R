
## Initiate list to store temporary objects
tmp_ona <- list()

## Connect to ONA
tmp_ona$get <- httr::GET(paste0("https://api.ona.io/api/v1/data/", rstudioapi::askForSecret("ONA Form ID code"), ".csv"))

## Fetch data, make all character and remove special characters in col name
if (tmp_ona$get$status_code == 200) {

  tmp_ona$content <- httr::content(tmp_ona$get, type = "text/csv") |> 
    mutate(across(everything(), as.character)) |>
    rename_with(.cols = everything(), str_replace_all, "/", "__") |>
    rename_with(.cols = everything(), str_replace_all, "\\[|\\]", "___") |>
    rename_with(.cols = everything(), str_replace_all, "_____", "___") |>
    rename_with(.cols = starts_with("_"), str_replace, "_", "ONA_")
  
}

if (is.null(tmp_ona$content)) stop("Couldn't download data from API") 

## Get time
tmp_ona$time <- local_time(usr$time_zone, .show_tz = F, .spe_chr = F)
tmp_ona$file_name <- paste0("ONAapi_", tmp_ona$time, ".csv")

## Write file
write_csv(tmp_ona$content, file.path(path$dat$src, tmp_ona$file_name))

## pass data to main list
data_init$master_csv <- tmp_ona$content

## Update log
write_lines(
  paste0(
    local_time("UTC"), ": Data downloaded with method ", usr$get_method, "\n\n" 
  ),
  here("log.txt") , append = T
)

## Clean tmp_ona
rm(tmp_ona)
