tt <- rstudioapi::askForPassword("enter your ID to connect to ONA")

path <- tempdir()

download.file(
  url = rstudioapi::askForSecret("URL"),
  destfile = path)
