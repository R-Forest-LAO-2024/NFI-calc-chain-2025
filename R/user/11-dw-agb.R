
dw <- dw |>
  mutate(
    dw_agb = case_when(
      dw_class == "decay 1"       ~ 0.6 * exp(-1.499 + (2.148 * log(dw_dbh)) + (0.207 * (log(dw_dbh))^2) - (0.0281 * (log(dw_dbh))^3)),
      dw_class == "decay 2 short" ~ pi * dw_bole_height * 100 / 12 * (dw_dbase^2 + dw_dbase * dw_dtop + dw_dtop^2) * 0.6 * 0.001, ## Convert H to cm then wd in g.cm-3 with WD = 0.6 then g to kg with 0.001 
      dw_class == "decay 2 tall"  ~ pi * dw_bole_height * 100 / 12 * (dw_dbase^2 + dw_dbase * dw_dtop + dw_dtop^2) * 0.6 * 0.001,
      TRUE ~ NA_real_
    )
  )

## Check
summary(dw$dw_agb)

