
## step 1: calculate d^2 or d^2 - d_hollow^2 as diameter
## step 2: volume / ha based on V = pi^2 / (8 * L) * sum(d^2) with V in m3/ha, L in m and d in cm (devries 1986)
## step 3: multiply by wood density to get kg/ha (wd in kg/m3 or g/cm3*1000)
## Here L = 32

ldw <- ldw |>
  mutate(
    d_sq = if_else(ldw_hollow_go == "yes", ldw_diameter^2  - ((ldw_hollow_d1 + ldw_hollow_d2) / 2)^2, ldw_diameter^2)
  )

ldw_aggregate <- ldw |>
  group_by(subplot_plot_no, subplot_no, ldw_density, lu_code_new) |>
  summarise(sum_d_sq = sum(d_sq), .groups = "drop") |>
  mutate(
    ldw_v_ha = round(pi^2 / (8 * 32) * sum_d_sq),
    ldw_wd = case_when(
      lu_code_new == "EG" & ldw_density == 1 ~ 0.39,
      lu_code_new == "EG" & ldw_density == 2 ~ 0.34,
      lu_code_new == "EG" & ldw_density == 3 ~ 0.26,
      lu_code_new == "MD" & ldw_density == 1 ~ 0.45,
      lu_code_new == "MD" & ldw_density == 2 ~ 0.30,
      lu_code_new == "MD" & ldw_density == 3 ~ 0.29,
      lu_code_new == "DD" & ldw_density == 1 ~ 0.44,
      lu_code_new == "DD" & ldw_density == 2 ~ 0.35,
      lu_code_new == "DD" & ldw_density == 3 ~ 0.32,
      ldw_density == 1 ~ 0.54,
      ldw_density == 2 ~ 0.46,
      ldw_density == 3 ~ 0.21,
      TRUE ~ NA_real_
    ),
    ldw_agb_ha_density = ldw_v_ha * ldw_wd * 1000, ## q1 g/cm3 = 1000 kg/m3
  ) |>
  group_by(subplot_plot_no, subplot_no) |>
  summarise(ldw_agb_ha = sum(ldw_agb_ha_density), .groups = "drop")
  
  





# 1 kg / 1 m3# 
# 1 000 g / 1 000 000 cm3
# 
# 1 kg/m3 = 1 g/cm3 / 1000
# 
# 1000 kg/m3 = 1 g/cm3