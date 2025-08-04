# 
# 
# ## This function is part of a series of aggregation functions for NFI analysis
# ## function 3: double sampling for stratification with ratio estimator
# 
# ## .ph1_df: Phase 1 plot level data frame, should contain the following columns: 
# ##    - plot_id   : plot ID of the Phase 2 plots, 
# ##    - subpop : Subpopulation ID for the Phase 1 plots,  
# ##    - stratum: Stratum ID for the Phase 1 plots
# ## .ph2_sp: Phase 2 data frame at the smallest measurement area, subplot or land vegetation class. Should contain the following columns:
# ##    - plot_id   : plot ID of the phase 2 plots, matching plot_no from the Phase 1 plot data frame, 
# ##    - subplot_id: Subplot ID (see .subplot_id),
# ##    - class_d   : domain of interest, for example forest classes.
# ##    - access    : accessibility of each subplot (TRUE/FALSE)
# ##    - An attribute x, at subplot level, denominator of the Ratio, R = Y / X (See .attr_x). It normally is the maximum measured area (in ha) at subplot level, i.e. largest nested measurement level for the attribute of interest,
# ## .ph2_en: Phase 2 data frame at the entity level (tree, deadwood, etc.)
# ##    - plot_id: Phase 2 plot ID same as phase 1 plots and phase 2 suplots.
# ##    - meas_area: area where the attribute y is measured, in ha, important for nested subplots.
# ##    - An attribute y at entity level, attribute of interest, with the same unit as the expected reporting unit (ton for biomass, m3 for volume, m2 for basal area, etc.)
# ## .class_d: domain values (in supblot table)
# ## .attr_y: attribute of interest (in entity table), numerator of the Ratio. Best use carbon pool name.
# ## .attr_x: attribute of interest (in subplot_table), denominator of the Ratio. Most often the largest measurement area at the subplot or subplot x lcs level.
# ## .aoi_area: total area of the area of interest (in ha). In case of NFIs, country area.
# 
# 
# nfi_aggregate3 <- function(.ph1_df, .ph2_sp, .ph2_en, .class_d, .attr_y, .attr_x, .aoi_area){
#   
#   ## !!! FOR TESTING ONLY
#   # .ph1_df = ph1_data
#   # .ph2_sp = ph2_subplot
#   # .ph2_en = tree2
#   # class_d = quo(lc_no)
#   # attr_x  = quo(sp_area)
#   # attr_y  = quo(agb)
#   # .aoi_area = 23680000
#   ## !!!
#   
#   
#   ## Setup ####
#   
#   ## + Defuse input column name ####
#   class_d <- enquo(.class_d)
#   attr_y  <- enquo(.attr_y)
#   attr_x  <- enquo(.attr_x)
#   
#   ## + Prepare constants ####
#   Ntot  <- nrow(.ph1_df)
#   Nstrat <- length(unique(.ph2_sp$stratum))
#   
#   vec_subpop  <- .ph2_sp |> pull(subpop) |> unique() |> sort()
#   vec_stratum <- .ph2_sp |> pull(stratum) |> unique() |> sort()
#   vec_d       <- .ph2_sp |> pull(!!class_d) |> unique() |> sort()
#   
#   
#   ## Aggregate entity to subplot ####
#   
#   sp_attr <- .ph2_en |>
#     group_by(plot_id, subplot_id) |>
#     summarise(
#       entity_count_ha = sum(1 / meas_area),
#       entity_value_ha = sum(!!attr_y / meas_area),
#       .groups= "drop"
#     ) |>
#     right_join(.ph2_sp, by = join_by(plot_id, subplot_id)) |>
#     mutate(
#       entity_count = if_else(!is.na(entity_count_ha), entity_count_ha * !!attr_x, 0),
#       entity_value = if_else(!is.na(entity_value_ha), entity_value_ha * !!attr_x, 0)
#     ) |>
#     select(plot_id, subplot_id, entity_count_ha, entity_value_ha, entity_count, entity_value, subpop, stratum, !!attr_x, access, !!class_d) |>
#     arrange(plot_id, subplot_id)
#   
#   
#   ## Aggregate subplot to plot ####
#   
#   plot_a <- sp_attr |>
#     filter(access) |>
#     group_by(plot_id, subpop, stratum) |>
#     summarise(
#       ai = sum(!!attr_x), 
#       .groups = "drop"
#     ) 
#   
#   plot_attr <- sp_attr |>
#     filter(access) |>
#     group_by(plot_id, subpop, stratum, !!class_d) |>
#     summarise(
#       count_sp = n(),
#       yid = sum(entity_value),
#       xid = sum(!!attr_x),
#       .groups = "drop"
#     ) |> 
#     left_join(plot_a, by = join_by(plot_id, subpop, stratum))
#   
#   
#   ## Aggregate plot to subpop x stratum ####
#   
#   ## + Prepare strata weight and areas ####
#   ss_ph1_totals <- .ph1_df |>
#     group_by(subpop) |>
#     summarise(N = n(), .groups = "drop")
#   
#   ss_ph1 <- .ph1_df |>
#     group_by(subpop, stratum) |>
#     summarise(Nh = n(), .groups = "drop") |>
#     left_join(ss_ph1_totals, by = join_by(subpop)) |>
#     mutate(
#       Wh = Nh/N, 4,
#       Ah = .aoi_area * Nh / Ntot
#       )
#   
#   ss_ph2_totals <- plot_attr |>
#     distinct(plot_id, subpop, stratum) |>
#     group_by(subpop) |>
#     summarise(n = n(), .groups = "drop")
#   
#   ss_ph2 <- plot_attr |>
#     distinct(plot_id, subpop, stratum) |>
#     group_by(subpop, stratum) |>
#     summarise(nh = n(), .groups = "drop") |>
#     left_join(ss_ph2_totals, by = join_by(subpop)) |>
#     mutate(wh = nh/n, 4)
#   
#   ## + Initiate subpop x stratum ####
#   subpop_stratum_init <- expand_grid(
#     !!class_d := vec_d,
#     subpop = vec_subpop,
#     stratum = vec_stratum,
#   ) 
#   
#   ## + Aggregate attributes ####
#   subpop_stratum_a <- plot_a |>
#     group_by(subpop, stratum) |>
#     summarise(
#       sum_ai    = sum(ai),
#       sum_ai_sq = sum(ai^2),
#       .groups = "drop"
#       )
#   
#   subpop_stratum_attr <- plot_attr |>
#     group_by(subpop, stratum, !!class_d) |>
#     summarise(
#       count_plot_h = n(),
#       count_sp     = sum(count_sp),
#       sum_yid      = sum(yid),
#       sum_yid_sq   = sum(yid^2),
#       sum_yid_ai   = sum(yid * ai),
#       sum_xid      = sum(xid),
#       sum_xid_sq   = sum(xid^2),
#       sum_xid_ai   = sum(xid * ai),
#       sum_yid_xid  = sum(yid * xid),
#       .groups = "drop"
#     ) 
#   
#   ## + Combine all ####
#   subpop_stratum_d <- subpop_stratum_init |>
#     left_join(ss_ph1, by = join_by(subpop, stratum)) |>
#     left_join(ss_ph2, by = join_by(subpop, stratum)) |>
#     left_join(subpop_stratum_attr, by = join_by(subpop, stratum, !!class_d)) |>
#     left_join(subpop_stratum_a, by = join_by(subpop, stratum)) |>
#     mutate(
#       mean_yd = if_else(sum_ai > 0, sum_yid / sum_ai, 0),
#       mean_xd = if_else(sum_ai > 0, sum_xid / sum_ai, 0)
#     ) |>
#     mutate(across(where(is.numeric), \(x) replace_na(x, 0))) |>
#     arrange(!!class_d, subpop, stratum)
#   
#   ## + Add sub-population mean ####
#   ## >> NOT NEEDED for function 3. Needed for double sampling for post-stratification
#   # subpop_mean_d <- subpop_stratum_d |>
#   #   group_by(!!class_d, subpop) |>
#   #   summarise(
#   #     subpop_mean_yd = sum(mean_yd * Wh),
#   #     subpop_mean_xd = sum(mean_xd * Wh),
#   #     .groups = "drop"
#   #   )
#   # 
#   # subpop_stratum_d <- subpop_stratum_d |>
#   #   mutate(
#   #     subpop_mean_yd = NA,
#   #     subpop_mean_xd = NA
#   #   ) |>
#   #   left_join(subpop_mean_d, by = join_by(!!class_d, subpop), suffix = c("_rm", "")) |>
#   #   select(-ends_with("_rm"))
#   
#   ## + Calc variance ####
#   subpop_stratum_d <- subpop_stratum_d |>
#     mutate(
#       var_yd          = if_else(nh > 1 & sum_ai > 0, nh^2/(nh - 1) * (sum_yid_sq - 2*mean_yd*sum_yid_ai + mean_yd^2*sum_ai_sq) / (sum_ai^2), 0),
#       var_xd          = if_else(nh > 1 & sum_ai > 0, nh^2/(nh - 1) * (sum_xid_sq - 2*mean_xd*sum_xid_ai + mean_xd^2*sum_ai_sq) / (sum_ai^2), 0),
#       covar_xdyd      = if_else(nh > 1 & sum_ai > 0, nh^2/(nh - 1) * (sum_yid_xid - mean_yd*sum_xid_ai - mean_xd*sum_yid_ai + mean_yd*mean_xd*sum_ai_sq) / (sum_ai^2), 0),
#       var_mean_yd     = if_else(Nh > 1 & n > 0, Wh * (Nh - 1) / (N - 1) * var_yd / nh, 0),
#       var_mean_xd     = if_else(Nh > 1 & n > 0, Wh * (Nh - 1) / (N - 1) * var_xd / nh, 0),
#       covar_mean_xdyd = if_else(Nh > 1 & n > 0, Wh * (Nh - 1) / (N - 1) * covar_xdyd / nh, 0)
#     )
#   
#   ## Aggregate to sub-populations ####
#   
#   subpop_d <- subpop_stratum_d |>
#     group_by(!!class_d, subpop) |>
#     summarise(
#       count_plot     = sum(count_plot_h),
#       subpop_mean_yd = sum(mean_yd * Wh),
#       subpop_mean_xd = sum(mean_xd * Wh),
#       subpop_var_yd  = sum(var_mean_yd),
#       subpop_var_xd  = sum(var_mean_xd),
#       subpop_covar_xdyd = sum(covar_mean_xdyd),
#       subpop_n       = sum(nh),
#       subpop_N       = sum(Nh),
#       subpop_A       = sum(Ah),
#       .groups = "drop"
#     ) |>
#     mutate(
#       subpop_W      = subpop_N / Ntot,
#       subpop_Rd     = if_else(subpop_mean_xd > 0, subpop_mean_yd / subpop_mean_xd, 0),
#       subpop_Rd_var = if_else(subpop_mean_xd > 0, abs((subpop_var_yd + subpop_Rd^2*subpop_var_xd - 2*subpop_Rd*subpop_covar_xdyd) / (subpop_mean_xd^2)), 0),
#       subpop_Rd_se  = sqrt(subpop_Rd_var),
#       subpop_Rd_me  = qt(1-0.1/2, df = subpop_n - Nstrat) * subpop_Rd_se,
#       subpop_Rd_mep = if_else(subpop_Rd > 0, round(subpop_Rd_me / subpop_Rd * 100, 1), 0)
#     )
#   
#   
#   ## Totals ####
#   
#   totals_d <- subpop_d |>
#     group_by(!!class_d) |>
#     summarise(
#       total_plot = sum(count_plot),
#       Yd         = sum(subpop_mean_yd * subpop_W),
#       Yd_var     = sum(subpop_var_yd * subpop_W^2),
#       Xd         = sum(subpop_mean_xd * subpop_W),
#       Xd_var     = sum(subpop_var_xd * subpop_W^2),
#       covar_XdYd = sum(subpop_covar_xdyd * subpop_W^2),
#       Ytot       = sum(subpop_mean_yd * subpop_A),
#       Xtot       = sum(subpop_mean_xd * subpop_A),
#       .groups = "drop"
#     ) |>
#     mutate(
#       Yd_mep = if_else(Yd != 0, round(qt(1-0.1/2, df = Inf) * sqrt(Yd_var) / Yd * 100, 1), 0),
#       Xd_mep = if_else(Xd != 0, round(qt(1-0.1/2, df = Inf) * sqrt(Xd_var) / Xd * 100, 1), 0),
#       Rd     = round(Yd / Xd, 3),
#       Rd_var = (Yd_var + Rd^2*Xd_var - 2*Rd*covar_XdYd)/Xd^2,
#       ## Margin of Error = half width of confidence interval, mep = percentage margin of error
#       Rd_mep = if_else(Rd != 0, round(qt(1-0.1/2, df = Inf) * sqrt(Rd_var) / Rd * 100, 1), 0)
#     ) |>
#     select(!!class_d, total_plot, Yd, Yd_mep, Xd, Xd_mep, Rd, Rd_mep, Ytot, Xtot)
#   totals_d
#   
#   out_d <- totals_d  |> 
#     mutate(attr = as_label(attr_y)) |>
#     select(!!class_d, attr, N_pp = total_plot, Rd, Rd_mep)
#   
#   out <- list(
#     suplot = sp_attr, 
#     plot = plot_attr, 
#     subpop_stratum = subpop_stratum_d, 
#     subpop = subpop_d,
#     totals = totals_d,
#     totals_short = out_d
#     )
#   
#   out
#   
# }
# 
