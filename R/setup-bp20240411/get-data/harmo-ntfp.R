
data_harmo$ntfp <- data_init$ntfp_init |>
  mutate(
    ntfp_no = as.numeric(ntfp_data__ntfp_nb),
    ntfp_type = ntfp_data__ntfp_types,
    ntfp_species = ntfp_data__ntfp_species,
    ntfp_species_other = ntfp_data__ntfp_species_oth
      )