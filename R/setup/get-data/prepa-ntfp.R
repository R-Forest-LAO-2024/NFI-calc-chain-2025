
## Rename columns and convert text to numeric

data_prep$ntfp <- data_init$ntfp_init |>
  rename(
    nftp_no = ONA_no,
    ntfp_type = ntfp_data__ntfp_types,
    ntfp_species = ntfp_data__ntfp_species,
    ntfp_species_other = ntfp_data__ntfp_species_oth,
  ) |>
  mutate(
    ntfp_number = as.numeric(ntfp_data__ntfp_nb)
      ) |>
  select(starts_with("ONA_"), starts_with("ntfp_"))
