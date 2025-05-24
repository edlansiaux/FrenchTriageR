source("01_import-raw-data.R")

clean_data <- raw_hospA %>%
  mutate(
    triage_level = case_when(
      str_detect(motif, "Urgence vitale") ~ 1,
      # ... règles de nettoyage spécifiques FRENCH
    ),
    # Normalisation des unités médicales
    pas = as.numeric(str_extract(pression_arterielle, "\\d+")),
    # Gestion des valeurs aberrantes
    temperature = ifelse(temperature > 45, NA, temperature)
  )

saveRDS(clean_data, "processed-data/clean_french_triage.rds")
