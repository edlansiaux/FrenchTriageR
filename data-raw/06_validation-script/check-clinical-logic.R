#' Validation de la logique clinique
#'
#' @description Vérifie la cohérence des décisions de triage

library(assertr)
library(readr)

# Chargement des données
merged_data <- readRDS("../processed-data/merged_triage_data.rds")

# 1. Vérification des seuils vitaux
merged_data %>%
  assert(within_bounds(50, 250), pas) %>%
  assert(within_bounds(30, 250), fc) %>%
  assert(in_set(c("Normal", "Anormal", "Inconnu")), ecg)

# 2. Cohérence motif/niveau
merged_data %>%
  filter(triage_level == 1) %>%
  assert(in_set(c("Arrêt cardio", "Détresse respiratoire")), motif)

# 3. Vérification des comorbidités
merged_data %>%
  filter(comorbidite == "Oui") %>%
  verify(!is.na(comorbidite_type))

# 4. Délais de prise en charge
merged_data %>%
  group_by(triage_level) %>%
  summarise(
    max_delay = max(delai_prise_en_charge, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  left_join(french_levels, by = c("triage_level" = "level")) %>%
  assert(within_bounds(0, max_delay_mins), max_delay)

message("Validation clinique réussie pour ", nrow(merged_data), " observations")
