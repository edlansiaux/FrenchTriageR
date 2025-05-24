#' Validation des données de sortie
#'
#' @description Contrôle qualité final avant intégration

library(tidyverse)
library(assertr)

final_data <- readRDS("../processed-data/final_dataset.rds")

# 1. Intégrité des données
final_data %>%
  assert(not_na, triage_level, pas, fc, SpO2) %>%
  assert(in_set(1:6), triage_level)

# 2. Cohérence temporelle
final_data %>%
  filter(arrival_date > as.Date("2020-01-01")) %>%
  verify(arrival_date <= Sys.Date())

# 3. Analyse des valeurs aberrantes
stats_report <- final_data %>%
  group_by(hospital_id) %>%
  summarise(
    avg_pas = mean(pas, na.rm = TRUE),
    sd_pas = sd(pas, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    pas_zscore = abs(avg_pas - 120)/sd_pas
  )

# 4. Vérification des métadonnées
stopifnot(
  "Les colonnes obligatoires manquent" =
    all(c("patient_id", "triage_level") %in% names(final_data)),
  "Valeurs de triage invalides" =
    all(final_data$triage_level %in% french_levels$level))
)

# Génération du rapport
validation_log <- list(
  timestamp = Sys.time(),
  n_records = nrow(final_data),
  failed_checks = list(
    missing_values = sum(is.na(final_data$triage_level)),
    date_anomalies = sum(final_data$arrival_date > Sys.Date())
  )
)

saveRDS(validation_log, "../reports/validation_report.rds")

message("Validation terminée. Rapport sauvegardé dans reports/")
