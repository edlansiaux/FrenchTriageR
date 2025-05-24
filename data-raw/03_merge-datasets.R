#' Fusion des sources de données hospitalières
#'
#' @description Combine les données de différents hôpitaux en un jeu unique
#' @note Exécuter depuis le répertoire data-raw

library(dplyr)
library(readr)
library(lubridate)

# Chargement des données nettoyées
hosp_a <- readRDS("processed-data/clean_hospitalA.rds")
hosp_b <- readRDS("processed-data/clean_hospitalB.rds")

# Harmonisation des colonnes
common_cols <- intersect(names(hosp_a), names(hosp_b))

# Fusion principale
merged_data <- bind_rows(
  hosp_a %>% select(all_of(common_cols)),
  hosp_b %>%
    select(all_of(common_cols)) %>%
    rename(
      pression_arterielle = blood_pressure,
      motif = chief_complaint
    )
)

# Gestion des colonnes spécifiques
merged_data <- merged_data %>%
  mutate(
    hospital_id = case_when(
      source_file == "hospital-A" ~ "HOSP_A",
      source_file == "hospital-B" ~ "HOSP_B"
    ),
    arrival_datetime = ymd_hm(arrival_time)
  )

# Anonymisation finale
merged_data <- merged_data %>%
  select(-patient_name, -patient_address) %>%
  mutate(
    patient_id = openssl::md5(paste0(patient_id, hospital_id))
  )

# Sauvegarde
saveRDS(merged_data, "processed-data/merged_triage_data.rds")
write_csv(merged_data, "processed-data/merged_triage_data.csv")

message("Fusion terminée : ", nrow(merged_data), " observations combinées")
