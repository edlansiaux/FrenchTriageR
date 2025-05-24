#' Création des tables de référence
#'
#' @description Génère les données de référence pour l'algorithme FRENCH
#' @note À exécuter après la fusion des données

library(tidyverse)

# Niveaux de triage FRENCH
french_levels <- tibble(
  level = 1:6,
  label = c(
    "Urgence vitale",
    "Urgence absolue",
    "Urgence relative A",
    "Urgence relative B",
    "Urgence différée",
    "Non urgent"
  ),
  max_delay_mins = c(0, 20, 60, 60, 120, 240),
  color = c("#FF0000", "#FF4500", "#FFA500", "#FFFF00", "#00FF00", "#FFFFFF")
)

# Motifs médicaux référents
medical_motifs <- merged_data %>%
  count(motif, sort = TRUE) %>%
  mutate(
    category = case_when(
      str_detect(motif, regex("douleur thoracique", ignore_case = TRUE)) ~ "Cardiaque",
      str_detect(motif, regex("trauma", ignore_case = TRUE)) ~ "Traumatologie",
      TRUE ~ "Autre"
    )
  ) %>%
  select(category, motif, n)

# Comorbidités standardisées
comorbidities_ref <- tribble(
  ~code, ~label, ~weight,
  "C1", "Diabète", 1.2,
  "C2", "Insuffisance cardiaque", 1.5,
  "C3", "Immunodépression", 1.8
)

# Sauvegarde dans le package
usethis::use_data(
  french_levels,
  medical_motifs,
  comorbidities_ref,
  internal = FALSE,
  overwrite = TRUE
)

message("Tables de référence générées dans data/")
