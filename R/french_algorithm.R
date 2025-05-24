#' FRENCH Triage Algorithm Implementation
#'
#' @description Applique l'algorithme de triage FRENCH selon les spécifications officielles
#' @param df DataFrame prétraité avec les colonnes standardisées
#' @return DataFrame avec colonne supplémentaire 'french_triage_level'
#' @export
#' @importFrom dplyr mutate case_when across everything
#' @importFrom stringr str_detect
#' @importFrom tidyr replace_na

apply_french_triage <- function(df) {
  # Validation initiale
  required_cols <- c("MOTIF2", "PAS", "PAD", "FC", "SpO2", "Temperature",
                     "Duree", "ECG", "Douleur", "Comorbidite", "GCS", "Cyanose",
                     "ShockIndex", "Avis", "Fievre", "Signes", "Tolerance")

  validate_data_structure(df, required_cols)

  # Nettoyage final
  df <- df %>%
    mutate(across(where(is.character), ~tidyr::replace_na(.x, "Inconnu")),
           across(where(is.numeric), ~tidyr::replace_na(.x, median(., na.rm = TRUE))))

  # Application de l'algorithme
  result <- tryCatch({
    df %>%
      mutate(
        french_triage_level = case_when(
          # Niveau 1 - Urgences vitales
          check_level1(.data) ~ 1L,

          # Niveau 2 - Urgences absolues
          check_level2(.data) ~ 2L,

          # Niveau 3 - Urgences relatives
          check_level3(.data) ~ 3L,

          # Niveau 4 - Urgences différées
          check_level4(.data) ~ 4L,

          # Niveau 5 - Non urgent
          check_level5(.data) ~ 5L,

          # Niveau 6 - Cas administratifs
          TRUE ~ 6L
        )
      ) %>%
      mutate(
        french_triage_level = factor(
          french_triage_level,
          levels = 1:6,
          labels = c("Urgence vitale - Prise en charge immédiate",
                     "Urgence absolue - <20min",
                     "Urgence relative - <60min",
                     "Urgence différée - <120min",
                     "Non urgent - <240min",
                     "Cas administratif"),
          ordered = TRUE
        )
      )
  }, error = function(e) {
    message("Erreur pendant le triage : ", e$message)
    return(NULL)
  })

  return(result)
}

# Fonctions de validation -----------------------------------------------------

#' Validation de la structure des données
#' @noRd
validate_data_structure <- function(df, required_cols) {
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop("Structure de données invalide. Colonnes manquantes : ",
         paste(missing, collapse = ", "))
  }
}

# Fonctions de vérification des niveaux ---------------------------------------

#' @noRd
check_level1 <- function(patient) {
  # Urgences vitales
  str_detect(patient$MOTIF2, "Arrêt cardiorespiratoire|Traumatisme crânien avec GCS ≤8|Détresse respiratoire aiguë") |
    (patient$SpO2 < 86 & !is.na(patient$SpO2)) |
    (patient$PAS <= 70 & str_detect(patient$MOTIF2, "Hypotension")) |
    (patient$Temperature <= 32 | patient$Temperature >= 41)
}

#' @noRd
check_level2 <- function(patient) {
  # Urgences absolues
  cardiac_conditions <- str_detect(patient$MOTIF2, "Douleur thoracique|Tachycardie|Bradycardie") &
    (patient$ECG == "Anormal" | patient$Douleur == "Intense")

  neuro_conditions <- str_detect(patient$MOTIF2, "AVC|Déficit neurologique") &
    patient$Duree <= 4.5

  trauma_conditions <- str_detect(patient$MOTIF2, "Traumatisme grave") &
    (patient$Penetrant == "Oui" | patient$Velocite == "Oui")

  cardiac_conditions | neuro_conditions | trauma_conditions
}

#' @noRd
check_level3 <- function(patient) {
  # Urgences relatives
  abdominal_conditions <- str_detect(patient$MOTIF2, "Douleur abdominale|Vomissements") &
    (patient$Douleur == "Sévère" | patient$Comorbidite == "Oui")

  infection_conditions <- str_detect(patient$MOTIF2, "Fièvre|Sepsis") &
    (patient$Fievre == "Oui" | patient$ShockIndex >= 1)

  psych_conditions <- str_detect(patient$MOTIF2, "Suicidaire|Psychiatrie aiguë") &
    patient$Agitation == "Oui"

  abdominal_conditions | infection_conditions | psych_conditions
}

#' @noRd
check_level4 <- function(patient) {
  # Urgences différées
  minor_trauma <- str_detect(patient$MOTIF2, "Plaie simple|Entorse") &
    patient$Tolerance == "Bonne"

  skin_conditions <- str_detect(patient$MOTIF2, "Abcès|Infection cutanée") &
    !is.na(patient$Signes)

  gastro_conditions <- str_detect(patient$MOTIF2, "Diarrhée|Constipation") &
    patient$Duree < 48

  minor_trauma | skin_conditions | gastro_conditions
}

#' @noRd
check_level5 <- function(patient) {
  # Non urgent
  admin_conditions <- str_detect(patient$MOTIF2, "Certificat|Renouvellement|Contrôle")
  minor_symptoms <- str_detect(patient$MOTIF2, "Toux|Rhume") & patient$Fievre == "Non"

  admin_conditions | minor_symptoms
}

# Fonctions helper supplémentaires --------------------------------------------

#' Vérification des comorbidités
#' @noRd
check_comorbidities <- function(patient) {
  ifelse(patient$Comorbidite == "Oui", 1, 0)
}

#' Calcul du score de gravité
#' @noRd
calculate_severity_score <- function(patient) {
  score <- 0
  score <- score + ifelse(patient$Douleur == "Intense", 2, 0)
  score <- score + ifelse(patient$Fievre == "Oui", 1, 0)
  score <- score + check_comorbidities(patient)
  score <- score + ifelse(patient$GCS < 15, 1, 0)
  return(score)
}

# Documentation supplémentaire ------------------------------------------------

#' @rdname apply_french_triage
#' @examples
#' \dontrun{
#' # Exemple d'utilisation
#' data <- read_excel("donnees_patients.xlsx")
#' data_prepro <- preprocess_data(data)
#' resultats <- apply_french_triage(data_prepro)
#' table(resultats$french_triage_level)
#' }
