#' Utility Functions for French Triage System
#'
#' @description Fonctions internes de support pour le package FrenchTriageR
#' @keywords internal
#' @noRd

# Gestion des erreurs ---------------------------------------------------------

#' Affiche un message d'erreur standardisé
#' @param context Contexte de l'erreur
#' @param message Message détaillé
#' @noRd
log_error <- function(context, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("[ERREUR] ", timestamp, " | ", context, " : ", message, "\n"))
}

#' Gère les erreurs de format de fichier
#' @noRd
validate_file_type <- function(path, allowed_ext = c("xlsx", "xls")) {
  ext <- tools::file_ext(path)
  if (!tolower(ext) %in% allowed_ext) {
    log_error("Validation fichier", paste("Format", ext, "non supporté"))
    stop("Seuls les fichiers Excel sont acceptés")
  }
}

#' Vérifie la présence de colonnes obligatoires
#' @noRd
check_required_columns <- function(df, required_cols) {
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    log_error("Validation colonnes", paste("Colonnes manquantes :", paste(missing, collapse = ", ")))
    return(FALSE)
  }
  return(TRUE)
}

# Transformation des données --------------------------------------------------

#' Normalise les chaînes de caractères médicales
#' @noRd
normalize_medical_terms <- function(text) {
  text %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[éèê]", "e") %>%
    stringr::str_replace_all("[àâ]", "a") %>%
    stringr::str_replace_all("[îï]", "i") %>%
    stringr::str_replace_all("[ôö]", "o") %>%
    stringr::str_replace_all("[ûùü]", "u") %>%
    stringr::str_remove_all("[^a-z0-9 ]")
}

#' Convertit les durées en heures
#' @noRd
convert_duration_to_hours <- function(duration) {
  case_when(
    str_detect(duration, "jour|j") ~ as.numeric(str_extract(duration, "\\d+")) * 24,
    str_detect(duration, "heure|h") ~ as.numeric(str_extract(duration, "\\d+")),
    str_detect(duration, "minute|min") ~ as.numeric(str_extract(duration, "\\d+")) / 60,
    TRUE ~ NA_real_
  )
}

# Calculs médicaux ------------------------------------------------------------

#' Calcule le score NEWS2 simplifié
#' @noRd
calculate_news2_score <- function(df) {
  df %>%
    mutate(
      news2 = case_when(
        SpO2 < 92 | PAS < 90 ~ 3,
        FC > 130 | FC < 50 ~ 3,
        Temperature > 38 | Temperature < 35 ~ 2,
        TRUE ~ 0
      )
    )
}

#' Détermine le niveau d'urgence basique
#' @noRd
basic_triage_level <- function(df) {
  df %>%
    mutate(
      triage_urgence = case_when(
        SpO2 < 90 | PAS < 90 ~ 1,
        FC > 120 | FC < 40 ~ 2,
        Temperature > 39 ~ 2,
        Douleur > 7 ~ 3,
        TRUE ~ 4
      )
    )
}

# Formatage des résultats -----------------------------------------------------

#' Formate les sorties pour l'export
#' @noRd
format_output <- function(df) {
  df %>%
    mutate(
      across(where(is.character), ~stringr::str_trunc(.x, 30)),
      across(where(is.numeric), ~round(.x, 2)),
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
}

#' Crée un résumé statistique
#' @noRd
generate_summary <- function(df) {
  list(
    n_patients = nrow(df),
    mean_age = mean(df$age, na.rm = TRUE),
    triage_distribution = table(df$french_triage_level),
    common_motifs = names(sort(table(df$motif1), decreasing = TRUE)[1:5])
  )
}

# Validation des entrées ------------------------------------------------------

#' Vérifie la validité des paramètres vitaux
#' @noRd
validate_vitals <- function(df) {
  df %>%
    mutate(
      valid_PAS = between(PAS, 50, 250),
      valid_FC = between(FC, 20, 250),
      valid_SpO2 = between(SpO2, 70, 100)
    ) %>%
    filter(valid_PAS & valid_FC & valid_SpO2) %>%
    select(-starts_with("valid_"))
}

#' Contrôle de qualité des données
#' @noRd
run_data_quality_check <- function(df) {
  checks <- list(
    missing_ids = sum(is.na(df$id)),
    invalid_dates = sum(is.na(as.Date(df$date_entree))),
    abnormal_values = nrow(df) - nrow(validate_vitals(df))
  )

  if (any(checks > 0)) {
    log_error("Contrôle qualité", paste(
      "Problèmes détectés :",
      paste(names(checks), checks, collapse = " | ")
    ))
  }

  return(checks)
}

# Documentation supplémentaire ------------------------------------------------

#' @rdname utils
#' @examples
#' \dontrun{
#' # Exemple d'utilisation interne
#' df <- data.frame(PAS = c(120, 80, 300), FC = c(80, 200, 60))
#' validate_vitals(df)
#' }
