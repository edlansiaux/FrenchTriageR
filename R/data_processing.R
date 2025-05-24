#' Data Preprocessing for French Triage System
#'
#' @description Nettoie et prépare les données brutes pour l'algorithme de triage FRENCH
#' @param path Chemin vers le fichier Excel brut
#' @return DataFrame prétraité
#' @export
#' @importFrom readxl read_excel
#' @importFrom dplyr select mutate across everything filter rename anti_join
#' @importFrom stringr str_detect str_replace str_split
#' @importFrom tidyr replace_na
#' @importFrom stringi stri_encode stri_split_regex

preprocess_data <- function(path) {
  # Lecture des données brutes
  raw_data <- read_raw_data(path)

  # Pipeline de prétraitement
  processed_data <- raw_data %>%
    clean_column_names() %>%
    remove_empty_columns() %>%
    handle_missing_values() %>%
    clean_french_column() %>%
    split_blood_pressure() %>%
    process_oxygen_values() %>%
    extract_motifs() %>%
    calculate_clinical_scores() %>%
    detect_medical_conditions() %>%
    final_processing()

  return(processed_data)
}

# Fonctions internes ----------------------------------------------------------

#' Lecture des données brutes
#' @noRd
read_raw_data <- function(path) {
  validate_file_extension(path)

  data <- readxl::read_excel(path) %>%
    rename_with(~tolower(gsub("[ ]", "_", .x)))

  return(data)
}

#' Validation de l'extension du fichier
#' @noRd
validate_file_extension <- function(path) {
  if (!grepl("\\.xlsx?$", path)) {
    stop("Le fichier doit être au format Excel (.xlsx ou .xls)")
  }
}

#' Nettoyage des noms de colonnes
#' @noRd
clean_column_names <- function(df) {
  df %>%
    rename(
      "french_inf" = matches("french.*inf"),
      "pression_arterielle" = matches("pas/pad"),
      "motif" = matches("motif")
    ) %>%
    select(-matches("^x[0-9]+|^...$"))  # Suppression colonnes vides
}

#' Gestion des valeurs manquantes
#' @noRd
handle_missing_values <- function(df) {
  df %>%
    mutate(across(
      where(is.character),
      ~tidyr::replace_na(.x, "non_renseigne")
    ) %>%
      mutate(across(
        where(is.numeric),
        ~tidyr::replace_na(.x, median(., na.rm = TRUE))
      ) %>%
        filter(!is.na(french_inf))
}

#' Nettoyage de la colonne French
#' @noRd
clean_french_column <- function(df) {
  df %>%
    mutate(
      french_inf = case_when(
        str_detect(french_inf, "3A") ~ "3",
        str_detect(french_inf, "3B") ~ "4",
        str_detect(french_inf, "[1-5]") ~ french_inf,
        TRUE ~ NA_character_
      ) %>%
        as.numeric()
    )
}

#' Séparation pression artérielle
#' @noRd
split_blood_pressure <- function(df) {
  df %>%
    mutate(
      pas = as.numeric(str_split(pression_arterielle, "/", simplify = TRUE)[,1]),
      pad = as.numeric(str_split(pression_arterielle, "/", simplify = TRUE)[,2])
    ) %>%
    select(-pression_arterielle)
}

#' Traitement des valeurs O2
#' @noRd
process_oxygen_values <- function(df) {
  df %>%
    mutate(
      o2 = case_when(
        o2 %in% c("AA", "aa", "N/A") ~ 0,
        str_detect(o2, "L|l") ~ as.numeric(str_replace(o2, "[A-Za-z]", "")),
        TRUE ~ as.numeric(o2)
      )
    )
}

#' Extraction des motifs médicaux
#' @noRd
extract_motifs <- function(df) {
  df %>%
    mutate(
      motif_processed = stri_encode(motif, to = "UTF-8"),
      motif1 = str_extract(motif_processed, "^[A-ZÉÈÀÊÔÛÎÇ/ -]+(?=:)"),
      motif2 = str_replace(motif_processed, "^[A-ZÉÈÀÊÔÛÎÇ/ -]+:\\s*", "")
    ) %>%
    select(-motif, -motif_processed)
}

#' Calcul des scores cliniques
#' @noRd
calculate_clinical_scores <- function(df) {
  df %>%
    mutate(
      shock_index = fc / pas,
      glycemie = case_when(
        dextro >= 3.9 & dextro <= 5.6 ~ "normale",
        dextro > 5.6 ~ "hyperglycemie",
        dextro < 3.9 ~ "hypoglycemie"
      ),
      cetose = case_when(
        acetonemie > 2 ~ "elevee",
        acetonemie >= 1 ~ "positive",
        TRUE ~ "negative"
      )
    )
}

#' Détection des conditions médicales
#' @noRd
detect_medical_conditions <- function(df) {
  df %>%
    mutate(
      comorbidite_cv = detect_comorbidites(antecedents),
      perte_connaissance = detect_pci(entretien),
      trouble_conscience = detect_confusion(entretien)
    )
}

#' Détection générique de comorbidités
#' @noRd
detect_comorbidites <- function(text) {
  termes <- c("diabete", "hta", "avc", "infarctus", "insuffisance cardiaque")
  as.integer(str_detect(tolower(text), paste(termes, collapse = "|")))
}

#' Détection perte de connaissance
#' @noRd
detect_pci <- function(text) {
  termes <- c("pci", "perte connaissance", "malaise", "syncope")
  as.integer(str_detect(tolower(text), paste(termes, collapse = "|")))
}

#' Nettoyage final
#' @noRd
final_processing <- function(df) {
  df %>%
    select(
      -matches("^x[0-9]"),
      -matches("^dextro"),
      -matches("^acetonemie"),
      -matches("^entretien")
    ) %>%
    mutate(across(where(is.character), as.factor))
}

# Documentation supplémentaire ------------------------------------------------

#' @rdname preprocess_data
#' @examples
#' \dontrun{
#' # Exemple d'utilisation
#' data <- preprocess_data("donnees_urgences.xlsx")
#' summary(data)
#' }
