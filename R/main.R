#' FrenchTriageR - Main Module
#'
#' @description Point d'entrée principal du package pour l'exécution complète du triage FRENCH
#' @export

# Fonctions principales -------------------------------------------------------

#' Exécute le pipeline complet de triage FRENCH
#'
#' @param input_path Chemin vers le fichier Excel d'entrée
#' @param output_path Chemin de sortie pour les résultats (optionnel)
#' @return DataFrame avec résultats du triage
#' @export
#' @importFrom openxlsx write.xlsx
#' @examples
#' \dontrun{
#' resultats <- run_french_triage("donnees_urgences.xlsx", "resultats.xlsx")
#' }

run_french_triage <- function(input_path, output_path = NULL) {
  tryCatch({
    # Chargement et prétraitement
    message("Étape 1/3 : Chargement des données...")
    donnees <- preprocess_data(input_path)

    # Application de l'algorithme
    message("Étape 2/3 : Application de l'algorithme FRENCH...")
    resultats <- apply_french_triage(donnees)

    # Export des résultats
    if (!is.null(output_path)) {
      message("Étape 3/3 : Export des résultats...")
      export_results(resultats, output_path)
    }

    message("Traitement terminé avec succès!")
    return(resultats)

  }, error = function(e) {
    log_error("Pipeline principal", e$message)
    stop("Échec du traitement : ", e$message)
  })
}

#' Exporte les résultats au format Excel
#' @param df DataFrame à exporter
#' @param path Chemin de sortie
#' @export
#' @importFrom openxlsx write.xlsx

export_results <- function(df, path) {
  validate_file_type(path, allowed_ext = "xlsx")

  df %>%
    format_output() %>%
    write.xlsx(file = path,
               overwrite = TRUE,
               asTable = TRUE)
}

# Fonctions de visualisation --------------------------------------------------

#' Affiche la répartition des niveaux de triage
#' @param df DataFrame des résultats
#' @export
#' @import ggplot2

plot_triage_distribution <- function(df) {
  if (!"french_triage_level" %in% names(df)) {
    stop("Colonne 'french_triage_level' manquante")
  }

  ggplot(df, aes(x = french_triage_level)) +
    geom_bar(fill = "steelblue") +
    labs(title = "Répartition des niveaux de triage FRENCH",
         x = "Niveau de triage",
         y = "Nombre de patients") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Génère un rapport synthétique
#' @param df DataFrame des résultats
#' @export

generate_triage_report <- function(df) {
  check_required_columns(df, "french_triage_level")

  rapport <- list(
    n_patients = nrow(df),
    taux_urgences_vitales = mean(df$french_triage_level == "Urgence vitale - Prise en charge immédiate"),
    delai_moyen = mean(df$delai_prise_en_charge, na.rm = TRUE),
    motifs_urgents = df %>%
      filter(french_triage_level %in% 1:3) %>%
      count(motif1, sort = TRUE) %>%
      head(5)
  )

  structure(rapport, class = "triage_report")
}

# Méthodes d'affichage --------------------------------------------------------

#' Affiche le rapport de triage
#' @export

print.triage_report <- function(x) {
  cat("=== Rapport de triage FRENCH ===\n")
  cat("Patients traités :", x$n_patients, "\n")
  cat("Taux d'urgences vitales :", round(x$taux_urgences_vitales * 100, 1), "%\n")
  cat("Délai moyen de prise en charge :", round(x$delai_moyen, 1), "minutes\n")
  cat("\nTop 5 des motifs urgents :\n")
  print(x$motifs_urgents)
}

# Documentation supplémentaire ------------------------------------------------

#' @rdname run_french_triage
#' @examples
#' \dontrun{
#' # Exemple complet
#' df <- run_french_triage("donnees.xlsx")
#' plot_triage_distribution(df)
#' rapport <- generate_triage_report(df)
#' print(rapport)
#' }
