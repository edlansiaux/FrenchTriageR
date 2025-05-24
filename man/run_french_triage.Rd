\name{run_french_triage}
\alias{run_french_triage}
\title{Pipeline Complet de Triage}
\description{
Exécute toute la chaîne de traitement des données.
}
\usage{
run_french_triage(input_path, output_path = NULL)
}
\arguments{
  \item{input_path}{Chemin d'entrée des données brutes}
  \item{output_path}{Chemin de sortie optionnel pour Excel}
}
\value{
DataFrame avec résultats complets
}
\seealso{
\code{\link{export_results}} pour l'export des résultats
}
