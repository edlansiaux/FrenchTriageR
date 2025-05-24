\name{export_results}
\alias{export_results}
\title{Export des Résultats}
\description{
Exporte les résultats du triage au format Excel.
}
\usage{
export_results(df, path)
}
\arguments{
  \item{df}{DataFrame des résultats}
  \item{path}{Chemin de sortie .xlsx}
}
\details{
Formate automatiquement :
\itemize{
  \item Troncature des textes longs
  \item Arrondi des valeurs numériques
}
}
