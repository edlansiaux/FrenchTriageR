\name{apply_french_triage}
\alias{apply_french_triage}
\title{Algorithme de Triage FRENCH}
\description{
Applique l'algorithme de triage médical FRENCH à un dataframe de patients.
}
\usage{
apply_french_triage(df)
}
\arguments{
  \item{df}{DataFrame contenant les données patients normalisées}
}
\value{
DataFrame avec colonne supplémentaire 'triage_level' (facteur ordonné)
}
\examples{
\dontrun{
data <- preprocess_data("donnees.xlsx")
resultats <- apply_french_triage(data)
}
}
\seealso{
\code{\link{preprocess_data}} pour la préparation des données
}
