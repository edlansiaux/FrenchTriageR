\name{preprocess_data}
\alias{preprocess_data}
\title{Prétraitement des Données Médicales}
\description{
Nettoie et transforme les données brutes pour l'algorithme FRENCH.
}
\usage{
preprocess_data(path)
}
\arguments{
  \item{path}{Chemin vers le fichier Excel brut}
}
\value{
DataFrame prétraité avec colonnes standardisées
}
\details{
Effectue :
\itemize{
  \item Nettoyage des valeurs manquantes
  \item Conversion des unités médicales
  \item Extraction automatique des motifs
}
}
\examples{
\dontrun{
data_clean <- preprocess_data("urgences.xlsx")
}
}
