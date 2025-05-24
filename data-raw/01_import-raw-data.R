# Import des données brutes depuis différents formats
library(readxl)
library(readr)

# Données Hospitalière A
raw_hospA <- read_excel("raw-data/hospital-A/2023-triage-data.xlsx")

# Données Hospitalière B
raw_hospB <- read_csv("raw-data/hospital-B/urgences-2023.csv")

saveRDS(raw_hospA, "raw-data/hospital-A/raw_hospA.rds")
saveRDS(raw_hospB, "raw-data/hospital-B/raw_hospB.rds")
