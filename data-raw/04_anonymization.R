# Anonymisation des données sensibles
library(anonymizer)

clean_data %>%
  mutate(
    patient_id = anonymize(patient_id, .algo = "sha256"),
    # Pseudonymisation des dates
    arrival_date = lubridate::floor_date(arrival_date, "month"),
    # Généralisation de l'âge
    age_group = cut(age, breaks = c(0, 18, 65, 120))
  ) %>%
  select(-nom, -prenom, -adresse) %>%
  saveRDS("processed-data/anonymized_data.rds")
