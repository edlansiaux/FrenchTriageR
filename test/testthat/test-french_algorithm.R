context("Tests de l'algorithme FRENCH")

test_that("Urgence vitale - Niveau 1", {
  test_patient <- data.frame(
    MOTIF2 = "Arrêt cardiorespiratoire",
    SpO2 = 85,
    PAS = 60,
    Temperature = 41
  )

  result <- apply_french_triage(test_patient)
  expect_equal(result$french_triage_level, "Urgence vitale - Prise en charge immédiate")
})

test_that("Urgence relative - Niveau 3", {
  test_patient <- data.frame(
    MOTIF2 = "Douleur abdominale aiguë",
    Douleur = "Sévère",
    Comorbidite = "Oui",
    Temperature = 37.5
  )

  result <- apply_french_triage(test_patient)
  expect_equal(levels(result$french_triage_level)[3], "Urgence relative - <60min")
})
