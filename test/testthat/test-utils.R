context("Tests des fonctions utilitaires")

test_that("Conversion durée en heures", {
  test_durations <- c("2 jours", "3 heures", "45 minutes")
  result <- convert_duration_to_hours(test_durations)
  expect_equal(result, c(48, 3, 0.75), tolerance = 0.01)
})

test_that("Validation paramètres vitaux", {
  test_df <- data.frame(PAS = c(120, 300, 80), FC = c(80, 60, 200))
  result <- validate_vitals(test_df)
  expect_equal(nrow(result), 1)
})
