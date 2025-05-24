context("Tests pour le prétraitement des données")

test_that("Nettoyage des colonnes French", {
  test_df <- data.frame(
    `FRENCH inf` = c("3A", "4", "5B", "N/A"),
    check.names = FALSE
  )

  result <- clean_french_column(test_df)
  expect_equal(result$french_inf, c(3, 4, 5, NA))
})

test_that("Gestion des valeurs manquantes", {
  test_df <- data.frame(a = c(1, NA), b = c("text", NA))
  result <- handle_missing_values(test_df)
  expect_equal(sum(is.na(result)), 0)
})

test_that("Détection des comorbidités", {
  test_text <- c("Pas d'antécédent", "Diabète type 2", "Antécédent AVC")
  result <- detect_comorbidites(test_text)
  expect_equal(result, c(0, 1, 1))
})
