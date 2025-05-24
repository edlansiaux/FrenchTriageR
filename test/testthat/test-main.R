context("Tests du workflow principal")

test_that("Exécution complète avec fichier exemple", {
  result <- run_french_triage(test_path("testdata/sample_data.xlsx"))
  expect_s3_class(result, "data.frame")
  expect_true("french_triage_level" %in% names(result))
})

test_that("Export Excel des résultats", {
  test_df <- data.frame(a = 1:3, b = letters[1:3])
  tmp_file <- tempfile(fileext = ".xlsx")
  export_results(test_df, tmp_file)
  expect_true(file.exists(tmp_file))
})
