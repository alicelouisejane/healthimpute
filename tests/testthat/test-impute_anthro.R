test_that("impute_anthro returns expected columns for weight", {
  example_data <- data.frame(
    patient_id = c(1, 1, 2, 2),
    age_at_visit = c(5, 6, 5.5, 6.5),  # age in years
    sex = c("Male", "Female", "Male", "Female"),
    weightkg = c(20, NA, 19, NA)
  )

  result <- healthimpute::impute_anthro(
    data = example_data,
    variable = "weightkg",
    patient_id_col = "patient_id",
    age_col = "age_at_visit",
    sex_col = "sex"
  )

  expect_true("imputed_value_weightkg" %in% names(result))
  expect_true("weightkg_carried" %in% names(result))
})
