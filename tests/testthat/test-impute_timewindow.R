test_that("impute_timewindow returns expected columns", {
  example_data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    dy = c(0, 15, 45, 0, 20),
    hba1c = c(7.2, NA, 7.4, NA, 6.9)
  )

  result <- healthimpute::impute_timewindow(
    data = example_data,
    variable = "hba1c",
    patient_id_col = "id",
    dy_col = "dy",
    is_date = FALSE
  )

  expect_true("imputed_value_hba1c" %in% names(result))
  expect_true("hba1c_carried" %in% names(result))
})
