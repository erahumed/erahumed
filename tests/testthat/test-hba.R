test_that("compute_hba(): no error with valid arguments", {
  expect_no_error(erahumed_model() |>
                    compute_inp() |>
                    compute_hba())
})

