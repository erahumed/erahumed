test_that("compute_hba(): no error with valid arguments", {
  expect_no_error(erahumed_model() |>
                    compute_inp() |>
                    compute_hba())
})

test_that("compute_hba(): returns an object of the correct class", {
  obj <- erahumed_model() |>
    compute_inp() |>
    compute_hba()
  expect_s3_class(obj, class(erahumed_model()))
})
