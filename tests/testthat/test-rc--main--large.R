test_that("PAF columns are probabilities", {
  e <- 1e-10

  test_df <- get_output(test_sim_large(), "rc") |>
    dplyr::filter(
      paf_chronic < -e | paf_acute < -e | paf_chronic > 1+e | paf_acute > 1+e
      )

  expect_equal(nrow(test_df), 0)
})

