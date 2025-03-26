test_that("Returned dataset has the expected number of rows", {
  test_df <- get_output(test_sim_large(), "rc")
  n_clusters <- nrow(albufera_clusters)
  n_days <- length( seq.Date(from = min(test_df$date),
                             to = max(test_df$date),
                             by = "day")
                    )
  n_tmoas <- length(unique(test_df$tmoa))

  expect_equal(nrow(test_df), n_clusters * n_days * n_tmoas)
})

test_that("PAF columns are probabilities", {
  e <- 1e-10

  test_df <- get_output(test_sim_large(), "rc") |>
    dplyr::filter(
      paf_chronic < -e | paf_acute < -e | paf_chronic > 1+e | paf_acute > 1+e
      )

  expect_equal(nrow(test_df), 0)
})

test_that("HU columns are positive", {
  e <- 1e-10

  test_df <- get_output(test_sim_large(), "rc") |>
    dplyr::filter(
      HU_chronic < -e * mean(abs(HU_chronic)) |
        HU_acute < -e * mean(abs(HU_acute))
    )

  expect_equal(nrow(test_df), 0)
})

test_that("sigmas are consistent across TMoAs", {
  e <- 1e-10

  test_df <- get_output(test_sim_large(), "rc") |>
    dplyr::summarise(
      sd_sigma_chr = sd(sigma_chronic),
      sd_sigma_acu = sd(sigma_acute),
      .by = tmoa
      ) |>
    dplyr::filter(sd_sigma_chr > e | sd_sigma_acu > e)
  expect_equal(nrow(test_df), 0)
})
