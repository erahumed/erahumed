test_that("Returned dataset has the expected number of rows", {
  test_df <- get_layer_output(test_sim_large(), "ctl")

  n_days <- length( seq.Date(from = min(test_df$date),
                             to = max(test_df$date),
                             by = "day")
  )
  n_chemicals <- length(unique(test_df$chemical))

  expect_equal(nrow(test_df), 1 * n_days * n_chemicals)
})

test_that("The time series of chemical masses are always positive", {
  lower_thresh <- -1e-9  # -1 microgram looks fair enough
  test_df <- get_layer_output(test_sim_large(), "ctl") |>
    dplyr::filter(mf < lower_thresh | mw < lower_thresh | ms < lower_thresh)

  expect_equal(nrow(test_df), 0)
})


test_that("Water and soil of all ditches have at least one >0 value", {
  tol_kg <- 1e-10

  non_universal_chems <- c("MCPA", "Benta", "Cyhalo", "Cicloxidim", "Penoxulam")

  test_df <- get_layer_output(test_sim_large(), "ctl") |>
    dplyr::filter(!(chemical %in% non_universal_chems)) |>
    dplyr::group_by(chemical) |>
    dplyr::summarise(mw = sum(mw), ms = sum(ms)) |>
    dplyr::filter(mw < tol_kg | ms < tol_kg)

  expect_equal(nrow(test_df), 0)
})

test_that("Foliage mass of all ditches is always =0", {
  tol_kg <- 1e-10

  non_universal_chems <- c("MCPA", "Benta", "Cyhalo", "Cicloxidim", "Penoxulam")

  test_df <- get_layer_output(test_sim_large(), "ctl") |>
    dplyr::filter(!(chemical %in% non_universal_chems)) |>
    dplyr::group_by(chemical) |>
    dplyr::summarise(mf = sum(mf)) |>
    dplyr::filter(mf > tol_kg)

  expect_equal(nrow(test_df), 0)
})


test_that("simple snapshot is constant", {
  skip_on_ci()  # Gives inconsistent result across different platforms

  test_df <- get_layer_output(test_sim_large(), "ctl")
  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})
