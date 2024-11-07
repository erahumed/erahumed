test_that("Returned dataset has the expected number of rows", {
  test_df <- get_layer_output(test_sim_large(), "ct")
  n_clusters <- nrow(albufera_clusters)
  n_days <- length( seq.Date(from = min(test_df$date),
                             to = max(test_df$date),
                             by = "day")
                    )
  n_chemicals <- length(unique(test_df$chemical))

  expect_equal(nrow(test_df), n_clusters * n_days * n_chemicals)
})

test_that("The time series of chemical masses are always positive", {
  lower_thresh <- -1e-9  # -1 microgram looks fair enough
  test_df <- get_layer_output(test_sim_large(), "ct") |>
    dplyr::filter(mf < lower_thresh | mw < lower_thresh | ms < lower_thresh)

  expect_equal(nrow(test_df), 0)
})

test_that("Chemical masses do not increase except if directly applied", {
  test_df <- get_layer_output(test_sim_large(), "ct")

  chemicals <- unique(test_df$chemical)
  applications_df <- get_layer_output(test_sim_large(), "ca")
  tol_kg <- 1e-10

  for (chemical in chemicals) {
    m_applied <- applications_df[[chemical]]
    if (is.null(m_applied))
      next
    actual_masses <- test_df |>
      dplyr::filter(chemical == !!chemical) |>
      dplyr::select(mf, mw, ms) |>
      rowSums()

    no_mass_increase <- diff(actual_masses) <= m_applied[-1] + tol_kg
    expect_true(all(no_mass_increase))
  }
})

test_that("All compartments of all clusters have at least one >0 value", {
  tol_kg <- 1e-10

  non_universal_chems <- c("MCPA", "Benta", "Cyhalo", "Cicloxidim", "Penoxulam")

  test_df <- get_layer_output(test_sim_large(), "ct") |>
    dplyr::filter(!(chemical %in% non_universal_chems)) |>
    dplyr::group_by(cluster_id, chemical) |>
    dplyr::summarise(mf = sum(mf), mw = sum(mw), ms = sum(ms)) |>
    dplyr::filter(mf < tol_kg | mw < tol_kg | ms < tol_kg)

  expect_equal(nrow(test_df), 0)
})

test_that("simple snapshot is constant", {
  skip_on_ci()  # Gives inconsistent result across different platforms

  test_df <- get_layer_output(test_sim_large(), "ct")
  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})
