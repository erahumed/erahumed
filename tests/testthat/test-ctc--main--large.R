test_that("Returned dataset has the expected number of rows", {
  test_df <- get_output(test_sim_large(), "ctc")
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
  test_df <- get_output(test_sim_large(), "ctc") |>
    dplyr::filter(mf_kg < lower_thresh | mw_kg < lower_thresh | ms_kg < lower_thresh)

  expect_equal(nrow(test_df), 0)
})

test_that("Chemical masses do not increase except if directly applied", {
  test_df <- get_output(test_sim_large(), "ctc")

  chemicals <- unique(test_df$chemical)
  applications_df <- get_output(test_sim_large(), "ca")
  tol_kg <- 1e-10

  for (chemical in chemicals) {
    m_applied <- applications_df[[chemical]]
    if (is.null(m_applied))
      next
    actual_masses <- test_df |>
      dplyr::filter(chemical == !!chemical) |>
      dplyr::select(mf_kg, mw_kg, ms_kg) |>
      rowSums()

    no_mass_increase <- diff(actual_masses) <= m_applied[-1] + tol_kg
    expect_true(all(no_mass_increase))
  }
})

test_that("All compartments of all clusters have at least one >0 value", {
  tol_kg <- 1e-10

  universal_chems <- c("Acetamiprid", "Azoxystrobin", "Difenoconazole")

  test_df <- get_output(test_sim_large(), "ctc") |>
    dplyr::filter(chemical %in% universal_chems) |>
    dplyr::group_by(element_id, chemical) |>
    dplyr::summarise(mf_kg = sum(mf_kg), mw_kg = sum(mw_kg), ms_kg = sum(ms_kg)) |>
    dplyr::filter(mf_kg < tol_kg | mw_kg < tol_kg | ms_kg < tol_kg)

  expect_equal(nrow(test_df), 0)
})

test_that("Calculated volume_sod_m3 coincide with lagged volume_eod_m3", {
  tol_m3 <- 1e-6

  test_df <- get_output(test_sim_large(), "ctc") |>
    dplyr::group_by(element_id, chemical) |>
    dplyr::mutate(
      n = dplyr::n(),
      diff = c(0, abs(volume_eod_m3[-n] - volume_sod_m3[-1]))
      ) |>
    dplyr::filter(diff > tol_m3)

  expect_equal(nrow(test_df), 0)
})

test_that("simple snapshot is constant", {
  skip_on_ci()  # Gives inconsistent result across different platforms

  test_df <- get_output(test_sim_large(), "ctc")
  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})
