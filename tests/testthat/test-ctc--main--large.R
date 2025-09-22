test_that("Returned dataset has the expected number of rows", {
  test_df <- get_raw_output(test_sim_large(), "ctc")
  n_clusters <- nrow(albufera_clusters)
  n_days <- length( seq.Date(from = min(test_df$date),
                             to = max(test_df$date),
                             by = "day")
  )
  n_chemicals <- length(unique(test_df$chemical_id))

  expect_equal(nrow(test_df), n_clusters * n_days * n_chemicals)
})

test_that("The time series of chemical masses are always positive", {
  lower_thresh <- -1e-9  # -1 microgram looks fair enough
  test_df <- get_raw_output(test_sim_large(), "ctc") |>
    dplyr::filter(mf_kg < lower_thresh | mw_kg < lower_thresh | ms_kg < lower_thresh)

  expect_equal(nrow(test_df), 0)
})

test_that("Chemical masses do not increase except if directly applied", {
  skip("TODO: Reimplement this test")
  test_df <- get_raw_output(test_sim_large(), "ctc")

  chemicals <- unique(test_df$chemical_id)
  applications_df <- get_etc(test_sim_large(), "applications_df")
  tol_kg <- 1e-10

  for (chemical_id in chemicals) {
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

  chemical_db <- get_etc(test_sim_large(), "chemical_db")
  chem_names <- sapply(seq_along(chemical_db), function(chemical_id) {
    ct_get_param(chemical_id, "display_name", chemical_db)
  })

  universal_chem_ids <- which(chem_names %in% universal_chems)

  test_df <- get_raw_output(test_sim_large(), "ctc") |>
    dplyr::filter(chemical_id %in% universal_chem_ids) |>
    dplyr::group_by(element_id, chemical_id) |>
    dplyr::summarise(mf_kg = sum(mf_kg), mw_kg = sum(mw_kg), ms_kg = sum(ms_kg), .groups = "drop") |>
    dplyr::filter(mf_kg < tol_kg | mw_kg < tol_kg | ms_kg < tol_kg)

  expect_equal(nrow(test_df), 0)
})


test_that("simple snapshot is constant", {
  skip_on_ci()  # Gives inconsistent result across different platforms

  test_df <- get_raw_output(test_sim_large(), "ctc")
  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})
