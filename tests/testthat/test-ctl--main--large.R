test_that("Returned dataset has the expected number of rows", {
  test_df <- get_output(test_sim_large(), "ctl")

  n_days <- length( seq.Date(from = min(test_df$date),
                             to = max(test_df$date),
                             by = "day")
  )
  n_chemicals <- length(unique(test_df$chemical))

  expect_equal(nrow(test_df), 1 * n_days * n_chemicals)
})

test_that("The time series of chemical masses are always positive", {
  lower_thresh <- -1e-9  # -1 microgram looks fair enough
  test_df <- get_output(test_sim_large(), "ctl") |>
    dplyr::filter(mf_kg < lower_thresh | mw_kg < lower_thresh | ms_kg < lower_thresh)

  expect_equal(nrow(test_df), 0)
})


test_that("Water and soil of all ditches have at least one >0 value", {
  tol_kg <- 1e-10

  universal_chems <- c("Acetamiprid", "Azoxystrobin", "Difenoconazole")

  chemical_db <- get_etc(test_sim_large(), "chemical_db")
  chem_names <- sapply(seq_along(chemical_db), function(chemical_id) {
    ct_get_param(chemical_id, "display_name", chemical_db)
  })

  universal_chem_ids <- which(chem_names %in% universal_chems)

  test_df <- get_output(test_sim_large(), "ctl") |>
    dplyr::filter(chemical_id %in% universal_chem_ids) |>
    dplyr::group_by(element_id, chemical_id) |>
    dplyr::summarise(mw_kg = sum(mw_kg), ms_kg = sum(ms_kg), .groups = "drop") |>
    dplyr::filter(mw_kg < tol_kg | ms_kg < tol_kg)

  expect_equal(nrow(test_df), 0)
})

test_that("Foliage mass of all ditches is always =0", {
  tol_kg <- 1e-10


  test_df <- get_output(test_sim_large(), "ctl") |>
    dplyr::summarise(mf_kg = sum(mf_kg)) |>
    dplyr::filter(mf_kg > tol_kg)

  expect_equal(nrow(test_df), 0)
})


test_that("simple snapshot is constant", {
  skip("TODO")
  skip_on_ci()  # Gives inconsistent result across different platforms

  test_df <- get_output(test_sim_large(), "ctl")
  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})
