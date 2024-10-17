test_that("Returned dataset has the expected number of rows", {
  test_df <- component_output(test_mod_large(), "ct")
  n_clusters <- nrow(albufera_clusters)
  n_days <- length( seq.Date(from = min(test_df$date),
                             to = max(test_df$date),
                             by = "day")
  )

  expect_equal(nrow(test_df), n_clusters * n_days)
})

test_that("The time series of chemical masses are always positive", {
  lower_thresh <- -1e-9  # -1 microgram looks fair enough
  test_df <- component_output(test_mod_large(), "ct")

  only_positive_values <- test_df |>
    dplyr::select(-c(cluster_id, date)) |>  # Select relevant columns
    (\(df) df < lower_thresh)() |>          # Applied threshold column-wise
    rowSums() |>  # Vec of number of cols that satisfy condition for each row
    (\(x) all(x == 0))()

  expect_true(only_positive_values)
})

test_that("Chemical masses do not increase except if directly applied", {
  chemicals <- unique(albufera_ca_schedules$chemical)
  test_df <- component_output(test_mod_large(), "ct")
  applications_df <- component_output(test_mod_large(), "ca")
  tol_kg <- 1e-10

  for (chemical in chemicals) {
    m_applied <- applications_df[[chemical]]
    if (is.null(m_applied))
      next
    actual_masses <- test_df |>
      dplyr::select(dplyr::contains(chemical)) |>
      rowSums()

    no_mass_increase <- diff(actual_masses) <= m_applied[-1] + tol_kg
    expect_true(all(no_mass_increase))
  }
})

test_that("All compartments of all clusters have at least one >0 value", {
  # We use Acetamiprid for this test, because it is applied to all clusters

  tol_kg <- 1e-10

  condition <- component_output(test_mod_large(), "ct") |>
    dplyr::select(cluster_id, dplyr::starts_with("Acetami")) |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), sum)) |>
    dplyr::select(-cluster_id) |>
    (\(df) df < tol_kg)() |>          # Apply threshold column-wise
    rowSums() |>    # Vec of number of cols that are below tol_kg for each row
    (\(x) all(x == 0))()  # ...For each row, this number should be zero!

  expect_true(condition)
})

test_that("simple snapshot is constant", {
  skip_on_ci()  # Gives inconsistent result across different platforms

  test_df <- component_output(test_mod_large(), "ct")
  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})
