withr::with_envvar(c(erahumed_randomize_clusters = FALSE), {
  date_min <- as.Date("2010-01-01")
  date_max <- as.Date("2011-12-31")

  outflows_df <- albufera_outflows
  weather_df <- albufera_weather
  clusters_df <- albufera_clusters
  management_df <- albufera_management

  test_df <- albufera_hydro_balance_local(outflows_df = outflows_df,
                                          weather_df = weather_df,
                                          clusters_df = clusters_df,
                                          management_df = management_df,
                                          date_min = date_min,
                                          date_max = date_max)

  eps <- 1e-10
  })


# TODO: remove this in favor of more dedicated unit tests
test_that("simple snapshot is constant", {
  skip_on_ci()  # Gives inconsistent result across different platforms

  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})



test_that("Returned dataset has the expected number of rows", {
  n_clusters <- nrow(clusters_df)
  n_days <- length( seq.Date(from = date_min, to = date_max, by = "day") )

  expect_equal(nrow(test_df), n_clusters * n_days)
})

test_that("ideal flows are always non-negative", {
  res <- test_df |>
    dplyr::filter(ideal_inflow_cm < 0 | ideal_outflow_cm < 0)
  expect_equal(nrow(res), 0)
})

test_that("real flows are always non-negative", {
  res <- test_df |>
    dplyr::filter(real_inflow_cm < 0 | real_outflow_cm < 0)
  expect_equal(nrow(res), 0)
})

test_that("sum(real outflows) = total capacity of ditch", {
  res <- test_df |>
    dplyr::group_by(date, ditch) |>
    dplyr::summarise(
      flowpoint = capacity_m3_s[1],
      real_outflow_m3_s = sum(real_outflow_m3_s),
      .groups = "drop"
    ) |>
    dplyr::filter(
      abs(real_outflow_m3_s - flowpoint) > mean(abs(flowpoint)) * eps
      )

  expect_equal(nrow(res), 0)
})
