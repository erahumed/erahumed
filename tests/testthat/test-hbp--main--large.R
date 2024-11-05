# Property based tests

test_that("Returned dataset has the expected number of rows", {
  test_df <- component_output(test_mod_large(), "hbp")
  n_clusters <- nrow(albufera_clusters)
  n_days <- length( seq.Date(from = min(test_df$date),
                             to = max(test_df$date),
                             by = "day")
  )

  expect_equal(nrow(test_df), n_clusters * n_days)
})

test_that("'ditch' is consistent along 'cluster_id'", {
  test_df <- component_output(test_mod_large(), "hbp")
  res <- test_df |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(distinct_values = dplyr::n_distinct(ditch)) |>
    dplyr::filter(distinct_values > 1)

  expect_equal(nrow(res), 0)
})

test_that("'tancat' is consistent along 'cluster_id'", {
  test_df <- component_output(test_mod_large(), "hbp")
  res <- test_df |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(distinct_values = dplyr::n_distinct(tancat)) |>
    dplyr::filter(distinct_values > 1)

  expect_equal(nrow(res), 0)
})

test_that("'variety' is consistent along 'cluster_id'", {
  test_df <- component_output(test_mod_large(), "hbp")
  res <- test_df |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(distinct_values = dplyr::n_distinct(variety)) |>
    dplyr::filter(distinct_values > 1)

  expect_equal(nrow(res), 0)
})

test_that("ideal flows are always non-negative", {
  test_df <- component_output(test_mod_large(), "hbp")
  res <- test_df |>
    dplyr::filter(ideal_inflow_cm < 0 | ideal_outflow_cm < 0)
  expect_equal(nrow(res), 0)
})

test_that("real flows are always non-negative", {
  test_df <- component_output(test_mod_large(), "hbp")
  res <- test_df |>
    dplyr::filter(inflow_cm < 0 | outflow_cm < 0)
  expect_equal(nrow(res), 0)
})

test_that("plan_delay is an integer", {
  test_df <- component_output(test_mod_large(), "hbp")
  res <- test_df |>
    dplyr::filter(abs(plan_delay - as.integer(plan_delay)) > 0.1)
  expect_equal(nrow(res), 0)
})

test_that("plan_delay is positive", {
  test_df <- component_output(test_mod_large(), "hbp")
  res <- test_df |>
    dplyr::filter(plan_delay < 0)
  expect_equal(nrow(res), 0)
})

test_that("sum(real outflows) = total capacity of ditch", {
  test_df <- component_output(test_mod_large(), "hbp")
  res <- test_df |>
    dplyr::group_by(date, ditch) |>
    dplyr::summarise(
      flowpoint = capacity_m3_s[1],
      outflow_m3_s = sum(outflow_m3_s),
      .groups = "drop"
    ) |>
    dplyr::filter(
      abs(outflow_m3_s - flowpoint) > mean(abs(flowpoint)) * 1e-10
    )

  expect_equal(nrow(res), 0)
})

test_that("irrigation is the delayed version of ideal_irrigation", {
  test_df <- component_output(test_mod_large(), "hbp")
  res <- test_df |>
    dplyr::group_by(cluster_id) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      # The correct delay for irrigation is that from the previous day!
      delay = dplyr::coalesce(dplyr::lag(plan_delay), 0),
      irrigation_v2 = ideal_irrigation[1:length(delay) - delay]
    ) |>
    dplyr::filter(irrigation != irrigation_v2)

  expect_equal(nrow(res), 0)
})

test_that("draining is the delayed version of ideal_draining", {
  test_df <- component_output(test_mod_large(), "hbp")
  res <- test_df |>
    dplyr::group_by(cluster_id) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      # The correct delay for irrigation is that from the previous day!
      delay = dplyr::coalesce(dplyr::lag(plan_delay), 0),
      draining_v2 = ideal_draining[1:length(delay) - delay]
    ) |>
    dplyr::filter(draining != draining_v2)

  expect_equal(nrow(res), 0)
})

test_that("simple snapshot is constant", {
  skip_on_ci()  # Gives inconsistent result across different platforms

  test_df <- component_output(test_mod_large(), "hbp")
  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})
