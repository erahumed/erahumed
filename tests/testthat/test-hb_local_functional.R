withr::with_envvar(c(erahumed_hb_sort_clusters = TRUE), {
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



test_that("simple snapshot is constant", {
  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})



test_that("Returned dataset has the expected number of rows", {
  n_clusters <- nrow(clusters_df)
  n_days <- length( seq.Date(from = date_min, to = date_max, by = "day") )

  expect_equal(nrow(test_df), n_clusters * n_days)
})




test_that("If draining, the ideal height difference is non-positive", {
  res <- test_df |>
    dplyr::filter(draining, height_diff_cm > 1 * eps)

  expect_equal(nrow(res), 0)
})

test_that("If irrigating, the ideal height difference is non-negative", {
  res <- test_df |>
    dplyr::filter(irrigation, height_diff_cm < -1 * eps)

  expect_equal(nrow(res), 0)
})

test_that("If draining and irrigating, the ideal height difference is zero", {
  res <- test_df |>
    dplyr::filter(draining, irrigation, abs(height_diff_cm) > 1 * eps)

  expect_equal(nrow(res), 0)
})



test_that("If draining, ideal inflow > ideal height difference", {
  res <- test_df |>
    dplyr::filter(draining,
                  height_diff_cm - ideal_inflow_cm > mean(abs(ideal_inflow_cm)) * eps
                  )

  expect_equal(nrow(res), 0)
})

test_that("ideal inflow is zero if not irrigating", {
  res <- test_df |>
    dplyr::filter(!irrigation,
                  abs(ideal_inflow_cm) / mean(abs(ideal_inflow_cm)) > eps
    )

  expect_equal(nrow(res), 0)
})

test_that("real inflow is zero if not irrigating", {
  res <- test_df |>
    dplyr::filter(!irrigation,
                  abs(real_inflow_cm) / mean(abs(real_inflow_cm)) > eps
                  )

  skip("Inflow can actually be != 0 if Evap_mismatch != 0") # TODO
  expect_equal(nrow(res), 0)
})

test_that("If ideal_outflow_drain > 0 the cluster must be draining", {
  res <- test_df |>
    dplyr::filter(ideal_outflow_drain > 0, !draining)

  expect_equal(nrow(res), 0)
})




test_that("outflows (real and ideal) are always non-negative", {
  res <- test_df |>
    dplyr::filter(
      ideal_outflow_drain < 0 | real_outflow_drain < 0 |
      ideal_outflow_flux < 0  | real_outflow_flux < 0  |
      ideal_outflow_rain < 0  | real_outflow_rain < 0
      )

  expect_equal(nrow(res), 0)
})

test_that("real outflows never exceed their ideal counterparts", {
  res <- test_df |>
    dplyr::filter(
      real_outflow_rain > ideal_outflow_rain + eps |
      real_outflow_drain > ideal_outflow_drain + eps |
      real_outflow_flux > ideal_outflow_flux + eps
      )

  expect_equal(nrow(res), 0)
})

test_that("sum(real outflows) = min(sum(ideal outflows), total capacity)", {
  res <- test_df |>
    dplyr::group_by(date, ditch) |>
    dplyr::summarise(
      flowpoint = flowpoint[1],
      real_outflow = sum(real_outflow_m3_s),
      ideal_outflow = sum(ideal_outflow_m3_s),
      .groups = "drop"
    ) |>
    dplyr::filter(
      abs(real_outflow - pmin(ideal_outflow, flowpoint)) >
        mean(abs(flowpoint)) * eps
      )

  expect_equal(nrow(res), 0)
})



test_that("If accum_drain is greater than zero the cluster must be draining", {
  res <- test_df |>
    dplyr::filter(accum_drain > 0, !draining)

  expect_equal(nrow(res), 0)
})

