test_that("albufera_hb_local() does not raise an error with valid inputs", {
  expect_no_error(
    albufera_hb_local(date_min = "2020-01-01", date_max = "2020-01-10")
    )
})

withr::with_envvar(c(erahumed_randomize_clusters = FALSE), {
  date_min <- as.Date("2010-01-01")
  date_max <- as.Date("2011-12-31")

  outflows_df <- albufera_outflows
  petp_df <- albufera_petp
  clusters_df <- albufera_clusters
  management_df <- albufera_management

  test_df <- albufera_hb_local(outflows_df = outflows_df,
                               petp_df = petp_df,
                               clusters_df = clusters_df,
                               management_df = management_df,
                               date_min = date_min,
                               date_max = date_max)

  eps <- 1e-10
  })


# Property based tests

test_that("Returned dataset has the expected number of rows", {
  n_clusters <- nrow(clusters_df)
  n_days <- length( seq.Date(from = date_min, to = date_max, by = "day") )

  expect_equal(nrow(test_df), n_clusters * n_days)
})

test_that("'ditch' is consistent along 'cluster_id'", {
  res <- test_df |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(distinct_values = dplyr::n_distinct(ditch)) |>
    dplyr::filter(distinct_values > 1)

  expect_equal(nrow(res), 0)
})

test_that("'tancat' is consistent along 'cluster_id'", {
  res <- test_df |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(distinct_values = dplyr::n_distinct(tancat)) |>
    dplyr::filter(distinct_values > 1)

  expect_equal(nrow(res), 0)
})

test_that("'variety' is consistent along 'cluster_id'", {
  res <- test_df |>
    dplyr::group_by(cluster_id) |>
    dplyr::summarise(distinct_values = dplyr::n_distinct(variety)) |>
    dplyr::filter(distinct_values > 1)

  expect_equal(nrow(res), 0)
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

test_that("plan_delay is an integer", {
  res <- test_df |>
    dplyr::filter(abs(plan_delay - as.integer(plan_delay)) > 0.1)
  expect_equal(nrow(res), 0)
})

test_that("plan_delay is positive", {
  res <- test_df |>
    dplyr::filter(plan_delay < 0)
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

test_that("real_irrigation is the delayed version of ideal_irrigation", {
  res <- test_df |>
    dplyr::group_by(cluster_id) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      # The correct delay for real_irrigation is that from the previous day!
      delay = dplyr::coalesce(dplyr::lag(plan_delay), 0),
      real_irrigation_v2 = ideal_irrigation[1:length(delay) - delay]
      ) |>
    dplyr::filter(real_irrigation != real_irrigation_v2)

  expect_equal(nrow(res), 0)
})

test_that("real_draining is the delayed version of ideal_draining", {
  res <- test_df |>
    dplyr::group_by(cluster_id) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      # The correct delay for real_irrigation is that from the previous day!
      delay = dplyr::coalesce(dplyr::lag(plan_delay), 0),
      real_draining_v2 = ideal_draining[1:length(delay) - delay]
      ) |>
    dplyr::filter(real_draining != real_draining_v2)

  expect_equal(nrow(res), 0)
})


# Exceptions

test_that("albufera_hb_local() error if invalid date range", {
  # Empty (no data)
  expect_error(
    albufera_hb_local(date_min = "1800-01-01", date_max = "1800-12-31"),
    regexp = "date_min"
    )

  # Invalid
  expect_error(
    albufera_hb_local(date_min = "2010-01-01", date_max = "2009-01-01"),
    class = "albufera_hb_local_argcheck_error"
  )

  expect_error(
    albufera_hb_local(date_min = "A", date_max = "2009-01-01"),
    class = "albufera_hb_local_argcheck_error"
  )

  expect_error(
    albufera_hb_local(date_min = 1 + i1, date_max = "2009-01-01"),
    class = "albufera_hb_local_argcheck_error"
  )
})

test_that("albufera_hb_local() error if invalid ideal_flow_rate_cm", {
  expect_error(
    albufera_hb_local(ideal_flow_rate_cm = -1),
    class = "albufera_hb_local_argcheck_error"
  )

  expect_error(
    albufera_hb_local(ideal_flow_rate_cm = "one"),
    class = "albufera_hb_local_argcheck_error"
  )

  expect_error(
    albufera_hb_local(ideal_flow_rate_cm = NA),
    class = "albufera_hb_local_argcheck_error"
  )

  expect_error(
    albufera_hb_local(ideal_flow_rate_cm = Inf),
    class = "albufera_hb_local_argcheck_error"
  )

  expect_error(
    albufera_hb_local(ideal_flow_rate_cm = NaN),
    class = "albufera_hb_local_argcheck_error"
  )


})

test_that("albufera_hb_local() error if invalid date frame inputs", {
  # remove one required column
  expect_error(
    albufera_hb_local(outflows_df = erahumed::albufera_outflows[,-1]),
    class = "albufera_hb_local_argcheck_error"
  )

  expect_error(
    albufera_hb_local(petp_df = erahumed::albufera_petp[,-1]),
    class = "albufera_hb_local_argcheck_error"
  )

  expect_error(
    albufera_hb_local(management_df = erahumed::albufera_management[,-1]),
    class = "albufera_hb_local_argcheck_error"
  )

  expect_error(
    albufera_hb_local(clusters_df = erahumed::albufera_clusters[,-1]),
    class = "albufera_hb_local_argcheck_error"
  )

})


# Precomputed results

test_that("albufera_hb_local_precomputed() does not return NULL normally", {
  formals <- formals(albufera_hb_local)
  call <- substitute(albufera_hb_local())

  expect_s3_class(albufera_hb_local_precomputed(formals, call), "hb_local")
})

test_that("albufera_hb_local_precomputed() is NULL if envvar set to FALSE", {
  formals <- formals(albufera_hb_local)
  call <- substitute(albufera_hb_local())
  withr::with_envvar(c(erahumed_use_precomputed = FALSE),
                     expect_null(albufera_hb_local_precomputed(formals, call))
                     )

})

test_that("Parquet precomputed file coincides with would-be default value", {
  skip_if_not(is_checking())

  expected <- albufera_hb_local()
  actual <- withr::with_seed(840,
            withr::with_envvar(c(erahumed_use_precomputed = FALSE),
              albufera_hb_local()
            ))

  expect_identical(actual, expected)

  expect_true(all(
    sapply(colnames(expected), \(c) all.equal(actual[[c]], expected[[c]]))
  ))
})




# Snapshot test, just to monitor unexpected changes. This is simply supposed to
# be an alert whenever the output of 'albufera_hb_local()' changes without
# apparent reason. We should not be too strict about this, and be eager to
# silence the warning (update the snapshot) whenever the lower level tests
# succeed.

test_that("simple snapshot is constant", {
  skip_on_ci()  # Gives inconsistent result across different platforms

  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})

