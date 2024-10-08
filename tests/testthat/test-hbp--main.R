test_that("compute_hbp() does not raise an error with valid inputs", {
  expect_no_error( compute_hbp(test_mod_small()) )
})

withr::with_envvar(c(erahumed_randomize_clusters = FALSE), {
  test_df <- hbp(test_mod_large())$output
  eps <- 1e-10
  })


# Property based tests

test_that("Returned dataset has the expected number of rows", {
  n_clusters <- nrow(albufera_clusters)
  n_days <- length( seq.Date(from = min(test_df$date),
                             to = max(test_df$date),
                             by = "day")
                    )

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


test_that("compute_hbp() error if invalid ideal_flow_rate_cm", {
  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = -1),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = "one"),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = NA),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = Inf),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = NaN),
    class = "compute_hbp_argcheck_error"
  )


})

test_that("compute_hbp() error if invalid hba_res", {
  skip("Adapt to new implementation")

  # remove one required column
  expect_error(
    hbp(hba_res = "not a valid hba_res"),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    hbp(hba_res = inp() |> hba(),
        management_df = erahumed::albufera_management[,-1]),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    hbp(hba_res = inp() |> hba(),
        clusters_df = erahumed::albufera_clusters[,-1]),
    class = "compute_hbp_argcheck_error"
  )

})


# Precomputed results

test_that("hbp_precomputed() does not return NULL normally", {
  skip("To adapt to new implementation")
  formals <- formals(hbp)
  call <- substitute(hbp())

  expect_s3_class(hbp_precomputed(formals, call), "erahumed_hbp")
})

test_that("hbp_precomputed() is NULL if envvar set to FALSE", {
  skip("To adapt to new implementation")
  formals <- formals(hbp)
  call <- substitute(hbp())
  withr::with_envvar(c(erahumed_use_precomputed = FALSE),
                     expect_null(hbp_precomputed(formals, call))
                     )

})

test_that("Parquet precomputed file coincides with would-be default value", {
  skip("To adapt to new implementation")
  skip_if_not(is_checking())

  expected <- hbp()
  actual <- withr::with_seed(840,
            withr::with_envvar(c(erahumed_use_precomputed = FALSE),
              hbp()
            ))

  expect_identical(actual, expected)

  expect_true(all(
    sapply(colnames(expected), \(c) all.equal(actual[[c]], expected[[c]]))
  ))
})




# Snapshot test, just to monitor unexpected changes. This is simply supposed to
# be an alert whenever the output of 'hbp()' changes without
# apparent reason. We should not be too strict about this, and be eager to
# silence the warning (update the snapshot) whenever the lower level tests
# succeed.

test_that("simple snapshot is constant", {
  skip_on_ci()  # Gives inconsistent result across different platforms

  hash <- digest::digest(test_df)

  expect_snapshot(hash)
})

