test_that("sum of cluster outflows does never exceed total ditch outflow", {
  withr::local_envvar(erahumed_hb_sort_clusters = TRUE)

  tol <- 1e-10

  test <- albufera_hydro_balance_local(date_min = "2010-01-01",
                                       date_max = "2010-02-01")

  res <- test |>
    dplyr::group_by(date, ditch) |>
    dplyr::summarise(
      flowpoint = flowpoint[1], outflow = sum(real_outflow_m3_s), .groups = "drop"
      ) |>
    dplyr::filter(outflow > flowpoint + tol)

  expect_equal(nrow(res), 0)
  })

test_that("real outflows never exceed ideal ones", {
  withr::local_envvar(erahumed_hb_sort_clusters = TRUE)
  tol <- 1e-10

  test <- albufera_hydro_balance_local(date_min = "2010-01-01",
                                       date_max = "2010-02-01")

  res <- test |>
    dplyr::filter(
      real_outflow_rain > ideal_outflow_rain + tol |
      real_outflow_drain > ideal_outflow_drain + tol |
      real_outflow_flux > ideal_outflow_flux + tol
      )

  expect_equal(nrow(res), 0)
})

test_that("drain and flux (real) outflows are always non-negative", {
  withr::local_envvar(erahumed_hb_sort_clusters = TRUE)
  tol <- 1e-10

  test <- albufera_hydro_balance_local(date_min = "2010-01-01",
                                       date_max = "2010-02-01")

  res <- test |>
    dplyr::filter(real_outflow_drain < 0 | real_outflow_flux < 0)

  expect_equal(nrow(res), 0)
})

test_that("rain outflow is always non-negative", {
  withr::local_envvar(erahumed_hb_sort_clusters = TRUE)
  tol <- 1e-10

  test <- albufera_hydro_balance_local(date_min = "2010-01-01",
                                       date_max = "2010-02-01")

  res <- test |>
    dplyr::filter(real_outflow_rain < 0)

  skip("Rain outflow can actually become negative (with small values)") # TODO
  expect_equal(nrow(res), 0)
})

test_that("inflow is zero if not irrigating", {
  withr::local_envvar(erahumed_hb_sort_clusters = TRUE)
  tol <- 1e-10

  test <- albufera_hydro_balance_local(date_min = "2010-01-01",
                                       date_max = "2010-02-01")

  res <- test |>
    dplyr::filter(!irrigation, abs(inflow) / mean(abs(inflow)) > tol)

  expect_equal(nrow(res), 0)
})

test_that("simple snapshot is constant", {
  withr::local_envvar(erahumed_hb_sort_clusters = TRUE)

  tol <- 1e-10

  test <- albufera_hydro_balance_local(date_min = "2010-01-01",
                                       date_max = "2010-02-01")

  hash <- digest::digest(test)

  expect_snapshot(hash)
})
