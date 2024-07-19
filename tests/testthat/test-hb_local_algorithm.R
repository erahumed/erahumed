test_that("sum of cluster outflows does never exceed total ditch outflow", {
  withr::local_envvar(erahumed_hb_sort_clusters = TRUE)

  tol <- 1e-10

  test <- albufera_hydro_balance_local(date_min = "2010-01-01",
                                       date_max = "2010-02-01")

  res <- test |>
    dplyr::group_by(date, ditch) |>
    dplyr::summarise(
      flowpoint = flowpoint[1], outflow = sum(outflow_m3), .groups = "drop"
      ) |>
    dplyr::filter(outflow > flowpoint + tol)

  expect_equal(nrow(res), 0)
  })

test_that("drain and flux (real) outflows are never negative", {
  withr::local_envvar(erahumed_hb_sort_clusters = TRUE)
  tol <- 1e-10

  test <- albufera_hydro_balance_local(date_min = "2010-01-01",
                                       date_max = "2010-02-01")

  res <- test |>
    dplyr::filter(real_outflow_drain < 0 | real_outflow_flux < 0)

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
