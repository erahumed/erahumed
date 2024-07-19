test_that("global hydrological balance checks out", {
  tol <- 1e-12

  df <- albufera_hydro_balance_global()

  res <- df |>
    dplyr::mutate(
      x = volume_change,
      y = (inflow_total - outflow_total) * s_per_day() + petp_change
    ) |>
    dplyr::filter(abs(x - y) / mean(abs(x)) > tol)

  expect_equal(nrow(res), 0)
  })

test_that("unaccounted outflow is only possible in cases of zero inflow", {
  tol <- 1e-12

  df <- albufera_hydro_balance_global()

  res <- df |>
    dplyr::filter(abs(inflow_total) > tol * mean(abs(inflow_total)),
                  abs(outflow_extra) > tol * mean(abs(outflow_total))
                  )

  expect_equal(nrow(res), 0)
})
