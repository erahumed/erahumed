test_that("plot.hb_global does not produce an error with valid inputs", {
  df <- albufera_hydro_balance_global()

  expect_no_error(plot(df, "residence_time_days"))
})

test_that("plot.hb_local does not produce an error with valid inputs", {
  df <- albufera_hydro_balance_local(
    date_min = "2010-01-01", date_max = "2010-01-10")

  expect_no_error(
    plot(df, type = "cluster_levels", cluster_id = df$cluster_id[1])
    )
})
