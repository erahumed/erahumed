test_that("plot.hb_global does not produce an error with valid inputs", {
  df <- albufera_hb_global()

  expect_no_error(plot(df, "residence_time_days"))
})

test_that("plot.hb_local() succeeds w/ type='cluster_levels' and valid input", {
  df <- albufera_hydro_balance_local(
    date_min = "2010-01-01", date_max = "2010-01-10")

  expect_no_error(
    plot(df, type = "cluster_levels", cluster_id = df$cluster_id[1])
    )
})

test_that("plot.hb_local() error w/ type='map'", {
  df <- albufera_hydro_balance_local(
    date_min = "2010-01-01", date_max = "2010-01-10")

  expect_error( plot(df, type = "map") )
})

test_that("plot.hb_local throws an error if no cluster is specified", {
  df <- albufera_hydro_balance_local(
    date_min = "2010-01-01", date_max = "2010-01-10")

  expect_error( plot(df, type = "cluster_levels") )  # No cluster_id passed
})

