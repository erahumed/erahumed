test_that("plot.lhb does not produce an error with valid inputs", {
  df <- albufera_lhb()

  expect_no_error(plot(df, "residence_time_days"))
})

test_that("plot.lhb raises an error with invalid inputs", {
  df <- albufera_lhb()
  expect_error(plot(df, "invalid_variable"), class = "plot.lhb_error")
  expect_error(plot(df, variable = 840), class = "plot.lhb_error")
})

test_that("plot.lhb raises a warning with unused arguments", {
  df <- albufera_lhb()
  expect_warning(plot(df,
                      variable = "residence_time_days",
                      unused_argument = "argument"),
                 class = "plot.lhb_warning"
                 )
})

test_that("plot.hb_local() succeeds w/ type='cluster_levels' and valid input", {
  df <- albufera_hb_local(date_min = "2010-01-01", date_max = "2010-01-10")
  expect_no_error(
    plot(df, type = "cluster_levels", cluster_id = df$cluster_id[1])
    )
})

test_that("plot.hb_local() error w/ type='map'", {
  df <- albufera_hb_local(date_min = "2010-01-01", date_max = "2010-01-10")
  expect_error( plot(df, type = "map") )
})

test_that("plot.hb_local throws an error if no cluster is specified", {
  df <- albufera_hb_local(date_min = "2010-01-01", date_max = "2010-01-10")
  expect_error( plot(df, type = "cluster_levels") )  # No cluster_id passed
})

