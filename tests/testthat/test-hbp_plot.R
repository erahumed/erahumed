test_that("plot.hbp() succeeds w/ type='cluster_levels' and valid input", {
  df <- albufera_hbp(date_min = "2010-01-01", date_max = "2010-01-10")
  expect_no_error(
    plot(df, type = "cluster_levels", cluster_id = df$cluster_id[1])
  )
})

test_that("plot.hbp() error w/ type='map'", {
  df <- albufera_hbp(date_min = "2010-01-01", date_max = "2010-01-10")
  expect_error( plot(df, type = "map") )
})

test_that("plot.hbp throws an error if no cluster is specified", {
  df <- albufera_hbp(date_min = "2010-01-01", date_max = "2010-01-10")
  expect_error( plot(df, type = "cluster_levels") )  # No cluster_id passed
})

