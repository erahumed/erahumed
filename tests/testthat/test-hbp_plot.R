hbp_obj <- albufera_outflows |>
  dplyr::filter("2010-01-01" <= date, date <= "2010-01-10") |>
  inp() |>
  hba() |>
  hbp()

test_that("plot.hbp() succeeds w/ type='cluster_levels' and valid input", {
  expect_no_error(
    plot(hbp_obj, type = "cluster_levels", cluster_id = hbp_obj$cluster_id[1])
  )
})

test_that("plot.hbp() error w/ type='map'", {
  expect_error( plot(hbp_obj, type = "map") )
})

test_that("plot.hbp throws an error if no cluster is specified", {
  expect_error( plot(hbp_obj, type = "cluster_levels") )  # No cluster_id passed
})

